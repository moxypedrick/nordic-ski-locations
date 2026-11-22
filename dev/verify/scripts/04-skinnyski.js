// Step 4: read the skinnyski trail pages.
// Phase A: fetch each trail page into dev/verify/cache/skinnyski/ (resumable,
//          one request at a time, 1.5 s apart).
// Phase B: parse the cached pages.
// Phase C: run checkUrl on every non-null officialLink (resumable).
//
// Deviation from the plan, pending David's decision at STOP 1:
// the previous-season pages (traildetail.asp?Id=N&season=2025) return a
// Cloudflare bot challenge (HTTP 403 "Just a moment...") to scripts, every
// time. We do not try to get past it. Set FETCH_LAST_SEASON=1 to attempt
// those pages anyway (each one is tried once; a challenge is recorded as
// lastSeasonBlocked: true and reportsLastSeason stays null).
//
// Usage: node dev/verify/scripts/04-skinnyski.js
const fs = require('fs');
const path = require('path');
const { checkMany, fetchPage, saveJson, loadJson, sleep, decodeEntities } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const AREAS = path.join(ROOT, 'dev/verify/work/areas.json');
const OUT = path.join(ROOT, 'dev/verify/work/skinnyski.json');
const CACHE = path.join(ROOT, 'dev/verify/cache/skinnyski');
const FETCH_LAST_SEASON = process.env.FETCH_LAST_SEASON === '1';
const DELAY_MS = 1500;

function isChallenge(html) { return /<title>Just a moment\.\.\.<\/title>/i.test(html || ''); }

function idFromUrl(url) {
  const m = String(url).match(/[?&]Id=(\d+)/i);
  return m ? Number(m[1]) : null;
}

async function fetchToCache(url, file) {
  if (fs.existsSync(file)) return { cached: true };
  const r = await fetchPage(url);
  if (r.ok && r.status === 200 && !isChallenge(r.html)) {
    fs.writeFileSync(file, r.html);
    return { cached: false, ok: true };
  }
  return { cached: false, ok: false, status: r.status || null, challenge: isChallenge(r.html), error: r.error };
}

function parseOfficialLink(html) {
  const m = html.match(/<div class="ss-traildetail-name">([\s\S]*?)<\/div>/i);
  if (!m) return { listingMissing: true, officialLink: null };
  const inner = m[1];
  const name = decodeEntities(inner.replace(/<[^>]+>/g, '')).trim();
  const a = inner.match(/<a\b[^>]*\bhref\s*=\s*"([^"]*)"/i) || inner.match(/<a\b[^>]*\bhref\s*=\s*'([^']*)'/i);
  const href = a ? decodeEntities(a[1]).trim() : '';
  return { listingMissing: name === '', officialLink: href && /^https?:\/\//i.test(href) ? href : (href ? href : null) };
}

// Report section: everything after the "Trail Reports" heading.
function reportSection(html) {
  const i = html.search(/<h3>\s*Trail Reports\s*<\/h3>/i);
  if (i < 0) return '';
  const rest = html.slice(i);
  const j = rest.search(/id="ss-site-footer"/i);
  return j > 0 ? rest.slice(0, j) : rest;
}

const MONTHS = { jan: 1, feb: 2, mar: 3, apr: 4, may: 5, jun: 6, jul: 7, aug: 8, sep: 9, oct: 10, nov: 11, dec: 12 };
function pad(n) { return String(n).padStart(2, '0'); }

// Collect dates found in the report section. Handles M/D/YYYY and
// "Month D, YYYY" forms; the format was confirmed against sample pages
// (see the note printed at the end of the run).
function reportDates(section) {
  const text = decodeEntities(section.replace(/<[^>]+>/g, ' ')).replace(/\s+/g, ' ');
  const dates = [];
  for (const m of text.matchAll(/\b(\d{1,2})\/(\d{1,2})\/(\d{4})\b/g)) {
    const mo = +m[1], d = +m[2], y = +m[3];
    if (mo >= 1 && mo <= 12 && d >= 1 && d <= 31 && y >= 2000) dates.push(`${y}-${pad(mo)}-${pad(d)}`);
  }
  for (const m of text.matchAll(/\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\.?\s+(\d{1,2}),?\s+(\d{4})\b/gi)) {
    const mo = MONTHS[m[1].toLowerCase().slice(0, 3)], d = +m[2], y = +m[3];
    if (d >= 1 && d <= 31 && y >= 2000) dates.push(`${y}-${pad(mo)}-${pad(d)}`);
  }
  return dates.sort();
}

function parseReports(html) {
  if (!html) return { hasReports: null, latest: null };
  // Removed listings render an empty shell with no "Trail Reports" section
  // and no "No reports found." text. Treat those as unknown, not as reports.
  if (!reportSection(html)) return { hasReports: null, latest: null };
  const has = !html.includes('No reports found.');
  const dates = has ? reportDates(reportSection(html)) : [];
  return { hasReports: has, latest: dates.length ? dates[dates.length - 1] : null };
}

(async () => {
  fs.mkdirSync(CACHE, { recursive: true });
  const areas = loadJson(AREAS, []).filter(a => a.group === 'skinnyski');
  const results = loadJson(OUT, {});
  const fetchLog = loadJson(path.join(ROOT, 'dev/verify/work/skinnyski-fetchlog.json'), {});

  // Phase A: fetch
  let n = 0;
  for (const a of areas) {
    const sid = idFromUrl(a.url);
    if (sid == null) { fetchLog[a.id] = { error: 'no Id in URL' }; continue; }
    const f1 = path.join(CACHE, `${sid}.html`);
    if (!fs.existsSync(f1) && !(fetchLog[a.id] && fetchLog[a.id].mainFailed)) {
      const r = await fetchToCache(`https://www.skinnyski.com/trails/traildetail.asp?Id=${sid}`, f1);
      if (!r.ok) fetchLog[a.id] = { ...(fetchLog[a.id] || {}), mainFailed: true, mainStatus: r.status, mainChallenge: r.challenge, mainError: r.error };
      saveJson(path.join(ROOT, 'dev/verify/work/skinnyski-fetchlog.json'), fetchLog);
      await sleep(DELAY_MS);
    }
    if (FETCH_LAST_SEASON) {
      const f2 = path.join(CACHE, `${sid}-2025.html`);
      if (!fs.existsSync(f2) && !(fetchLog[a.id] && fetchLog[a.id].lastSeasonTried)) {
        const r = await fetchToCache(`https://www.skinnyski.com/trails/traildetail.asp?Id=${sid}&season=2025`, f2);
        fetchLog[a.id] = { ...(fetchLog[a.id] || {}), lastSeasonTried: true, lastSeasonBlocked: !r.ok && !!r.challenge, lastSeasonStatus: r.status };
        saveJson(path.join(ROOT, 'dev/verify/work/skinnyski-fetchlog.json'), fetchLog);
        await sleep(DELAY_MS);
      }
    }
    if (++n % 50 === 0) console.log(`  fetched ${n}/${areas.length}`);
  }

  // Phase B: parse
  for (const a of areas) {
    const sid = idFromUrl(a.url);
    const prev = results[a.id] || {};
    const f1 = path.join(CACHE, `${sid}.html`);
    const f2 = path.join(CACHE, `${sid}-2025.html`);
    const h1 = fs.existsSync(f1) ? fs.readFileSync(f1, 'utf8') : null;
    const h2 = fs.existsSync(f2) ? fs.readFileSync(f2, 'utf8') : null;
    const link = h1 ? parseOfficialLink(h1) : { listingMissing: null, officialLink: null };
    const r1 = parseReports(h1);
    const r2 = parseReports(h2);
    const latest = [r1.latest, r2.latest].filter(Boolean).sort().pop() || null;
    const log = fetchLog[a.id] || {};
    const rec = {
      skinnyskiId: sid,
      officialLink: link.officialLink,
      listingMissing: link.listingMissing,
      reportsThisSeason: r1.hasReports,
      reportsLastSeason: r2.hasReports,
      latestReportDate: latest,
    };
    if (!h1) rec.pageFetchFailed = true;
    if (log.lastSeasonBlocked || !h2) rec.lastSeasonUnavailable = true;
    // keep an existing link check if the link did not change
    if (prev.linkCheck && prev.officialLink === rec.officialLink) rec.linkCheck = prev.linkCheck;
    results[a.id] = rec;
  }
  saveJson(OUT, results);

  // Phase C: link check
  const todo = areas.filter(a => results[a.id].officialLink && !results[a.id].linkCheck);
  console.log(`link check: ${todo.length} to check`);
  let c = 0;
  await checkMany(todo.map(a => ({ key: a.id, url: results[a.id].officialLink })), (key, res) => {
    results[key].linkCheck = res;
    saveJson(OUT, results);
    if (++c % 50 === 0) console.log(`  checked ${c}/${todo.length}`);
  });

  // Summary
  const vals = areas.map(a => results[a.id]);
  const states = {};
  for (const v of vals) if (v.officialLink) { const s = v.linkCheck ? v.linkCheck.state : 'missing'; states[s] = (states[s] || 0) + 1; }
  console.log('Entries:', Object.keys(results).filter(k => areas.some(a => a.id === k)).length);
  console.log('Page fetch failed:', vals.filter(v => v.pageFetchFailed).length);
  console.log('Listing missing:', vals.filter(v => v.listingMissing).length);
  console.log('With official link:', vals.filter(v => v.officialLink).length);
  console.log('Reports this season (2026-27):', vals.filter(v => v.reportsThisSeason).length);
  console.log('Reports last season (2025-26):', vals.filter(v => v.reportsLastSeason).length, '| last-season page unavailable:', vals.filter(v => v.lastSeasonUnavailable).length);
  console.log('Reports in either season:', vals.filter(v => v.reportsThisSeason || v.reportsLastSeason).length);
  console.log('With a latest report date:', vals.filter(v => v.latestReportDate).length);
  console.log('Link states:', states);
})();
