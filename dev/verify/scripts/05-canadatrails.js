// Step 5: read the canadatrails pages.
// Phase A: fetch each page as-is (http) into dev/verify/cache/canadatrails/,
//          1.5 s apart; on failure use the 2020 Wayback copy, 3 s apart.
// Phase B: parse cached pages for the Website link and last-reviewed date.
// Phase C: run checkUrl on every non-null websiteLink.
// Resumable at every phase.
//
// Usage: node dev/verify/scripts/05-canadatrails.js
const fs = require('fs');
const path = require('path');
const { checkMany, fetchPage, saveJson, loadJson, sleep, decodeEntities } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const AREAS = path.join(ROOT, 'dev/verify/work/areas.json');
const OUT = path.join(ROOT, 'dev/verify/work/canadatrails.json');
const LOG = path.join(ROOT, 'dev/verify/work/canadatrails-fetchlog.json');
const CACHE = path.join(ROOT, 'dev/verify/cache/canadatrails');

const MONTHS = { january: 1, february: 2, march: 3, april: 4, may: 5, june: 6, july: 7, august: 8, september: 9, october: 10, november: 11, december: 12 };
const pad = n => String(n).padStart(2, '0');

function parseWebsite(html) {
  // Find the label, then the first <a href> after it inside the same <p>.
  const m = html.match(/<b>\s*Web\s?site:\s*<\/b>/i);
  if (!m) return null;
  const after = html.slice(m.index + m[0].length);
  const pEnd = after.search(/<\/p>|<p[\s>]/i);
  const inP = pEnd >= 0 ? after.slice(0, pEnd) : after;
  const a = inP.match(/<a\b[^>]*\bhref\s*=\s*"([^"]*)"/i) || inP.match(/<a\b[^>]*\bhref\s*=\s*'([^']*)'/i);
  if (!a) return null;
  let href = decodeEntities(a[1]).trim();
  // Wayback copies rewrite links to /web/<ts>/<original>; recover the original.
  const wb = href.match(/^(?:https?:\/\/web\.archive\.org)?\/web\/\d+[a-z_]*\/(https?:\/\/.+)$/i);
  if (wb) href = wb[1];
  return href || null;
}

function parseLastReviewed(html) {
  const p = html.match(/<p[^>]*id="lastRevDate"[^>]*>([\s\S]*?)<\/p>/i);
  if (!p) return null;
  const text = decodeEntities(p[1].replace(/<[^>]+>/g, ' ')).replace(/\s+/g, ' ');
  const m = text.match(/last reviewed and updated:\s*([A-Za-z]+)\s+(\d{1,2}),?\s+(\d{4})/i);
  if (m && MONTHS[m[1].toLowerCase()]) return `${m[3]}-${pad(MONTHS[m[1].toLowerCase()])}-${pad(+m[2])}`;
  const raw = text.match(/last reviewed and updated:\s*(.+)$/i);
  return raw ? raw[1].trim() : null;
}

(async () => {
  fs.mkdirSync(CACHE, { recursive: true });
  const areas = loadJson(AREAS, []).filter(a => a.group === 'canadatrails');
  const results = loadJson(OUT, {});
  const log = loadJson(LOG, {});

  // Phase A: live fetch
  let n = 0;
  for (const a of areas) {
    const file = path.join(CACHE, `${a.id}.html`);
    if (fs.existsSync(file) || (log[a.id] && log[a.id].liveTried)) { n++; continue; }
    const r = await fetchPage(a.url);
    if (r.ok && r.status === 200) {
      fs.writeFileSync(file, r.html);
      log[a.id] = { liveTried: true, fromArchive: false };
    } else {
      log[a.id] = { liveTried: true, liveStatus: r.status || null, liveError: r.error || null };
    }
    saveJson(LOG, log);
    if (++n % 50 === 0) console.log(`  live ${n}/${areas.length}`);
    await sleep(1500);
  }

  // Phase A2: archive fallback
  const needArchive = areas.filter(a => !fs.existsSync(path.join(CACHE, `${a.id}.html`)) && !(log[a.id] && log[a.id].archiveTried));
  console.log(`archive fallback: ${needArchive.length}`);
  for (const a of needArchive) {
    const file = path.join(CACHE, `${a.id}.html`);
    const r = await fetchPage(`http://web.archive.org/web/2020id_/${a.url}`);
    log[a.id] = { ...(log[a.id] || {}), archiveTried: true, archiveStatus: r.status || null, archiveError: r.error || null };
    if (r.ok && r.status === 200) { fs.writeFileSync(file, r.html); log[a.id].fromArchive = true; }
    saveJson(LOG, log);
    await sleep(3000);
  }

  // Phase B: parse
  for (const a of areas) {
    const file = path.join(CACHE, `${a.id}.html`);
    const prev = results[a.id] || {};
    const html = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null;
    const rec = {
      websiteLink: html ? parseWebsite(html) : null,
      lastReviewed: html ? parseLastReviewed(html) : null,
      fromArchive: !!(log[a.id] && log[a.id].fromArchive),
    };
    if (!html) rec.pageFetchFailed = true;
    if (prev.linkCheck && prev.websiteLink === rec.websiteLink) rec.linkCheck = prev.linkCheck;
    results[a.id] = rec;
  }
  saveJson(OUT, results);

  // Phase C: link check
  const todo = areas.filter(a => results[a.id].websiteLink && !results[a.id].linkCheck);
  console.log(`link check: ${todo.length} to check`);
  let c = 0;
  await checkMany(todo.map(a => ({ key: a.id, url: results[a.id].websiteLink })), (key, res) => {
    results[key].linkCheck = res;
    saveJson(OUT, results);
    if (++c % 50 === 0) console.log(`  checked ${c}/${todo.length}`);
  });

  const vals = areas.map(a => results[a.id]);
  const states = {};
  for (const v of vals) if (v.websiteLink) { const s = v.linkCheck ? v.linkCheck.state : 'missing'; states[s] = (states[s] || 0) + 1; }
  const years = {};
  for (const v of vals) { const y = v.lastReviewed && /^\d{4}/.test(v.lastReviewed) ? v.lastReviewed.slice(0, 4) : 'none'; years[y] = (years[y] || 0) + 1; }
  console.log('Entries:', vals.length);
  console.log('Page not fetched (live or archive):', vals.filter(v => v.pageFetchFailed).length);
  console.log('From archive:', vals.filter(v => v.fromArchive).length);
  console.log('With website link:', vals.filter(v => v.websiteLink).length);
  console.log('Last-reviewed year:', years);
  console.log('Link states:', states);
})();
