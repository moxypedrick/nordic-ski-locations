// Step 6: read the paccsa pages.
// Fetch each page (2 s apart). On 403 record blocked: true and move on (no
// retry with other headers). Collect external links from the main content
// and run checkUrl on each. Resumable.
//
// Usage: node dev/verify/scripts/06-paccsa.js
const fs = require('fs');
const path = require('path');
const { checkUrl, fetchPage, saveJson, loadJson, sleep, decodeEntities, hostOf } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const AREAS = path.join(ROOT, 'dev/verify/work/areas.json');
const OUT = path.join(ROOT, 'dev/verify/work/paccsa.json');
const CACHE = path.join(ROOT, 'dev/verify/cache/paccsa');

const EXCLUDE = [
  /(^|\.)paccsa\.org$/, /(^|\.)facebook\.com$/, /(^|\.)fb\.com$/, /(^|\.)twitter\.com$/, /(^|\.)x\.com$/,
  /(^|\.)instagram\.com$/, /(^|\.)youtube\.com$/, /(^|\.)youtu\.be$/, /(^|\.)google\.[a-z.]+$/, /(^|\.)goo\.gl$/,
  // weather sites
  /(^|\.)weather\.gov$/, /(^|\.)weather\.com$/, /(^|\.)accuweather\.com$/, /(^|\.)wunderground\.com$/,
  /(^|\.)noaa\.gov$/, /(^|\.)weatherbug\.com$/, /(^|\.)opensnow\.com$/, /(^|\.)windy\.com$/,
];

function mainContent(html) {
  const i = html.search(/<div class="region region-content">/i);
  if (i < 0) return '';
  const rest = html.slice(i);
  const j = rest.search(/<\/article>/i);
  return j > 0 ? rest.slice(0, j) : rest;
}

function candidateLinks(html) {
  const out = [];
  for (const m of mainContent(html).matchAll(/<a\b[^>]*\bhref\s*=\s*["']([^"']+)["']/gi)) {
    const href = decodeEntities(m[1]).trim();
    if (!/^https?:\/\//i.test(href)) continue;
    const host = hostOf(href);
    if (!host || EXCLUDE.some(r => r.test(host))) continue;
    if (!out.includes(href)) out.push(href);
  }
  return out;
}

(async () => {
  fs.mkdirSync(CACHE, { recursive: true });
  const areas = loadJson(AREAS, []).filter(a => a.group === 'paccsa');
  const results = loadJson(OUT, {});

  for (const a of areas) {
    if (results[a.id] && results[a.id].done) continue;
    const file = path.join(CACHE, `${a.id}.html`);
    let html = fs.existsSync(file) ? fs.readFileSync(file, 'utf8') : null;
    let rec = { blocked: false, httpStatus: 200, candidateLinks: [] };
    if (!html) {
      const r = await fetchPage(a.url);
      await sleep(2000);
      rec.httpStatus = r.status || null;
      if (r.ok && r.status === 403) { rec.blocked = true; rec.done = true; results[a.id] = rec; saveJson(OUT, results); continue; }
      if (!r.ok || r.status !== 200) { rec.fetchFailed = true; rec.error = r.error || null; rec.done = true; results[a.id] = rec; saveJson(OUT, results); continue; }
      html = r.html;
      fs.writeFileSync(file, html);
    }
    const links = candidateLinks(html);
    rec.candidateLinks = [];
    for (const l of links) rec.candidateLinks.push({ url: l, linkCheck: await checkUrl(l) });
    rec.done = true;
    results[a.id] = rec;
    saveJson(OUT, results);
  }

  const vals = areas.map(a => results[a.id]);
  const states = {};
  for (const v of vals) for (const c of v.candidateLinks || []) states[c.linkCheck.state] = (states[c.linkCheck.state] || 0) + 1;
  console.log('Entries:', vals.length);
  console.log('Blocked (403):', vals.filter(v => v.blocked).length);
  console.log('Other fetch failures:', vals.filter(v => v.fetchFailed).length);
  console.log('Areas with at least one candidate link:', vals.filter(v => (v.candidateLinks || []).length).length);
  console.log('Areas with an ok/moved candidate that mentions skiing:', vals.filter(v => (v.candidateLinks || []).some(c => ['ok', 'moved'].includes(c.linkCheck.state) && c.linkCheck.mentionsSkiing)).length);
  console.log('Candidate link states:', states);
})();
