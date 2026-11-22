// Step 9b: build data/verification.json and dev/verify/review-queue.csv
// from the validated records in dev/verify/results/.
// Run 09a-validate.js first; this script refuses to run if records are missing.
// Usage: node dev/verify/scripts/09b-build.js
const fs = require('fs');
const path = require('path');
const { loadJson } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const RES = path.join(ROOT, 'dev/verify/results');
const areas = loadJson(path.join(ROOT, 'dev/verify/work/areas.json'), []);
const areaById = Object.fromEntries(areas.map(a => [a.id, a]));

// Same duplicate rule as 09a: prefer the research record.
const byId = {};
for (const f of fs.readdirSync(RES).filter(f => f.endsWith('.json')).sort()) {
  for (const r of loadJson(path.join(RES, f), [])) {
    if (!byId[r.id] || (r.method === 'research' && byId[r.id].method !== 'research')) byId[r.id] = r;
  }
}
const missing = areas.filter(a => !byId[a.id]);
if (missing.length) { console.error(`${missing.length} ids have no record. Run 09a-validate.js and finish research first.`); process.exit(1); }

// verification.json in areas.json order
const out = {};
for (const a of areas) out[a.id] = byId[a.id];
fs.writeFileSync(path.join(ROOT, 'data/verification.json'), JSON.stringify(out, null, 1));

// review queue
const urlCount = {};
for (const r of Object.values(out)) if (r.officialUrl) urlCount[r.officialUrl] = (urlCount[r.officialUrl] || 0) + 1;
const csvCell = v => { const s = v == null ? '' : String(v); return /[",\n\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s; };
const cols = ['id', 'name', 'region', 'status', 'confidence', 'officialUrl', 'oldUrl', 'suggestedName', 'evidenceUrl', 'evidenceSummary', 'comment', 'reason'];
const rows = [cols.join(',')];
const reasonCounts = {};
for (const a of areas) {
  const r = out[a.id];
  const reasons = [];
  if (r.status === 'closed') reasons.push('closed');
  if (r.confidence === 'low') reasons.push('low confidence');
  if (r.suggestedName) reasons.push('rename');
  if (r.comment && r.comment.trim()) reasons.push('comment');
  if (r.officialUrl && urlCount[r.officialUrl] > 3) reasons.push('shared url');
  if (!reasons.length) continue;
  reasons.forEach(x => reasonCounts[x] = (reasonCounts[x] || 0) + 1);
  const ev = (r.evidence || [])[0] || {};
  rows.push([a.id, a.name, a.region, r.status, r.confidence, r.officialUrl, a.url, r.suggestedName, ev.url, ev.summary, r.comment, reasons.join('; ')].map(csvCell).join(','));
}
// BOM so Excel reads accented names correctly
fs.writeFileSync(path.join(ROOT, 'dev/verify/review-queue.csv'), '﻿' + rows.join('\r\n') + '\r\n');

const tally = k => Object.values(out).reduce((m, r) => (m[r[k]] = (m[r[k]] || 0) + 1, m), {});
const cross = {};
for (const r of Object.values(out)) { const k = `${r.status}/${r.confidence}`; cross[k] = (cross[k] || 0) + 1; }
console.log('verification.json records:', Object.keys(out).length);
console.log('Status:', tally('status'));
console.log('Confidence:', tally('confidence'));
console.log('Status/confidence:', cross);
console.log('With officialUrl:', Object.values(out).filter(r => r.officialUrl).length);
console.log('Review queue rows:', rows.length - 1, reasonCounts);
