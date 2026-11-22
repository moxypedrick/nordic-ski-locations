// Step 8 pilot: merge dev/verify/work/pilot-parts/result-*.json into
// dev/verify/results/pilot.json and print the STOP 2 summary.
// Usage: node dev/verify/scripts/08-merge-pilot.js
const fs = require('fs');
const path = require('path');
const { saveJson, loadJson } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const PARTS = path.join(ROOT, 'dev/verify/work/pilot-parts');
const ids = loadJson(path.join(ROOT, 'dev/verify/work/pilot-ids.json'), []);

const byId = {};
let searches = 0, fetches = 0;
for (const f of fs.readdirSync(PARTS)) {
  if (/^result-\d+\.json$/.test(f)) for (const r of loadJson(path.join(PARTS, f), [])) {
    if (byId[r.id]) console.log('Duplicate record:', r.id, 'in', f);
    byId[r.id] = r;
  }
  if (/^stats-\d+\.json$/.test(f)) { const s = loadJson(path.join(PARTS, f), {}); searches += s.searches || 0; fetches += s.fetches || 0; }
}

const records = ids.filter(id => byId[id]).map(id => byId[id]);
const missing = ids.filter(id => !byId[id]);
saveJson(path.join(ROOT, 'dev/verify/results/pilot.json'), records);

const count = (k) => records.reduce((m, r) => (m[r[k]] = (m[r[k]] || 0) + 1, m), {});
const areas = Object.fromEntries(loadJson(path.join(ROOT, 'dev/verify/work/areas.json'), []).map(a => [a.id, a]));
const cross = {};
for (const r of records) { const k = `${areas[r.id].group}/${r.status}`; cross[k] = (cross[k] || 0) + 1; }
console.log('Records:', records.length, '| missing:', missing.length, missing.join(', '));
console.log('Status:', count('status'));
console.log('Confidence:', count('confidence'));
console.log('Method:', count('method'));
console.log('Group/status:', cross);
console.log('With officialUrl:', records.filter(r => r.officialUrl).length);
console.log('Searches:', searches, '| fetches:', fetches);
