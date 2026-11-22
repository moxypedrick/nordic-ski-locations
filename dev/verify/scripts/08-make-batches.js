// Step 8 full run: split remaining site-check/research areas into batches (BATCH_SIZE env, default 25),
// in triage.json order, skipping ids that already have a record in results/.
// Writes dev/verify/work/batches/input-NNN.json. Existing input files are kept
// unchanged so batch numbers stay stable on re-runs.
// Usage: node dev/verify/scripts/08-make-batches.js
const fs = require('fs');
const path = require('path');
const { saveJson, loadJson } = require('./lib/check-url');
const ROOT = path.resolve(__dirname, '../../..');
const W = f => path.join(ROOT, 'dev/verify/work', f);
const RES = path.join(ROOT, 'dev/verify/results');
const BATCH = W('batches');

const done = new Set();
for (const f of fs.readdirSync(RES)) if (f.endsWith('.json')) for (const r of loadJson(path.join(RES, f), [])) done.add(r.id);
const assigned = new Set();
let maxN = 0;
for (const f of fs.readdirSync(BATCH)) {
  const m = f.match(/^input-(\d+)\.json$/);
  if (m) { maxN = Math.max(maxN, +m[1]); for (const a of loadJson(path.join(BATCH, f), [])) assigned.add(a.id); }
}
const areas = Object.fromEntries(loadJson(W('areas.json'), []).map(a => [a.id, a]));
const todo = loadJson(W('triage.json'), [])
  .filter(t => t.bucket !== 'auto' && !done.has(t.id) && !assigned.has(t.id))
  .map(t => { const a = areas[t.id]; return { id: a.id, name: a.name, region: a.region, lat: a.lat, lon: a.lon, group: a.group, source: a.source, oldUrl: a.url, bucket: t.bucket, candidateUrl: t.candidateUrl, candidateState: t.candidateState, hint: t.hint }; });
let n = maxN;
const SIZE = Number(process.env.BATCH_SIZE || 25);
for (let i = 0; i < todo.length; i += SIZE) saveJson(path.join(BATCH, `input-${String(++n).padStart(3, '0')}.json`), todo.slice(i, i + SIZE));
console.log(`done ${done.size}, already batched ${assigned.size}, new areas ${todo.length}, batches now 1..${n}`);
