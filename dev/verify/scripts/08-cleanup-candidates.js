// Step 8 cleanup: list records that need another research pass.
//   - unverified records from batches 031+ whose stats show fewer than 3 searches
//     (the brief's checklist needs searches 1, 2 and 3 before unverified)
//   - ids named on the command line (for example flagged DNR parks)
// Writes dev/verify/work/batches/cleanup-input.json in the same shape as a
// batch input, so a research agent can redo them with the normal brief.
// Usage: node dev/verify/scripts/08-cleanup-candidates.js [extra-id ...]
const fs = require('fs');
const path = require('path');
const { saveJson, loadJson } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const BATCH = path.join(ROOT, 'dev/verify/work/batches');
const RES = path.join(ROOT, 'dev/verify/results');
const extra = new Set(process.argv.slice(2));

const inputs = {};
for (const f of fs.readdirSync(BATCH)) {
  const m = f.match(/^input-(\d+)\.json$/);
  if (m) for (const a of loadJson(path.join(BATCH, f), [])) inputs[a.id] = { ...a, batch: m[1] };
}

const picks = [];
const reasons = {};
for (const f of fs.readdirSync(RES)) {
  const m = f.match(/^batch-(\d+)\.json$/);
  if (!m) continue;
  const n = +m[1];
  const stats = loadJson(path.join(BATCH, `stats-${m[1]}.json`), {});
  for (const r of loadJson(path.join(RES, f), [])) {
    const s = (stats.perArea || {})[r.id] || {};
    let why = null;
    if (n >= 31 && r.status === 'unverified' && (s.searches || 0) < 3) why = `unverified after ${s.searches || 0} searches`;
    if (extra.has(r.id)) why = why ? why + '; named' : 'named';
    if (why && inputs[r.id]) { picks.push({ ...inputs[r.id], previousStatus: r.status }); reasons[r.id] = why; }
  }
}
saveJson(path.join(BATCH, 'cleanup-input.json'), picks);
console.log(`Cleanup candidates: ${picks.length}`);
for (const p of picks) console.log(`  ${p.id} (batch ${p.batch}): ${reasons[p.id]}`);
