// Step 3: run checkUrl on every area in group "other".
// Resumable: skips ids already in the output file.
// Usage: node dev/verify/scripts/03-check-other.js
const path = require('path');
const { checkMany, saveJson, loadJson } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const AREAS = path.join(ROOT, 'dev/verify/work/areas.json');
const OUT = path.join(ROOT, 'dev/verify/work/linkcheck-other.json');

(async () => {
  const areas = loadJson(AREAS, []).filter(a => a.group === 'other');
  const results = loadJson(OUT, {});
  const todo = areas.filter(a => !results[a.id]);
  console.log(`other: ${areas.length} total, ${todo.length} to check`);
  let n = 0;
  await checkMany(todo.map(a => ({ key: a.id, url: a.url })), (key, res) => {
    results[key] = res;
    saveJson(OUT, results);
    if (++n % 25 === 0) console.log(`  ${n}/${todo.length}`);
  });
  const counts = {};
  for (const a of areas) { const s = results[a.id] ? results[a.id].state : 'missing'; counts[s] = (counts[s] || 0) + 1; }
  console.log('Entries:', Object.keys(results).length);
  console.log('States:', counts);
})();
