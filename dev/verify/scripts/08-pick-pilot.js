// Step 8 pilot: pick 60 non-auto areas spread across the list.
// 30 canadatrails, 10 skinnyski, 10 source skimap, 10 other (every Nth item).
// Writes dev/verify/work/pilot-ids.json and pilot-input.json (context for research).
// Usage: node dev/verify/scripts/08-pick-pilot.js
const path = require('path');
const { saveJson, loadJson } = require('./lib/check-url');
const ROOT = path.resolve(__dirname, '../../..');
const W = f => path.join(ROOT, 'dev/verify/work', f);

const areas = loadJson(W('areas.json'), []);
const triage = Object.fromEntries(loadJson(W('triage.json'), []).map(t => [t.id, t]));
const pool = areas.filter(a => triage[a.id].bucket !== 'auto');
const chosen = new Set();

function pick(list, n) {
  list = list.filter(a => !chosen.has(a.id));
  const step = list.length / n;
  const out = [];
  for (let i = 0; i < n && i * step < list.length; i++) out.push(list[Math.floor(i * step)]);
  out.forEach(a => chosen.add(a.id));
  return out;
}

const picks = [
  ...pick(pool.filter(a => a.group === 'canadatrails'), 30),
  ...pick(pool.filter(a => a.group === 'skinnyski'), 10),
  ...pick(pool.filter(a => a.source === 'skimap'), 10),
  ...pick(pool.filter(a => a.group === 'other'), 10),
];
saveJson(W('pilot-ids.json'), picks.map(a => a.id));
saveJson(W('pilot-input.json'), picks.map(a => ({
  id: a.id, name: a.name, region: a.region, lat: a.lat, lon: a.lon, group: a.group, source: a.source,
  oldUrl: a.url, bucket: triage[a.id].bucket, candidateUrl: triage[a.id].candidateUrl,
  candidateState: triage[a.id].candidateState, hint: triage[a.id].hint,
})));
const c = {};
for (const a of picks) { const k = `${a.group}/${triage[a.id].bucket}`; c[k] = (c[k] || 0) + 1; }
console.log('Pilot ids:', picks.length, c);
