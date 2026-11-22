// Step 1: build the working list of areas from the merged file.
// Usage: node dev/verify/scripts/01-build-list.js
const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '../../..');
const SRC = path.join(ROOT, 'data/nordic-ski-areas-merged.geojson');
const OUT = path.join(ROOT, 'dev/verify/work/areas.json');

function groupFor(url) {
  if (url == null || String(url).trim() === '') return 'none';
  if (url.includes('canadatrails.ca')) return 'canadatrails';
  if (url.includes('skinnyski.com')) return 'skinnyski';
  if (url.includes('paccsa.org')) return 'paccsa';
  return 'other';
}

const geo = JSON.parse(fs.readFileSync(SRC, 'utf8'));
const areas = geo.features.map(f => {
  const p = f.properties;
  return {
    id: p.id,
    name: p.Name,
    region: p.region,
    lat: f.geometry.coordinates[1],
    lon: f.geometry.coordinates[0],
    url: p.URL == null || String(p.URL).trim() === '' ? null : p.URL,
    group: groupFor(p.URL),
    source: p.source,
  };
});

fs.writeFileSync(OUT, JSON.stringify(areas, null, 1));
const counts = {};
for (const a of areas) counts[a.group] = (counts[a.group] || 0) + 1;
const ids = new Set(areas.map(a => a.id));
console.log('Items:', areas.length, 'unique ids:', ids.size);
console.log('Groups:', counts);
