/**
 * apply-overrides.js
 *
 * Applies manual-overrides.json on top of nordic-ski-areas-merged.geojson
 * without re-running the full merge. Use this after editing in admin.html
 * when you want to bake the overrides into the file for committing.
 *
 * Usage: node dev/scripts/apply-overrides.js
 */

'use strict';

const fs   = require('fs');
const path = require('path');

const ROOT           = path.join(__dirname, '..', '..'); // repo root (scripts live in dev/scripts)
const MERGED_FILE    = path.join(ROOT, 'data', 'nordic-ski-areas-merged.geojson');
const OVERRIDES_FILE = path.join(ROOT, 'data', 'manual-overrides.json');

if (!fs.existsSync(OVERRIDES_FILE)) {
  console.log('No manual-overrides.json found — nothing to apply.');
  process.exit(0);
}

const overrides = JSON.parse(fs.readFileSync(OVERRIDES_FILE, 'utf8'));
const ids = Object.keys(overrides);

if (!ids.length) {
  console.log('manual-overrides.json is empty — nothing to apply.');
  process.exit(0);
}

console.log(`Loading ${MERGED_FILE}...`);
const gj = JSON.parse(fs.readFileSync(MERGED_FILE, 'utf8'));
const byId = new Map(gj.features.map(f => [f.properties.id, f]));

let applied = 0, missing = [];
const deleted = new Set();

for (const id of ids) {
  const feature = byId.get(id);
  const patch = overrides[id];
  if (!feature) {
    // An area deleted earlier is already gone from the merged file; that is fine.
    if (!patch.deleted) missing.push(id);
    continue;
  }
  // { deleted: true } removes the area (recorded by admin.html's Delete button)
  if (patch.deleted) {
    deleted.add(id);
    applied++;
    console.log(`  ✗ ${feature.properties.Name || id} (deleted)`);
    continue;
  }
  if (patch.geometry)    feature.geometry = patch.geometry;
  if (patch.properties)  Object.assign(feature.properties, patch.properties);
  applied++;
  console.log(`  ✓ ${feature.properties.Name || id}`);
}

if (deleted.size) gj.features = gj.features.filter(f => !deleted.has(f.properties.id));
fs.writeFileSync(MERGED_FILE, JSON.stringify(gj));

console.log(`\nApplied ${applied} override(s).`);
if (missing.length) {
  console.log(`Could not find ${missing.length} id(s) in the merged file: ${missing.join(', ')}`);
}
