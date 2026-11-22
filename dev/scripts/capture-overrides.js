/**
 * capture-overrides.js
 *
 * Finds edits that were made directly to data/nordic-ski-areas-merged.geojson
 * and are not yet in data/manual-overrides.json, and adds them there, so that
 * a rebuild with merge-skimap.js reproduces the current file.
 *
 * How it works:
 *   1. Runs merge-skimap.js into a temporary file, without manual overrides
 *      (verification.json is still applied, as in a normal rebuild).
 *   2. Compares that rebuild with the current merged file, feature by feature.
 *   3. For each difference, writes an override holding the CURRENT value:
 *        - area in the rebuild but not in the current file -> { deleted: true }
 *        - changed property    -> properties: { key: current value }
 *        - changed coordinates -> geometry: current geometry
 *      Existing entries in manual-overrides.json are kept; captured values are
 *      merged into them.
 *      Region values that differ only by an older "US-"/"CA-" prefix are
 *      skipped, so the rebuild's newer "Town, ST" format is kept.
 *   4. Reports areas that exist only in the current file (for example areas
 *      added in admin.html). Those cannot be expressed as overrides.
 *
 * It never writes to the merged file.
 *
 * Usage: node dev/scripts/capture-overrides.js            (writes overrides)
 *        node dev/scripts/capture-overrides.js --dry-run  (only prints)
 */

'use strict';

const fs   = require('fs');
const os   = require('os');
const path = require('path');
const { execFileSync } = require('child_process');

const ROOT           = path.join(__dirname, '..', '..');
const MERGED_FILE    = path.join(ROOT, 'data', 'nordic-ski-areas-merged.geojson');
const OVERRIDES_FILE = path.join(ROOT, 'data', 'manual-overrides.json');
const DRY_RUN        = process.argv.includes('--dry-run');

const tmpOut = path.join(os.tmpdir(), `merge-rebuild-${Date.now()}.geojson`);
console.log('Rebuilding without manual overrides (temporary file)...');
execFileSync(process.execPath, [path.join(__dirname, 'merge-skimap.js')], {
  env: { ...process.env, MERGE_OUT_FILE: tmpOut, MERGE_SKIP_OVERRIDES: '1' },
  stdio: ['ignore', 'ignore', 'inherit'],
});

const rebuilt = JSON.parse(fs.readFileSync(tmpOut, 'utf8'));
const current = JSON.parse(fs.readFileSync(MERGED_FILE, 'utf8'));
for (const f of [tmpOut, tmpOut + '.report.json']) { try { fs.unlinkSync(f); } catch {} }

const overrides = fs.existsSync(OVERRIDES_FILE) ? JSON.parse(fs.readFileSync(OVERRIDES_FILE, 'utf8')) : {};
const curById = new Map(current.features.map(f => [f.properties.id, f]));
const rebById = new Map(rebuilt.features.map(f => [f.properties.id, f]));
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

const report = { deleted: [], edited: 0, geometry: 0, fields: {}, removedKeys: [], onlyInCurrent: [] };

for (const [id, rf] of rebById) {
  const cf = curById.get(id);
  if (!cf) {
    overrides[id] = { deleted: true };
    report.deleted.push(`${id} (${rf.properties.Name})`);
    continue;
  }
  const props = {};
  for (const k of new Set([...Object.keys(rf.properties), ...Object.keys(cf.properties)])) {
    if (same(rf.properties[k], cf.properties[k])) continue;
    // Region format only ("Erwin, US-MI" in an older build vs "Erwin, MI" now):
    // not a hand edit, so let the rebuild's newer format stand.
    if (k === 'region' && typeof cf.properties[k] === 'string' &&
        cf.properties[k].replace(/\b(US|CA)-/g, '') === rf.properties[k]) {
      report.regionFormat = (report.regionFormat || 0) + 1;
      continue;
    }
    if (!(k in cf.properties)) {
      // The current file has no such key. An override can set a value, not remove a key.
      report.removedKeys.push(`${id}.${k}`);
      continue;
    }
    props[k] = cf.properties[k];
    report.fields[k] = (report.fields[k] || 0) + 1;
  }
  const geoChanged = !same(rf.geometry, cf.geometry);
  if (!Object.keys(props).length && !geoChanged) continue;
  const entry = overrides[id] && !overrides[id].deleted ? overrides[id] : {};
  if (Object.keys(props).length) entry.properties = Object.assign(entry.properties || {}, props);
  if (geoChanged) { entry.geometry = cf.geometry; report.geometry++; }
  overrides[id] = entry;
  report.edited++;
}
for (const id of curById.keys()) if (!rebById.has(id)) report.onlyInCurrent.push(id);

console.log(`Deleted areas captured: ${report.deleted.length}`);
report.deleted.forEach(d => console.log(`  ${d}`));
console.log(`Edited areas captured: ${report.edited} (geometry changes: ${report.geometry})`);
console.log('Fields captured:', report.fields);
if (report.regionFormat) console.log(`Region format-only differences (not captured; the rebuild uses the newer "Town, ST" format): ${report.regionFormat}`);
if (report.removedKeys.length) console.log(`Keys present in the rebuild but missing from the current file (not captured): ${report.removedKeys.length}`, report.removedKeys.slice(0, 10));
if (report.onlyInCurrent.length) console.log(`Areas only in the current file (not captured): ${report.onlyInCurrent.length}`, report.onlyInCurrent.slice(0, 10));

if (DRY_RUN) {
  console.log('\n--dry-run: manual-overrides.json not written.');
} else {
  fs.writeFileSync(OVERRIDES_FILE, JSON.stringify(overrides, null, 2));
  console.log(`\nWrote ${Object.keys(overrides).length} entries to ${OVERRIDES_FILE}`);
}
