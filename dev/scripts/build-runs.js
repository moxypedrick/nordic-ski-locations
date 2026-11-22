/**
 * build-runs.js
 *
 * Phase 2: Stream-parse runs.geojson and output per-area GeoJSON files
 * of nordic trail lines for areas that are in our merged dataset.
 *
 * Usage: node dev/scripts/build-runs.js
 *
 * Output:
 *   data/runs/<skimapId>.geojson  — one lean FeatureCollection per area
 *   data/runs/index.json          — sorted array of skimapIds with run files
 */

'use strict';

const fs       = require('fs');
const path     = require('path');
const readline = require('readline');

const ROOT        = path.join(__dirname, '..', '..'); // repo root (scripts live in dev/scripts)
const MERGED_FILE = path.join(ROOT, 'data', 'nordic-ski-areas-merged.geojson');
const RUNS_FILE   = path.join(ROOT, 'dev', 'source-data', 'runs.geojson');
const OUT_DIR     = path.join(ROOT, 'data', 'runs');

// ── Collect skimapIds present in the merged dataset ──────────────────────────
console.log('Loading merged dataset for skimapId index...');
const merged = JSON.parse(fs.readFileSync(MERGED_FILE, 'utf8'));
const validSkimapIds = new Set(
  merged.features
    .map(f => f.properties.skimapId)
    .filter(Boolean)
);
console.log(`  Areas with skimapId: ${validSkimapIds.size}`);

// ── Stream runs.geojson line by line ─────────────────────────────────────────
console.log('Streaming runs.geojson...');

const runsByArea = new Map();
const groomCounts = {};
const diffCounts  = {};
let total = 0, kept = 0;

const rl = readline.createInterface({
  input: fs.createReadStream(RUNS_FILE, { encoding: 'utf8' }),
  crlfDelay: Infinity,
});

rl.on('line', (raw) => {
  const line = raw.trim().replace(/^,/, '');
  if (!line.startsWith('{"type":"Feature"')) return;

  total++;

  let f;
  try { f = JSON.parse(line); } catch { return; }

  const p = f.properties;

  // Filter 1: must be a nordic run
  if (!p.uses || !p.uses.includes('nordic')) return;

  // Filter 2: must be in NA (US or CA)
  const places = p.places || [];
  if (!places.some(pl => ['US', 'CA'].includes(pl.iso3166_1Alpha2))) return;

  // Filter 3: operating only
  if (p.status !== 'operating') return;

  // Filter 4: must belong to a ski area in our merged dataset
  const skiAreas = p.skiAreas || [];
  const matchedAreaId = skiAreas
    .map(sa => sa?.properties?.id)
    .find(id => id && validSkimapIds.has(id));
  if (!matchedAreaId) return;

  // Build lean output — drop elevationProfile and viewportHint
  const lean = {
    type: 'Feature',
    geometry: f.geometry,
    properties: {
      id:         p.id,
      name:       p.name || null,
      ref:        p.ref  || null,
      grooming:   p.grooming   || null,
      difficulty: p.difficulty || null,
      lit:        p.lit        || null,
      oneway:     p.oneway     || null,
      skiAreaId:  matchedAreaId,
    },
  };

  if (!runsByArea.has(matchedAreaId)) runsByArea.set(matchedAreaId, []);
  runsByArea.get(matchedAreaId).push(lean);

  const g = lean.properties.grooming || 'null';
  const d = lean.properties.difficulty || 'null';
  groomCounts[g] = (groomCounts[g] || 0) + 1;
  diffCounts[d]  = (diffCounts[d]  || 0) + 1;
  kept++;

  if (kept % 1000 === 0) process.stdout.write(`\r  Kept ${kept} runs so far...`);
});

rl.on('close', () => {
  // Prepare clean output directory
  fs.mkdirSync(OUT_DIR, { recursive: true });
  for (const name of fs.readdirSync(OUT_DIR)) {
    if (name.endsWith('.geojson') || name === 'index.json') {
      fs.unlinkSync(path.join(OUT_DIR, name));
    }
  }

  let totalBytes = 0;
  const ids = Array.from(runsByArea.keys()).sort();
  for (const id of ids) {
    const payload = JSON.stringify({ type: 'FeatureCollection', features: runsByArea.get(id) });
    fs.writeFileSync(path.join(OUT_DIR, `${id}.geojson`), payload, 'utf8');
    totalBytes += Buffer.byteLength(payload, 'utf8');
  }

  fs.writeFileSync(path.join(OUT_DIR, 'index.json'), JSON.stringify(ids), 'utf8');

  const sizeMB = (totalBytes / 1024 / 1024).toFixed(1);
  console.log(`\n  Scanned: ${total.toLocaleString()} lines`);
  console.log(`  Kept:    ${kept.toLocaleString()} nordic NA runs across ${ids.length} areas`);
  console.log(`  Output:  ${OUT_DIR} (${sizeMB} MB total, avg ${Math.round(totalBytes / ids.length / 1024)} KB/area)`);
  console.log('\nDone.');
  console.log('\nGrooming breakdown:', groomCounts);
  console.log('Difficulty breakdown:', diffCounts);
});
