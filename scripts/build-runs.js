/**
 * build-runs.js
 *
 * Phase 2: Stream-parse runs.geojson and output a lean file of nordic
 * trail lines for areas that are in our merged dataset.
 *
 * Usage: node scripts/build-runs.js
 *
 * Output: data/nordic-ski-runs-na.geojson
 */

'use strict';

const fs       = require('fs');
const path     = require('path');
const readline = require('readline');

const ROOT        = path.join(__dirname, '..');
const MERGED_FILE = path.join(ROOT, 'data', 'nordic-ski-areas-merged.geojson');
const RUNS_FILE   = path.join(ROOT, 'data', 'runs.geojson');
const OUT_FILE    = path.join(ROOT, 'data', 'nordic-ski-runs-na.geojson');

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

const outStream = fs.createWriteStream(OUT_FILE, { encoding: 'utf8' });
outStream.write('{"type":"FeatureCollection","features":[\n');

let total = 0, kept = 0, first = true;

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

  const json = JSON.stringify(lean);
  if (!first) outStream.write(',\n');
  outStream.write(json);
  first = false;
  kept++;

  if (kept % 1000 === 0) process.stdout.write(`\r  Kept ${kept} runs so far...`);
});

rl.on('close', () => {
  outStream.write('\n]}');
  outStream.end(() => {
    const sizeMB = (fs.statSync(OUT_FILE).size / 1024 / 1024).toFixed(1);
    console.log(`\n  Scanned: ${total.toLocaleString()} lines`);
    console.log(`  Kept:    ${kept.toLocaleString()} nordic NA runs linked to our areas`);
    console.log(`  Output:  ${OUT_FILE} (${sizeMB} MB)`);
    console.log('\nDone.');

    // Print grooming breakdown
    const sample = JSON.parse(fs.readFileSync(OUT_FILE, 'utf8'));
    const groomCounts = {};
    const diffCounts  = {};
    for (const f of sample.features) {
      const g = f.properties.grooming || 'null';
      const d = f.properties.difficulty || 'null';
      groomCounts[g] = (groomCounts[g] || 0) + 1;
      diffCounts[d]  = (diffCounts[d]  || 0) + 1;
    }
    console.log('\nGrooming breakdown:', groomCounts);
    console.log('Difficulty breakdown:', diffCounts);
  });
});
