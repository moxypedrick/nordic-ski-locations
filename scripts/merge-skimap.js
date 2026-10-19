/**
 * merge-skimap.js
 *
 * Phase 1: Match our nordic-ski-areas-v2.geojson against skimap ski_areas.geojson
 * and produce a merged dataset + verification report.
 *
 * Usage: node scripts/merge-skimap.js
 *
 * Outputs:
 *   data/nordic-ski-areas-merged.geojson  — enriched + net-new areas
 *   data/merge-report.json               — match stats and samples for review
 */

'use strict';

const fs = require('fs');
const path = require('path');

const ROOT = path.join(__dirname, '..');
const OUR_FILE       = path.join(ROOT, 'data', 'nordic-ski-areas-v2.geojson');
const SKIMAP_FILE    = path.join(ROOT, 'data', 'ski_areas.geojson');
const OUT_MERGED     = path.join(ROOT, 'data', 'nordic-ski-areas-merged.geojson');
const OUT_REPORT     = path.join(ROOT, 'data', 'merge-report.json');
const OVERRIDES_FILE = path.join(ROOT, 'data', 'manual-overrides.json');

const MATCH_KM = 1.0;   // proximity threshold
const NEW_AREA_MIN_KM = 1.0; // skimap-only areas must have ≥ this many km of runs

// --- Haversine distance in km ---
function distKm(lon1, lat1, lon2, lat2) {
  const R = 6371;
  const dLat = (lat2 - lat1) * Math.PI / 180;
  const dLon = (lon2 - lon1) * Math.PI / 180;
  const a = Math.sin(dLat/2)**2 +
            Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
            Math.sin(dLon/2)**2;
  return R * 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
}

// --- Extract total nordic km from skimap statistics ---
function nordicKmFromStats(stats) {
  try {
    const byDiff = stats.runs.byActivity.nordic.byDifficulty;
    return Object.values(byDiff).reduce((sum, d) => sum + (d.lengthInKm || 0), 0);
  } catch { return 0; }
}

// --- Extract km breakdown by grooming from skimap statistics ---
function nordicKmByDifficulty(stats) {
  try {
    const byDiff = stats.runs.byActivity.nordic.byDifficulty;
    const result = {};
    for (const [diff, d] of Object.entries(byDiff)) {
      if (d.lengthInKm > 0) result[diff] = Math.round(d.lengthInKm * 10) / 10;
    }
    return result;
  } catch { return {}; }
}

// ============================================================
// Apply manual overrides from manual-overrides.json
// ============================================================
function applyOverrides(gj, overridesFile) {
  if (!fs.existsSync(overridesFile)) return 0;
  const overrides = JSON.parse(fs.readFileSync(overridesFile, 'utf8'));
  const ids = Object.keys(overrides);
  if (!ids.length) return 0;

  // Index features by id for fast lookup
  const byId = new Map(gj.features.map(f => [f.properties.id, f]));
  let applied = 0;

  for (const id of ids) {
    const feature = byId.get(id);
    if (!feature) continue;
    const patch = overrides[id];
    if (patch.geometry) feature.geometry = patch.geometry;
    if (patch.properties) Object.assign(feature.properties, patch.properties);
    applied++;
  }
  return applied;
}

// ============================================================
// Load data
// ============================================================
console.log('Loading our data...');
const ourGJ = JSON.parse(fs.readFileSync(OUR_FILE, 'utf8'));
const ourFeatures = ourGJ.features;
console.log(`  Our areas: ${ourFeatures.length}`);

console.log('Loading skimap ski_areas...');
const skimapGJ = JSON.parse(fs.readFileSync(SKIMAP_FILE, 'utf8'));

// Filter skimap to nordic, NA (US or CA), operating
const skimapNordic = skimapGJ.features.filter(f => {
  const p = f.properties;
  if (!p.activities || !p.activities.includes('nordic')) return false;
  if (p.status !== 'operating') return false;
  const places = p.places || [];
  return places.some(pl => ['US', 'CA'].includes(pl.iso3166_1Alpha2));
});
console.log(`  Skimap nordic NA operating areas: ${skimapNordic.length}`);

// --- Get a representative [lon, lat] for a skimap feature ---
// ski_areas can be Point or Polygon; viewportHint.center is always [lon, lat]
function skimapCenter(f) {
  const vh = f.properties.viewportHint;
  if (vh && vh.center && vh.center.length === 2) return vh.center;
  if (f.geometry.type === 'Point') return f.geometry.coordinates;
  return null;
}

// ============================================================
// Spatial match: for each skimap area, find our closest area
// ============================================================
console.log('Matching areas (1km threshold)...');

// For each of OUR areas, find the closest skimap area within 1km
const matched = [];       // { ourIdx, skimapIdx, distKm }
const ourMatchedSet = new Set();
const skimapMatchedSet = new Set();

for (let si = 0; si < skimapNordic.length; si++) {
  const sf = skimapNordic[si];
  const center = skimapCenter(sf);
  if (!center) continue;
  const [sLon, sLat] = center;
  let bestDist = Infinity;
  let bestOurIdx = -1;

  for (let oi = 0; oi < ourFeatures.length; oi++) {
    const of_ = ourFeatures[oi];
    const [oLon, oLat] = of_.geometry.coordinates;
    const d = distKm(sLon, sLat, oLon, oLat);
    if (d < bestDist) {
      bestDist = d;
      bestOurIdx = oi;
    }
  }

  if (bestDist <= MATCH_KM) {
    // Only record if this our-area hasn't been claimed by a closer skimap area
    const existingMatch = matched.find(m => m.ourIdx === bestOurIdx);
    if (!existingMatch || existingMatch.distKm > bestDist) {
      if (existingMatch) {
        // Remove old match — a closer skimap area won
        const idx = matched.indexOf(existingMatch);
        matched.splice(idx, 1);
        skimapMatchedSet.delete(existingMatch.skimapIdx);
      }
      matched.push({ ourIdx: bestOurIdx, skimapIdx: si, distKm: bestDist });
      ourMatchedSet.add(bestOurIdx);
      skimapMatchedSet.add(si);
    }
  }
}

// ourOnly and skimapOnlyRaw computed after pass 2 below

// Filter net-new: must have a name and ≥ NEW_AREA_MIN_KM of nordic runs
// ============================================================
// Pass 2: name-similarity match for unmatched areas (up to 15km)
// Catches large trail systems where center points are far apart
// ============================================================
const NAME_MATCH_KM = 15;

function tokenize(str) {
  if (!str) return new Set();
  return new Set(
    str.toLowerCase()
      .replace(/[^a-z0-9 ]/g, ' ')
      .split(/\s+/)
      .filter(w => w.length > 2 && !['ski','nordic','trail','trails','cross','country','park','lake','center','centre'].includes(w))
  );
}

function nameSimilarity(a, b) {
  const ta = tokenize(a), tb = tokenize(b);
  if (!ta.size || !tb.size) return 0;
  let overlap = 0;
  for (const t of ta) if (tb.has(t)) overlap++;
  return overlap / Math.min(ta.size, tb.size);
}

const unmatchedOurIdxs = new Set(
  ourFeatures.map((_, i) => i).filter(i => !ourMatchedSet.has(i))
);
const unmatchedSkimapIdxs = skimapNordic
  .map((_, i) => i)
  .filter(i => !skimapMatchedSet.has(i));

let pass2Count = 0;
for (const si of unmatchedSkimapIdxs) {
  const sf = skimapNordic[si];
  const center = skimapCenter(sf);
  if (!center) continue;
  const [sLon, sLat] = center;

  let bestScore = 0, bestDist = Infinity, bestOurIdx = -1;

  for (const oi of unmatchedOurIdxs) {
    const of_ = ourFeatures[oi];
    const [oLon, oLat] = of_.geometry.coordinates;
    const d = distKm(sLon, sLat, oLon, oLat);
    if (d > NAME_MATCH_KM) continue;

    const score = nameSimilarity(sf.properties.name, of_.properties.Name);
    if (score < 0.5) continue; // require ≥50% token overlap

    // Prefer high name score, break ties by distance
    if (score > bestScore || (score === bestScore && d < bestDist)) {
      bestScore = score;
      bestDist = d;
      bestOurIdx = oi;
    }
  }

  if (bestOurIdx !== -1) {
    matched.push({ ourIdx: bestOurIdx, skimapIdx: si, distKm: bestDist, nameScore: bestScore, pass: 2 });
    ourMatchedSet.add(bestOurIdx);
    skimapMatchedSet.add(si);
    unmatchedOurIdxs.delete(bestOurIdx);
    pass2Count++;
  }
}
console.log(`  Pass 2 name-similarity matches: ${pass2Count}`);
console.log(`  Total matched: ${matched.length}`);

const skimapOnlyRaw = skimapNordic.filter((_, i) => !skimapMatchedSet.has(i));
const ourOnly = ourFeatures.filter((_, i) => !ourMatchedSet.has(i));

const skimapOnly = skimapOnlyRaw.filter(f => {
  if (!f.properties.name) return false;
  const km = nordicKmFromStats(f.properties.statistics);
  return km >= NEW_AREA_MIN_KM;
});

console.log(`  Matched pairs: ${matched.length}`);
console.log(`  Skimap-only (raw): ${skimapOnlyRaw.length}  → after filter (name + ≥${NEW_AREA_MIN_KM}km): ${skimapOnly.length}`);
console.log(`  Our-only (not in skimap): ${ourOnly.length}`);

// ============================================================
// Build merged feature collection
// ============================================================
console.log('Building merged dataset...');

const mergedFeatures = [];

// 1. Matched: our data enriched with skimap id + km stats
for (const { ourIdx, skimapIdx, distKm: d } of matched) {
  const of_ = ourFeatures[ourIdx];
  const sf = skimapNordic[skimapIdx];
  const skimapKm = nordicKmFromStats(sf.properties.statistics);
  const kmByDiff = nordicKmByDifficulty(sf.properties.statistics);

  const enriched = {
    ...of_,
    properties: {
      ...of_.properties,
      skimapId: sf.properties.id,
      skimapKm: Math.round(skimapKm * 10) / 10,
      skimapKmByDifficulty: Object.keys(kmByDiff).length ? kmByDiff : undefined,
      // Add website from skimap if we don't have one
      URL: of_.properties.URL || (sf.properties.websites[0] || null),
      source: 'our-data',
      matchDistKm: Math.round(d * 100) / 100,
    },
  };
  mergedFeatures.push(enriched);
}

// 2. Our-only: unchanged
for (const f of ourOnly) {
  mergedFeatures.push({ ...f, properties: { ...f.properties, source: 'our-data' } });
}

// 3. Net-new from skimap — use a Point geometry (viewportHint center or existing Point)
for (const sf of skimapOnly) {
  const places = sf.properties.places || [];
  const place = places[0];
  const skimapKm = nordicKmFromStats(sf.properties.statistics);
  const kmByDiff = nordicKmByDifficulty(sf.properties.statistics);

  const region = place
    ? `${place.localized.en.locality ? place.localized.en.locality + ', ' : ''}${place.iso3166_2}`
    : null;

  const center = skimapCenter(sf);
  if (!center) continue; // skip if no usable coordinate

  const feature = {
    type: 'Feature',
    geometry: { type: 'Point', coordinates: center },
    properties: {
      id: sf.properties.id,
      Name: sf.properties.name,
      region,
      groomSkate: null,
      groomClassic: null,
      groomType: null,
      Groomed: null,
      km: Math.round(skimapKm),
      kmSkate: null,
      kmClassic: null,
      Snowmaking: null,
      Lit: null,
      URL: sf.properties.websites[0] || null,
      Notes: null,
      passPrice: null,
      rentals: null,
      lessons: null,
      dogs: null,
      photo: null,
      groomingUrl: null,
      skimapId: sf.properties.id,
      skimapKm: Math.round(skimapKm * 10) / 10,
      skimapKmByDifficulty: Object.keys(kmByDiff).length ? kmByDiff : undefined,
      source: 'skimap',
    },
  };
  mergedFeatures.push(feature);
}

const mergedGJ = { type: 'FeatureCollection', features: mergedFeatures };

// Apply manual overrides before writing
const overrideCount = applyOverrides(mergedGJ, OVERRIDES_FILE);
if (overrideCount > 0) console.log(`  Applied ${overrideCount} manual override(s)`);

fs.writeFileSync(OUT_MERGED, JSON.stringify(mergedGJ));
console.log(`  Merged dataset: ${mergedGJ.features.length} features → ${OUT_MERGED}`);

// ============================================================
// Verification report
// ============================================================
console.log('Building verification report...');

// Sample 20 matched pairs for review
const sampleMatches = matched
  .sort((a, b) => a.distKm - b.distKm)
  .filter((_, i) => i % Math.floor(matched.length / 20) === 0)
  .slice(0, 20)
  .map(({ ourIdx, skimapIdx, distKm: d }) => {
    const of_ = ourFeatures[ourIdx];
    const sf = skimapNordic[skimapIdx];
    return {
      ourName:     of_.properties.Name,
      ourRegion:   of_.properties.region,
      ourKm:       of_.properties.km,
      skimapName:  sf.properties.name,
      skimapKm:    Math.round(nordicKmFromStats(sf.properties.statistics) * 10) / 10,
      distKm:      Math.round(d * 100) / 100,
      skimapWebsite: sf.properties.websites[0] || null,
    };
  });

// Sample 20 net-new areas for review
const sampleNew = skimapOnly
  .sort((a, b) => nordicKmFromStats(b.properties.statistics) - nordicKmFromStats(a.properties.statistics))
  .slice(0, 20)
  .map(sf => {
    const places = sf.properties.places || [];
    const place = places[0];
    return {
      name:    sf.properties.name,
      region:  place ? `${place.iso3166_2}` : '?',
      locality: place?.localized?.en?.locality || null,
      km:      Math.round(nordicKmFromStats(sf.properties.statistics) * 10) / 10,
      website: sf.properties.websites[0] || null,
    };
  });

// Distance histogram for matched pairs
const distBuckets = { '0-0.1': 0, '0.1-0.25': 0, '0.25-0.5': 0, '0.5-0.75': 0, '0.75-1.0': 0 };
for (const { distKm: d } of matched) {
  if (d <= 0.1) distBuckets['0-0.1']++;
  else if (d <= 0.25) distBuckets['0.1-0.25']++;
  else if (d <= 0.5) distBuckets['0.25-0.5']++;
  else if (d <= 0.75) distBuckets['0.5-0.75']++;
  else distBuckets['0.75-1.0']++;
}

const report = {
  summary: {
    ourTotal:       ourFeatures.length,
    skimapNordicNA: skimapNordic.length,
    matched:        matched.length,
    ourOnly:        ourOnly.length,
    skimapOnlyRaw:  skimapOnlyRaw.length,
    skimapOnlyFiltered: skimapOnly.length,
    mergedTotal:    mergedFeatures.length,
    matchThresholdKm: MATCH_KM,
    newAreaMinKm:   NEW_AREA_MIN_KM,
  },
  distanceHistogram: distBuckets,
  sampleMatches,
  sampleNewAreas: sampleNew,
};

fs.writeFileSync(OUT_REPORT, JSON.stringify(report, null, 2));
console.log(`  Report → ${OUT_REPORT}`);
console.log('\nDone.');
