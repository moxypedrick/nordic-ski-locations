#!/usr/bin/env node
// Build script: reads nordic-ski-areas.geojson and outputs nordic-ski-areas-v2.geojson
// with normalized ids, regions, grooming booleans, and km fields.
// Usage: node scripts/build-geojson.js

'use strict';

const fs = require('fs');
const path = require('path');

const INPUT  = path.join(__dirname, '../data/nordic-ski-areas.geojson');
const OUTPUT = path.join(__dirname, '../data/nordic-ski-areas-v2.geojson');

// ---------------------------------------------------------------------------
// State / province abbreviation map
// ---------------------------------------------------------------------------
const STATE_ABBR = {
  // US states
  'Alabama': 'AL', 'Alaska': 'AK', 'Arizona': 'AZ', 'Arkansas': 'AR',
  'California': 'CA', 'Colorado': 'CO', 'Connecticut': 'CT', 'Delaware': 'DE',
  'Florida': 'FL', 'Georgia': 'GA', 'Hawaii': 'HI', 'Idaho': 'ID',
  'Illinois': 'IL', 'Indiana': 'IN', 'Iowa': 'IA', 'Kansas': 'KS',
  'Kentucky': 'KY', 'Louisiana': 'LA', 'Maine': 'ME', 'Maryland': 'MD',
  'Massachusetts': 'MA', 'Michigan': 'MI', 'Minnesota': 'MN', 'Mississippi': 'MS',
  'Missouri': 'MO', 'Montana': 'MT', 'Nebraska': 'NE', 'Nevada': 'NV',
  'New Hampshire': 'NH', 'New Jersey': 'NJ', 'New Mexico': 'NM', 'New York': 'NY',
  'North Carolina': 'NC', 'North Dakota': 'ND', 'Ohio': 'OH', 'Oklahoma': 'OK',
  'Oregon': 'OR', 'Pennsylvania': 'PA', 'Rhode Island': 'RI', 'South Carolina': 'SC',
  'South Dakota': 'SD', 'Tennessee': 'TN', 'Texas': 'TX', 'Utah': 'UT',
  'Vermont': 'VT', 'Virginia': 'VA', 'Washington': 'WA', 'West Virginia': 'WV',
  'Wisconsin': 'WI', 'Wyoming': 'WY', 'District of Columbia': 'DC',
  // Canadian provinces and territories
  'Alberta': 'AB', 'British Columbia': 'BC', 'Manitoba': 'MB',
  'New Brunswick': 'NB', 'Newfoundland': 'NL', 'Newfoundland and Labrador': 'NL',
  'Northwest Territories': 'NT', 'Nova Scotia': 'NS', 'Nunavut': 'NU',
  'Ontario': 'ON', 'Prince Edward Island': 'PE', 'Quebec': 'QC',
  'Saskatchewan': 'SK', 'Yukon': 'YT',
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function isValid(val) {
  if (val === null || val === undefined) return false;
  const s = String(val).trim().toLowerCase();
  return s !== '' && s !== 'n/a' && s !== 'na' && s !== 'unknown' && s !== 'none';
}

function slugify(str) {
  return str
    .toLowerCase()
    .normalize('NFD').replace(/[̀-ͯ]/g, '')  // strip accents
    .replace(/[^a-z0-9\s-]/g, '')
    .trim()
    .replace(/\s+/g, '-')
    .replace(/-+/g, '-');
}

// Returns { groomSkate, groomClassic, groomType }
// groomType: 'both' | 'classic' | 'skate' | 'yes' | 'unknown'
function normalizeGrooming(val) {
  if (!isValid(val)) return { groomSkate: null, groomClassic: null, groomType: 'unknown' };
  const v = val.trim().toLowerCase();
  if (v === 'unknown') return { groomSkate: null, groomClassic: null, groomType: 'unknown' };

  const hasBoth = (v.indexOf('skate') > -1 && v.indexOf('classic') > -1)
    || v === 'skating/classical'
    || v === 'classic and skate';
  if (hasBoth) return { groomSkate: true, groomClassic: true, groomType: 'both' };

  const isClassic = v === 'classic' || v === 'classic only';
  if (isClassic) return { groomSkate: false, groomClassic: true, groomType: 'classic' };

  const isSkate = v === 'skate' || v === 'skate only';
  if (isSkate) return { groomSkate: true, groomClassic: false, groomType: 'skate' };

  // "Yes" — groomed but type unknown
  if (v === 'yes') return { groomSkate: null, groomClassic: null, groomType: 'yes' };

  return { groomSkate: null, groomClassic: null, groomType: 'unknown' };
}

// Returns km as a number, or null
function getLengthKm(length, units) {
  if (!isValid(length)) return null;
  const len = parseFloat(String(length).replace(/,/g, ''));
  if (isNaN(len) || len <= 0) return null;
  if (!isValid(units)) return null;
  const u = String(units).trim().toLowerCase();
  if (u === 'km' || u === 'km') return Math.round(len * 10) / 10;
  if (u === 'mi' || u === 'm') return Math.round(len * 1.60934 * 10) / 10;
  return null;
}

// Build the region string: "City, ST" or just "ST" or state full name as fallback
function buildRegion(city, stateOrProvince) {
  const abbr = STATE_ABBR[stateOrProvince] || stateOrProvince || null;
  if (city && isValid(city) && abbr) return `${city.trim()}, ${abbr}`;
  if (abbr) return abbr;
  return null;
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

const raw = JSON.parse(fs.readFileSync(INPUT, 'utf8'));
const usedIds = new Set();
const needsRegionReview = [];

const features = raw.features
  .map(f => {
    const p = f.properties;

    // --- Name ---
    // New-format features use 'Name'; old-format use 'Ski Area Name'
    const rawName = isValid(p.Name) ? p.Name.trim()
      : isValid(p['Ski Area Name']) ? p['Ski Area Name'].trim()
      : null;
    if (!rawName) return null; // drop nameless features

    // --- Region ---
    let region;
    if (isValid(p['State/Province'])) {
      // New-format: state/province only (no city in data)
      region = buildRegion(null, p['State/Province']);
    } else if (isValid(p.State)) {
      // Old-format: city + state (may be full name or abbreviation)
      const city = isValid(p.City) ? p.City.trim() : null;
      const st   = STATE_ABBR[p.State.trim()] || p.State.trim();
      region = city ? `${city}, ${st}` : st;
    } else {
      region = null;
    }

    if (!region) needsRegionReview.push(rawName);

    // --- Grooming ---
    let groomRaw = p.Groomed;
    // Old-format features: 'Groomed trails' is a boolean, type unknown
    if (!isValid(groomRaw) && p['Groomed trails'] === true) groomRaw = 'Yes';
    const { groomSkate, groomClassic, groomType } = normalizeGrooming(groomRaw);

    // --- Trail lengths ---
    let totalKm;
    if (isValid(p.Length) && isValid(p.Units)) {
      totalKm = getLengthKm(p.Length, p.Units);
    } else if (p['Kilometers of groomed trail'] != null) {
      // Old-format: already in km
      const v = parseFloat(p['Kilometers of groomed trail']);
      totalKm = isNaN(v) ? null : Math.round(v * 10) / 10;
    } else {
      totalKm = null;
    }

    // kmSkate and kmClassic: only set when we know the grooming type exclusively
    let kmSkate   = null;
    let kmClassic = null;

    if (groomType === 'skate' && totalKm !== null) {
      kmSkate = totalKm;
    } else if (groomType === 'classic' && totalKm !== null) {
      kmClassic = totalKm;
    }
    // 'both', 'yes', 'unknown': split is unknown — leave as null for manual entry

    // km = max(kmSkate, kmClassic) when both known; otherwise whichever is known; fallback to totalKm
    let km;
    if (kmSkate !== null && kmClassic !== null) {
      km = Math.max(kmSkate, kmClassic);
    } else if (kmSkate !== null) {
      km = kmSkate;
    } else if (kmClassic !== null) {
      km = kmClassic;
    } else {
      km = totalKm; // total trail length when type split is unknown
    }

    // --- ID ---
    const stateAbbr = (() => {
      if (isValid(p['State/Province'])) return (STATE_ABBR[p['State/Province']] || slugify(p['State/Province'])).toLowerCase();
      if (isValid(p.State)) return (STATE_ABBR[p.State.trim()] || p.State.trim()).toLowerCase();
      return '';
    })();
    const baseSlug = slugify(rawName) + (stateAbbr ? '-' + stateAbbr : '');
    let id = baseSlug;
    let counter = 2;
    while (usedIds.has(id)) { id = baseSlug + '-' + counter++; }
    usedIds.add(id);

    // --- URL validation ---
    let url = null;
    const rawUrl = p.URL || p['Google Maps URL'] || p['Wesbite'] || p.Website || null;
    if (rawUrl && /^https?:\/\//i.test(String(rawUrl).trim())) {
      url = String(rawUrl).trim();
    }

    // --- Notes ---
    const notes = isValid(p.Notes) ? p.Notes.trim() : null;

    return {
      type: 'Feature',
      properties: {
        id,
        Name: rawName,
        region: region || null,
        groomSkate,
        groomClassic,
        groomType,
        Groomed: groomRaw || null,   // keep original for reference
        km,
        kmSkate,
        kmClassic,
        Snowmaking: isValid(p.Snowmaking) && p.Snowmaking === 'Yes' ? 'Yes'
          : isValid(p.Snowmaking) && p.Snowmaking === 'No' ? 'No' : null,
        Lit: isValid(p.Lit) && p.Lit === 'Yes' ? 'Yes'
          : isValid(p.Lit) && p.Lit === 'No' ? 'No' : null,
        URL: url,
        Notes: notes,
        // Future fields — no data yet
        passPrice: null,
        rentals: null,
        lessons: null,
        dogs: null,
        photo: null,
        groomingUrl: null,
      },
      geometry: f.geometry,
    };
  })
  .filter(Boolean);

const output = {
  type: 'FeatureCollection',
  features,
};

fs.writeFileSync(OUTPUT, JSON.stringify(output, null, 2));

console.log(`Written: ${OUTPUT}`);
console.log(`Features: ${features.length} (dropped ${raw.features.length - features.length} nameless)`);

// Summary stats
const withKm          = features.filter(f => f.properties.km !== null).length;
const withKmSkate     = features.filter(f => f.properties.kmSkate !== null).length;
const withKmClassic   = features.filter(f => f.properties.kmClassic !== null).length;
const groomBoth       = features.filter(f => f.properties.groomType === 'both').length;
const groomClassic    = features.filter(f => f.properties.groomType === 'classic').length;
const groomSkate      = features.filter(f => f.properties.groomType === 'skate').length;
const groomYes        = features.filter(f => f.properties.groomType === 'yes').length;
const groomUnknown    = features.filter(f => f.properties.groomType === 'unknown').length;
const withRegion      = features.filter(f => f.properties.region !== null).length;

console.log(`\nGrooming breakdown:`);
console.log(`  Both skate+classic : ${groomBoth}`);
console.log(`  Classic only       : ${groomClassic}`);
console.log(`  Skate only         : ${groomSkate}`);
console.log(`  Yes (type unknown) : ${groomYes}`);
console.log(`  Unknown            : ${groomUnknown}`);
console.log(`\nTrail length:`);
console.log(`  With total km      : ${withKm}`);
console.log(`  With kmSkate       : ${withKmSkate}`);
console.log(`  With kmClassic     : ${withKmClassic}`);
console.log(`\nRegion:`);
console.log(`  With region        : ${withRegion}`);
console.log(`  Missing region     : ${needsRegionReview.length}`);
if (needsRegionReview.length) {
  console.log('  Areas needing region review:');
  needsRegionReview.forEach(n => console.log('    -', n));
}
