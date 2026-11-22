// Step 10: apply suggested renames from data/verification.json.
//
// A rename is applied automatically only when the record is:
//   - status "operating", confidence "high" or "medium", and
//   - free of hedging words (tentative, guess, possibly, likely, probably,
//     unconfirmed, "not confirmed", "?") in its comment or suggested name, and
//   - an actual change (the suggested name differs from the current name).
// Applied renames are written to data/manual-overrides.json as a Name
// override, so a rebuild with merge-skimap.js keeps them and admin.html shows
// the area as edited. To undo one, delete its Name from manual-overrides.json
// (or edit the name in admin.html).
//
// Every suggested rename, applied or not, is listed in
// dev/verify/renames.csv for later review.
//
// Usage: node dev/verify/scripts/10-apply-renames.js
//        then: node dev/scripts/apply-overrides.js
const fs = require('fs');
const path = require('path');

const ROOT = path.resolve(__dirname, '../../..');
const VER = path.join(ROOT, 'data/verification.json');
const OVR = path.join(ROOT, 'data/manual-overrides.json');
const MERGED = path.join(ROOT, 'data/nordic-ski-areas-merged.geojson');
const CSV = path.join(ROOT, 'dev/verify/renames.csv');

const HEDGE = /tentative|guess|possibl|unconfirm|not confirm|likely|probabl|\?/i;

const ver = JSON.parse(fs.readFileSync(VER, 'utf8'));
const overrides = fs.existsSync(OVR) ? JSON.parse(fs.readFileSync(OVR, 'utf8')) : {};
const merged = JSON.parse(fs.readFileSync(MERGED, 'utf8'));
const nameById = new Map(merged.features.map(f => [f.properties.id, f.properties.Name]));

const rows = [];
const counts = { applied: 0, skipped: 0 };
for (const [id, r] of Object.entries(ver)) {
  if (!r.suggestedName) continue;
  const oldName = nameById.has(id) ? nameById.get(id) : null;
  const ov = overrides[id];
  let decision;
  if (oldName === null || (ov && ov.deleted)) decision = 'not applied: area deleted';
  else if (r.suggestedName.trim() === String(oldName).trim()) decision = 'not applied: same name';
  else if (ov && ov.properties && 'Name' in ov.properties && ov.properties.Name !== r.suggestedName) decision = 'not applied: name already edited by hand';
  else if (r.status !== 'operating') decision = `check first: status ${r.status}`;
  else if (r.confidence === 'low') decision = 'check first: low confidence';
  else if (HEDGE.test(`${r.comment} ${r.suggestedName}`)) decision = 'check first: research was unsure';
  else decision = 'applied';

  if (decision === 'applied') {
    const entry = ov && !ov.deleted ? ov : {};
    entry.properties = Object.assign(entry.properties || {}, { Name: r.suggestedName });
    entry._renamedFrom = oldName;
    overrides[id] = entry;
    counts.applied++;
  } else counts.skipped++;

  const ev = (r.evidence || [])[0] || {};
  rows.push({ decision, id, oldName, newName: r.suggestedName, status: r.status, confidence: r.confidence, officialUrl: r.officialUrl, evidenceUrl: ev.url, evidenceSummary: ev.summary, comment: r.comment });
}

fs.writeFileSync(OVR, JSON.stringify(overrides, null, 2));

const cell = v => { const s = v == null ? '' : String(v); return /[",\n\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s; };
const cols = ['decision', 'id', 'oldName', 'newName', 'status', 'confidence', 'officialUrl', 'evidenceUrl', 'evidenceSummary', 'comment'];
rows.sort((a, b) => (a.decision === 'applied') - (b.decision === 'applied') || a.decision.localeCompare(b.decision));
fs.writeFileSync(CSV, '﻿' + [cols.join(',')].concat(rows.map(r => cols.map(c => cell(r[c])).join(','))).join('\r\n') + '\r\n');

const byDecision = rows.reduce((m, r) => (m[r.decision] = (m[r.decision] || 0) + 1, m), {});
console.log('Suggested renames:', rows.length);
console.log(byDecision);
console.log(`Wrote ${OVR} and ${CSV}`);
