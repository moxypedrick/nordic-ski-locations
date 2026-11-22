/**
 * apply-verification.js
 *
 * Applies data/verification.json (status and official URL for each area,
 * from the data cleaning run in dev/verify/) on top of
 * nordic-ski-areas-merged.geojson. Modelled on apply-overrides.js.
 *
 * For each id in verification.json:
 *   - verifyStatus = status, verifiedDate = checkedDate
 *   - officialUrl set and different from URL: sourceUrl = old URL, URL = officialUrl
 *   - officialUrl null and URL is a canadatrails.ca page: sourceUrl = URL, URL = null
 *   - officialUrl null, any other URL: URL unchanged
 *   - status closed and approved (dev/verify/work/closure-approvals.json):
 *       adminStatus = 'closed'
 *   - status closed and not approved: verifyStatus = 'unverified', adminStatus untouched
 * No other property and no geometry is changed. suggestedName is not applied.
 * Running it twice gives the same result.
 *
 * Usage: node dev/scripts/apply-verification.js
 * Also used by merge-skimap.js, which calls applyVerification() before
 * applyOverrides() so that manual overrides still win.
 */

'use strict';

const fs   = require('fs');
const path = require('path');

const REPO_ROOT          = path.join(__dirname, '..', '..');
const MERGED_FILE        = path.join(REPO_ROOT, 'data', 'nordic-ski-areas-merged.geojson');
const VERIFICATION_FILE  = path.join(REPO_ROOT, 'data', 'verification.json');
const APPROVALS_FILE     = path.join(REPO_ROOT, 'dev', 'verify', 'work', 'closure-approvals.json');

function isCanadaTrails(url) {
  return typeof url === 'string' && url.includes('canadatrails.ca');
}

function applyVerification(gj, verificationFile = VERIFICATION_FILE, approvalsFile = APPROVALS_FILE) {
  const stats = { applied: 0, missing: [], urlChanged: 0, urlCleared: 0, closed: 0, closedNotApproved: 0 };
  if (!fs.existsSync(verificationFile)) return stats;

  const records = JSON.parse(fs.readFileSync(verificationFile, 'utf8'));
  const approvals = fs.existsSync(approvalsFile)
    ? JSON.parse(fs.readFileSync(approvalsFile, 'utf8'))
    : { approved: [] };
  const approved = new Set(approvals.approved || []);
  const byId = new Map(gj.features.map(f => [f.properties.id, f]));

  for (const [id, rec] of Object.entries(records)) {
    const feature = byId.get(id);
    if (!feature) { stats.missing.push(id); continue; }
    const p = feature.properties;

    p.verifyStatus = rec.status;
    p.verifiedDate = rec.checkedDate;

    if (rec.officialUrl) {
      if (rec.officialUrl !== p.URL) {
        if (p.URL && !p.sourceUrl) p.sourceUrl = p.URL;
        p.URL = rec.officialUrl;
        stats.urlChanged++;
      }
    } else if (isCanadaTrails(p.URL)) {
      if (!p.sourceUrl) p.sourceUrl = p.URL;
      p.URL = null;
      stats.urlCleared++;
    }

    if (rec.status === 'closed') {
      if (approved.has(id)) {
        p.adminStatus = 'closed';
        stats.closed++;
      } else {
        p.verifyStatus = 'unverified';
        stats.closedNotApproved++;
      }
    }
    stats.applied++;
  }
  return stats;
}

module.exports = { applyVerification };

if (require.main === module) {
  if (!fs.existsSync(VERIFICATION_FILE)) {
    console.log('No data/verification.json found. Nothing to apply.');
    process.exit(0);
  }
  console.log(`Loading ${MERGED_FILE}...`);
  const gj = JSON.parse(fs.readFileSync(MERGED_FILE, 'utf8'));
  const s = applyVerification(gj);
  fs.writeFileSync(MERGED_FILE, JSON.stringify(gj));
  console.log(`Applied ${s.applied} verification record(s).`);
  console.log(`  URL replaced with official URL: ${s.urlChanged}`);
  console.log(`  canadatrails URL cleared (no official URL): ${s.urlCleared}`);
  console.log(`  Marked closed (approved): ${s.closed}`);
  console.log(`  Closed but not approved, set to unverified: ${s.closedNotApproved}`);
  if (s.missing.length) console.log(`  Ids not found in the merged file: ${s.missing.length}: ${s.missing.join(', ')}`);
}
