// Step 7: sort every area into a bucket (auto / site-check / research).
// Reads the work files from Steps 1 to 6, writes dev/verify/work/triage.json
// and, for bucket auto, dev/verify/results/auto.json.
//
// Usage: node dev/verify/scripts/07-triage.js
const path = require('path');
const { saveJson, loadJson } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const W = f => path.join(ROOT, 'dev/verify/work', f);
const TODAY = new Date().toISOString().slice(0, 10);

const areas = loadJson(W('areas.json'), []);
const other = loadJson(W('linkcheck-other.json'), {});
const sk = loadJson(W('skinnyski.json'), {});
const ct = loadJson(W('canadatrails.json'), {});
const pa = loadJson(W('paccsa.json'), {});

const live = s => s === 'ok' || s === 'moved';

function candidateFor(a) {
  // returns { url, check, hint }
  if (a.group === 'skinnyski') {
    const r = sk[a.id] || {};
    const hint = r.listingMissing ? 'skinnyski listing missing' : 'skinnyski: no reports found (last season page blocked)';
    return { url: r.officialLink || null, check: r.linkCheck || null, hint, listingMissing: !!r.listingMissing, sk: r };
  }
  if (a.group === 'canadatrails') {
    const r = ct[a.id] || {};
    const hint = r.pageFetchFailed ? 'canadatrails page not fetched' : `canadatrails reviewed ${r.lastReviewed || 'unknown'}${r.fromArchive ? ' (archive)' : ''}`;
    return { url: r.websiteLink || null, check: r.linkCheck || null, hint };
  }
  if (a.group === 'paccsa') {
    const r = pa[a.id] || {};
    const c = (r.candidateLinks || []).find(x => live(x.linkCheck.state) && x.linkCheck.mentionsSkiing);
    const hint = r.blocked ? 'paccsa page blocked (403)' : r.fetchFailed ? `paccsa page HTTP ${r.httpStatus}` : `paccsa: ${(r.candidateLinks || []).length} external links`;
    return c ? { url: c.url, check: c.linkCheck, hint } : { url: null, check: null, hint };
  }
  if (a.group === 'other') return { url: a.url, check: other[a.id] || null, hint: 'own URL' };
  return { url: null, check: null, hint: a.source === 'skimap' ? 'no URL (skimap only)' : 'no URL' };
}

const triage = [];
const auto = [];
for (const a of areas) {
  const c = candidateFor(a);
  const state = c.url && c.check ? c.check.state : null;
  const candidateUrl = c.url && live(state) && c.check.finalUrl ? c.check.finalUrl : c.url;
  let bucket;
  if (a.group === 'skinnyski' && c.sk && (c.sk.reportsThisSeason || c.sk.reportsLastSeason) && live(state)) bucket = 'auto';
  else if (live(state) && c.check.mentionsSkiing && !c.listingMissing) bucket = 'site-check';
  else bucket = 'research';

  let hint = c.hint;
  if (c.url) hint += `; link ${state || 'unchecked'}`;
  if (live(state) && !c.check.mentionsSkiing) hint += ' (no ski mention)';
  triage.push({ id: a.id, bucket, candidateUrl: candidateUrl || null, candidateState: state, hint });

  if (bucket === 'auto') {
    const r = c.sk;
    const high = r.reportsThisSeason || (r.latestReportDate && r.latestReportDate >= '2025-10-01');
    auto.push({
      id: a.id, status: 'operating', confidence: high ? 'high' : 'medium', officialUrl: candidateUrl,
      evidence: [{ url: `https://www.skinnyski.com/trails/traildetail.asp?Id=${r.skinnyskiId}`, date: r.latestReportDate || null, summary: 'Skier trail report on skinnyski.com.' }],
      suggestedName: null, method: 'skinnyski-report', checkedDate: TODAY, comment: '',
    });
  }
}

saveJson(W('triage.json'), triage);
saveJson(path.join(ROOT, 'dev/verify/results/auto.json'), auto);
const counts = {};
for (const t of triage) counts[t.bucket] = (counts[t.bucket] || 0) + 1;
console.log('Items:', triage.length);
console.log('Buckets:', counts);
const byGroup = {};
for (const a of areas) { const t = triage.find(x => x.id === a.id); const k = `${a.group}/${t.bucket}`; byGroup[k] = (byGroup[k] || 0) + 1; }
console.log('By group:', byGroup);
