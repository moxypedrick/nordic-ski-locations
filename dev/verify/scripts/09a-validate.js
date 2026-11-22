// Step 9a: validate every record in dev/verify/results/.
// Prints failures, duplicates, and the count of ids with no record.
// Exit code 1 if anything fails or any id is missing.
// Usage: node dev/verify/scripts/09a-validate.js [--progress]
//   --progress  only print how many ids have a record (for use during the run)
const fs = require('fs');
const path = require('path');
const { loadJson } = require('./lib/check-url');

const ROOT = path.resolve(__dirname, '../../..');
const RES = path.join(ROOT, 'dev/verify/results');
const areas = loadJson(path.join(ROOT, 'dev/verify/work/areas.json'), []);
const areaIds = new Set(areas.map(a => a.id));

const DIRECTORY = [
  /skinnyski\.com/i, /canadatrails\.ca/i, /paccsa\.org/i, /xcski\.org/i, /trailforks\.com/i, /alltrails\.com/i,
  /tripadvisor\./i, /yelp\./i, /wikipedia\.org/i, /openskimap\.org/i, /skimap\.org/i, /nordicskireport/i,
  /google\.[a-z.]+\/maps/i, /maps\.google\./i, /goo\.gl\/maps/i, /maps\.app\.goo\.gl/i,
];
const STATUS = ['operating', 'closed', 'unverified'];
const CONF = ['high', 'medium', 'low'];
const METHOD = ['skinnyski-report', 'site-check', 'research'];
const KEYS = ['id', 'status', 'confidence', 'officialUrl', 'evidence', 'suggestedName', 'method', 'checkedDate', 'comment'];

function check(r) {
  const e = [];
  for (const k of KEYS) if (!(k in r)) e.push(`missing field ${k}`);
  if (!areaIds.has(r.id)) e.push('id not in areas.json');
  if (!STATUS.includes(r.status)) e.push(`bad status ${r.status}`);
  if (!CONF.includes(r.confidence)) e.push(`bad confidence ${r.confidence}`);
  if (r.officialUrl !== null && !(typeof r.officialUrl === 'string' && /^http/.test(r.officialUrl))) e.push('bad officialUrl');
  if (r.officialUrl && DIRECTORY.some(d => d.test(r.officialUrl))) e.push(`officialUrl is a directory site: ${r.officialUrl}`);
  if (!Array.isArray(r.evidence)) e.push('evidence not a list');
  else {
    if (r.status !== 'unverified' && !r.evidence.some(v => v && typeof v.url === 'string' && /^http/.test(v.url))) e.push(`${r.status} without evidence url`);
    for (const v of r.evidence) {
      if (v.date !== null && !/^\d{4}-\d{2}-\d{2}$/.test(v.date || '')) e.push(`bad evidence date ${v.date}`);
      const words = String(v.summary || '').trim().split(/\s+/).filter(Boolean).length;
      if (words > 20) e.push(`summary ${words} words`);
    }
  }
  if (r.suggestedName !== null && typeof r.suggestedName !== 'string') e.push('bad suggestedName');
  if (!METHOD.includes(r.method)) e.push(`bad method ${r.method}`);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(r.checkedDate || '')) e.push('bad checkedDate');
  if (typeof r.comment !== 'string') e.push('comment not a string');
  return e;
}

const seen = {};
const failures = [];
const dups = [];
for (const f of fs.readdirSync(RES).filter(f => f.endsWith('.json')).sort()) {
  let list;
  try { list = JSON.parse(fs.readFileSync(path.join(RES, f), 'utf8')); } catch (err) { failures.push(`${f}: cannot parse (${err.message})`); continue; }
  if (!Array.isArray(list)) { failures.push(`${f}: not a list`); continue; }
  for (const r of list) {
    const errs = check(r);
    if (errs.length) failures.push(`${f} ${r.id}: ${errs.join('; ')}`);
    if (seen[r.id]) {
      const keep = seen[r.id].r.method === 'research' ? seen[r.id] : (r.method === 'research' ? { f, r } : seen[r.id]);
      dups.push(`${r.id} in ${seen[r.id].f} and ${f} (keeping ${keep.f})`);
      seen[r.id] = keep;
    } else seen[r.id] = { f, r };
  }
}
const missing = areas.filter(a => !seen[a.id]).map(a => a.id);

if (process.argv.includes('--progress')) {
  console.log(`records ${Object.keys(seen).length}/${areas.length}, missing ${missing.length}, failures ${failures.length}`);
  process.exit(0);
}
console.log('Records:', Object.keys(seen).length, '/', areas.length);
console.log('Failures:', failures.length); failures.forEach(x => console.log('  ' + x));
console.log('Duplicates:', dups.length); dups.forEach(x => console.log('  ' + x));
console.log('Ids with no record:', missing.length); if (missing.length <= 50) missing.forEach(x => console.log('  ' + x));
process.exit(failures.length || missing.length ? 1 : 0);
