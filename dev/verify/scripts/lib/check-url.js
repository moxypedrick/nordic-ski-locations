// Shared link checker for the data verification scripts (plan Step 2).
// Plain Node 18+, no packages. Exports checkUrl(url) plus small helpers
// (fetchPage, visibleText, sleep, saveJson) used by the step scripts.
//
// Concurrency: at most 4 requests at once across the whole process, and
// never 2 at once to the same host. Every request made through this module
// goes through that limiter.

const fs = require('fs');
const path = require('path');

const USER_AGENT = 'Mozilla/5.0 (compatible; trail-link-check)';
const TIMEOUT_MS = 20000;
const RETRY_WAIT_MS = 5000;
const MAX_CONCURRENT = 4;
const MAX_BODY_BYTES = 3 * 1024 * 1024;

// ---------- limiter ----------
let active = 0;
const busyHosts = new Set();
const waiting = []; // { host, resolve }

function hostOf(url) {
  try { return new URL(url).hostname.toLowerCase(); } catch { return ''; }
}

function pump() {
  for (let i = 0; i < waiting.length && active < MAX_CONCURRENT; i++) {
    const w = waiting[i];
    if (busyHosts.has(w.host)) continue;
    waiting.splice(i, 1);
    i--;
    active++;
    busyHosts.add(w.host);
    w.resolve();
  }
}

function acquire(host) {
  return new Promise(resolve => { waiting.push({ host, resolve }); pump(); });
}

function release(host) {
  active--;
  busyHosts.delete(host);
  pump();
}

async function limited(url, fn) {
  const host = hostOf(url);
  await acquire(host);
  try { return await fn(); } finally { release(host); }
}

// ---------- helpers ----------
const sleep = ms => new Promise(r => setTimeout(r, ms));

function saveJson(file, data) {
  fs.mkdirSync(path.dirname(file), { recursive: true });
  const tmp = file + '.tmp';
  const text = JSON.stringify(data, null, 1);
  fs.writeFileSync(tmp, text);
  // On Windows the rename can fail briefly with EPERM/EBUSY while another
  // process (indexer, antivirus) has the target open. Retry, then fall back
  // to a direct write.
  for (let i = 0; i < 10; i++) {
    try { fs.renameSync(tmp, file); return; } catch (e) {
      if (e.code !== 'EPERM' && e.code !== 'EBUSY' && e.code !== 'EACCES') throw e;
      const until = Date.now() + 50 * (i + 1);
      while (Date.now() < until) { /* short busy wait */ }
    }
  }
  fs.writeFileSync(file, text);
  try { fs.unlinkSync(tmp); } catch {}
}

function loadJson(file, fallback) {
  try { return JSON.parse(fs.readFileSync(file, 'utf8')); } catch { return fallback; }
}

const ENTITIES = { amp: '&', lt: '<', gt: '>', quot: '"', apos: "'", nbsp: ' ', eacute: 'é', egrave: 'è', agrave: 'à', ccedil: 'ç', ocirc: 'ô', ecirc: 'ê' };
function decodeEntities(s) {
  return s.replace(/&(#x[0-9a-f]+|#\d+|[a-z]+);/gi, (m, e) => {
    if (e[0] === '#') {
      const n = e[1] === 'x' || e[1] === 'X' ? parseInt(e.slice(2), 16) : parseInt(e.slice(1), 10);
      try { return String.fromCodePoint(n); } catch { return m; }
    }
    const v = ENTITIES[e.toLowerCase()];
    return v !== undefined ? v : m;
  });
}

function visibleText(html) {
  return decodeEntities(
    html
      .replace(/<!--[\s\S]*?-->/g, ' ')
      .replace(/<(script|style|noscript|template|svg|head)\b[\s\S]*?<\/\1>/gi, ' ')
      .replace(/<[^>]+>/g, ' ')
  ).replace(/\s+/g, ' ').trim();
}

function titleOf(html) {
  const m = html.match(/<title[^>]*>([\s\S]*?)<\/title>/i);
  return m ? decodeEntities(m[1]).replace(/\s+/g, ' ').trim() : null;
}

async function readBody(res) {
  // Read at most MAX_BODY_BYTES, decode as UTF-8.
  const reader = res.body && res.body.getReader ? res.body.getReader() : null;
  if (!reader) return await res.text();
  const chunks = [];
  let total = 0;
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    chunks.push(value);
    total += value.length;
    if (total > MAX_BODY_BYTES) { try { await reader.cancel(); } catch {} break; }
  }
  return Buffer.concat(chunks.map(c => Buffer.from(c))).toString('utf8');
}

// One GET with timeout. Returns { ok, status, finalUrl, html } or { ok:false, error }.
async function fetchOnce(url) {
  return limited(url, async () => {
    const ctrl = new AbortController();
    const timer = setTimeout(() => ctrl.abort(), TIMEOUT_MS);
    try {
      const res = await fetch(url, {
        method: 'GET',
        redirect: 'follow',
        signal: ctrl.signal,
        headers: { 'User-Agent': USER_AGENT, 'Accept': 'text/html,application/xhtml+xml,*/*;q=0.8' },
      });
      const html = await readBody(res);
      return { ok: true, status: res.status, finalUrl: res.url || url, html };
    } catch (err) {
      const cause = err && err.cause ? (err.cause.code || err.cause.message) : '';
      return { ok: false, error: (err.name === 'AbortError' ? 'timeout' : (err.message || String(err))) + (cause ? ' (' + cause + ')' : '') };
    } finally {
      clearTimeout(timer);
    }
  });
}

// GET with one retry on timeout / network error / 5xx (after 5 s).
async function fetchPage(url) {
  let r = await fetchOnce(url);
  if (!r.ok || r.status >= 500) {
    await sleep(RETRY_WAIT_MS);
    r = await fetchOnce(url);
  }
  return r;
}

// ---------- classification ----------
const PARKED_PHRASES = ['domain is for sale', 'buy this domain', 'this domain may be for sale', 'domain parking', 'hugedomains', 'dan.com'];

function isParked(lower) {
  if (PARKED_PHRASES.some(p => lower.includes(p))) return true;
  if (/\bsedo\b/.test(lower)) return true;
  if (lower.includes('godaddy') && lower.includes('parked')) return true;
  return false;
}

function mentionsSkiing(lower) {
  const words = ['ski', 'nordic', 'cross-country', 'cross country', 'xc ', 'ski de fond', 'groom', 'damé', 'damage des pistes'];
  if (words.some(w => lower.includes(w))) return true;
  return lower.includes('raquette') && lower.includes('piste');
}

function stripHost(h) { return h.replace(/^www\./, ''); }

function classify(inputUrl, r) {
  const out = { inputUrl, finalUrl: null, httpStatus: null, state: null, title: null, mentionsSkiing: false, textSample: '' };
  if (!r.ok) { out.state = 'dead'; out.error = r.error; return out; }
  out.finalUrl = r.finalUrl;
  out.httpStatus = r.status;
  const text = visibleText(r.html || '');
  const lower = text.toLowerCase();
  out.title = titleOf(r.html || '');
  out.mentionsSkiing = mentionsSkiing(lower);
  out.textSample = text.slice(0, 500);
  const s = r.status;
  if (s === 404 || s === 410 || s >= 500) out.state = 'dead';
  else if (s === 401 || s === 403 || s === 429) out.state = 'blocked';
  else if (s === 200 && isParked(lower)) out.state = 'parked';
  else if (s === 200 && stripHost(hostOf(inputUrl)) !== stripHost(hostOf(r.finalUrl))) out.state = 'moved';
  else if (s === 200) out.state = 'ok';
  else out.state = 'other'; // status the plan's table does not cover (e.g. 400, 406, 999)
  return out;
}

// ---------- public ----------
async function checkUrl(url) {
  url = String(url).trim();
  const tried = [];
  if (/^http:\/\//i.test(url)) {
    const httpsUrl = 'https://' + url.slice(7);
    const r1 = await fetchPage(httpsUrl);
    const c1 = classify(url, r1);
    tried.push(httpsUrl);
    if (c1.state !== 'dead') { c1.triedUrls = tried; return c1; }
  }
  const r2 = await fetchPage(url);
  const c2 = classify(url, r2);
  tried.push(url);
  c2.triedUrls = tried;
  return c2;
}

// Run checkUrl over many items with the limiter handling concurrency.
// items: [{ key, url }]. onResult(key, result) is called as each finishes.
async function checkMany(items, onResult) {
  await Promise.all(items.map(async it => {
    const res = await checkUrl(it.url);
    await onResult(it.key, res);
  }));
}

module.exports = { checkUrl, checkMany, fetchPage, visibleText, titleOf, decodeEntities, sleep, saveJson, loadJson, hostOf, USER_AGENT };
