# Nordic Ski Centers — Feature Implementation Plan
## Steps 10–14: Favorites, Recent Snow, Trail Lines, and Grooming Status

---

## Project Context

**Single file app.** All HTML, CSS, and JS live in `index.html` (~3,200 lines). No build tool, no bundler. ES5-compatible JS throughout (no arrow functions, no `const`/`let` outside of where they already exist, no template literals unless already present). All changes go in this one file.

**Mapbox GL JS v1.12.0** (`mapbox://styles/mapbox/outdoors-v11`). The map is initialized as:
```js
var map = new mapboxgl.Map({
  container: 'map',
  style: 'mapbox://styles/mapbox/outdoors-v11',
  center: [-98, 38.88],
  minZoom: 1,
  zoom: 3
});
```

**GeoJSON data** loaded from `data/nordic-ski-areas-v2.geojson` — 1,273 nordic ski area Point features. Relevant property fields:
```json
{
  "id": "kincaid-park-ak",
  "Name": "Kincaid Park",
  "region": "Anchorage, AK",
  "groomSkate": null,
  "groomClassic": null,
  "groomType": "yes",
  "Groomed": "Yes",
  "km": 69,
  "kmSkate": null,
  "kmClassic": null,
  "Snowmaking": null,
  "Lit": null,
  "URL": "https://...",
  "Notes": null,
  "passPrice": null,
  "rentals": null,
  "lessons": null,
  "dogs": null,
  "photo": null,
  "groomingUrl": null
}
```
`groomType` values: `"both"` | `"skate"` | `"classic"` | `"yes"` | `null`

**Global state object** (defined around line 1487):
```js
var state = {
  location: null,         // { lnglat: [lng, lat], label: string }
  filters: {
    skate: false,
    classic: false,
    snow: false,
    lit: false,
    kmMin: 0,
    kmMax: 100
  },
  sort: 'name',
  sync: true,
  selectedId: null
};
```

**Key global vars**: `allFeatures` (all 1273), `visibleFeatures` (in current map bounds), `fullGeoJSON`, `dataLoaded` (bool), `popup` (Mapbox popup instance).

**Key functions**:
- `applyFilters()` — re-filters list + updates map source. Call this whenever filter state changes.
- `featureMatchesFilters(feature)` — returns bool; add new filter checks here.
- `hasActiveFilters()` — returns bool; add new filter keys here.
- `renderListings(features)` — re-renders the sidebar list. Each card is a `<button class="card">`.
- `updateVisibleFeatures()` — recalculates `visibleFeatures` from map bounds, then calls `applyFilters()`.
- `openDetailPanel(feature)` — opens the detail panel for an area.
- `scheduleUrlSync()` + `buildQueryString()` — URL state sync; add new params here.
- `saveToStorage()` / `loadFromStorage()` — localStorage persistence; add new state keys here.

**Filter bar HTML structure** (around line 1265):
```html
<div class="filter-bar" role="group" aria-label="Filters">
  <div class="chip-wrap">  <!-- Grooming popover -->
    <button class="chip" id="chip-grooming">Grooming ▾</button>
    <div class="popover" id="popover-grooming"> ... </div>
  </div>
  <button class="chip" id="chip-snowmaking">Snowmaking</button>
  <div class="chip-wrap">  <!-- More filters popover -->
    <button class="chip" id="chip-more">More filters ▾</button>
    <div class="popover" id="popover-more"> ... </div>
  </div>
  <button class="chip chip--clear" id="chip-clear" style="display:none">Clear all</button>
</div>
```

**Design tokens** (`:root`, line 19):
```css
--navy: #12355b;   --ink: #12212f;   --muted: #4d5d6c;
--link: #1d5f9e;   --ice: #e8f3fa;   --line: #e3e8ed;
--chip: #f1f4f7;   --warm-bg: #fdf0e3;  --warm-fg: #7a3d06;
--water: #cfe6f3;
--radius-pill: 20px;  --radius-panel: 12px;  --radius-input: 8px;
--shadow-popover: 0 10px 30px rgba(18,33,47,.16);
--shadow-control: 0 2px 8px rgba(18,33,47,.18);
```

**Map layer IDs** currently in use: `cluster-halo`, `clusters`, `cluster-count`, `unclustered-point`, `unclustered-point-hitarea`, `unclustered-point-label`.

**Mobile breakpoint**: `@media (max-width: 760px)`. All mobile CSS is in a single block near the end of the `<style>` section.

---

## Implementation Order

```
Step 10 — Favorites          (independent, ~1 day)
Step 11 — Snow data + filter (independent, ~2 days)
Step 12 — Snow map viz       (depends on Step 11)
Step 13 — Trail lines        (independent, ~1 day)
Step 14 — Grooming status    (depends on Step 13)
```

Steps 10 and 11 can be implemented simultaneously; each is independent.

---

## Step 10 — Favorites

**Goal**: Star any area to save it to localStorage. Toggle a filter to see only favorites. Show a star on map pins for favorited areas.

### State changes

Add to `state`:
```js
state.favorites = false;  // filter: show only favorites
```

Add a top-level var (not inside state, since it's a Set):
```js
var favoriteIds = new Set();  // Set<string> of area IDs; persisted in localStorage
var FAVORITES_KEY = 'nsc:v1:favorites';
```

### localStorage

On page load, read favorites before map loads:
```js
try {
  var raw = localStorage.getItem(FAVORITES_KEY);
  if (raw) {
    var arr = JSON.parse(raw);
    if (Array.isArray(arr)) favoriteIds = new Set(arr);
  }
} catch (e) {}
```

Write on every change:
```js
function saveFavorites() {
  try { localStorage.setItem(FAVORITES_KEY, JSON.stringify(Array.from(favoriteIds))); } catch (e) {}
}
```

### Filter bar HTML

Add a "Favorites" chip after the snowmaking chip:
```html
<button class="chip" id="chip-favorites" type="button" aria-pressed="false">
  ☆ Favorites
</button>
```

### Filter logic

In `featureMatchesFilters()`, add at the top:
```js
if (state.favorites && !favoriteIds.has(prop.id)) return false;
```

In `hasActiveFilters()`, add:
```js
|| state.favorites
```

In `clearFilters()` (find this function and add):
```js
state.favorites = false;
```

### Filter chip JS

```js
var chipFavorites = document.getElementById('chip-favorites');
chipFavorites.addEventListener('click', function () {
  state.favorites = !state.favorites;
  chipFavorites.setAttribute('aria-pressed', state.favorites);
  applyFilters();
});
```

Update `updateFilterChipStates()` to sync this chip's active state.

### Star button — list card

In `renderListings()`, add a star button to each card after the name element:

```js
var starBtn = document.createElement('button');
starBtn.type = 'button';
starBtn.className = 'card__star' + (favoriteIds.has(prop.id) ? ' is-favorited' : '');
starBtn.setAttribute('aria-label', favoriteIds.has(prop.id) ? 'Remove from favorites' : 'Add to favorites');
starBtn.setAttribute('title', starBtn.getAttribute('aria-label'));
starBtn.innerHTML = favoriteIds.has(prop.id) ? '★' : '☆';
starBtn.addEventListener('click', function (e) {
  e.stopPropagation();  // don't open the detail panel
  toggleFavorite(prop.id, starBtn);
});
top.appendChild(starBtn);
```

### Star button — detail panel

In `openDetailPanel()`, add a star button in `.detail-panel__actions` (next to the Directions and Website buttons):
```html
<button class="btn-favorite" id="detail-star-btn" type="button">☆ Save</button>
```
Update it on open: if `favoriteIds.has(feature.properties.id)` show "★ Saved", else "☆ Save".

### Toggle function

```js
function toggleFavorite(id, starEl) {
  if (favoriteIds.has(id)) {
    favoriteIds.delete(id);
  } else {
    favoriteIds.add(id);
  }
  saveFavorites();
  var isFav = favoriteIds.has(id);
  if (starEl) {
    starEl.innerHTML = isFav ? '★' : '☆';
    starEl.className = 'card__star' + (isFav ? ' is-favorited' : '');
    starEl.setAttribute('aria-label', isFav ? 'Remove from favorites' : 'Add to favorites');
  }
  // Re-filter if favorites filter is active
  if (state.favorites) applyFilters();
  // Update map pin
  updateFavoritePin(id);
}
```

### Map pins for favorites

After the map layers are added, add a favorite star symbol layer:
```js
map.addLayer({
  id: 'favorites-star',
  type: 'symbol',
  source: 'nordic-ski-areas',
  filter: ['!', ['has', 'point_count']],
  minzoom: 5,
  layout: {
    'text-field': '★',
    'text-font': ['Arial Unicode MS Bold'],
    'text-size': 14,
    'text-offset': [0, 0],
    'text-anchor': 'center',
    'text-allow-overlap': true
  },
  paint: {
    'text-color': '#f5c842',
    'text-halo-color': '#fff',
    'text-halo-width': 1
  }
});
```

Set a feature filter on this layer to only show favorited IDs:
```js
function updateFavoritePin(id) {
  var favArr = Array.from(favoriteIds);
  var filter = favArr.length
    ? ['in', ['get', 'id'], ['literal', favArr]]
    : ['==', 'id', ''];  // show nothing
  map.setFilter('favorites-star', ['!', ['has', 'point_count'], filter]);
  // Correct syntax:
  if (favArr.length > 0) {
    map.setFilter('favorites-star', ['match', ['get', 'id'], favArr, true, false]);
  } else {
    map.setFilter('favorites-star', ['==', 'id', '___none___']);
  }
}
```

Call `updateFavoritePin()` with no argument to initialize once data loads, and after every `toggleFavorite()`.

### CSS

```css
.card__star {
  background: none;
  border: none;
  font-size: 18px;
  color: var(--muted);
  cursor: pointer;
  padding: 0 0 0 4px;
  line-height: 1;
  flex-shrink: 0;
  transition: color 0.1s;
}
.card__star:hover { color: #f5c842; }
.card__star.is-favorited { color: #f5c842; }
.card__star:focus-visible { outline: 2px solid #6cc0e5; border-radius: 4px; }

.btn-favorite {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  height: 38px;
  padding: 0 18px;
  border-radius: var(--radius-pill);
  font-size: 14px;
  font-weight: 600;
  background: #fff;
  color: var(--navy);
  border: 1px solid var(--line);
  cursor: pointer;
}
.btn-favorite.is-favorited { background: var(--ice); border-color: var(--navy); }
.btn-favorite:hover { border-color: var(--navy); background: var(--ice); }
```

### URL sync

Add `?fav=1` when `state.favorites` is true. Read it on init and set `state.favorites = true`. No need to sync the favorite IDs themselves to the URL (device-local).

---

## Step 11 — Recent Snow: Data + List + Filter

**Goal**: Fetch hourly snowfall and snow depth from Open-Meteo for each area. Show badges on list cards. Add a time window selector and a snow filter with a minimum-inches threshold.

### API

**Open-Meteo** (free, no key, verified working):
```
GET https://api.open-meteo.com/v1/forecast
  ?latitude={lat}
  &longitude={lon}
  &hourly=snowfall,snow_depth
  &past_days=2
  &forecast_days=0
  &timezone=auto
```

Response shape:
```json
{
  "hourly": {
    "time": ["2024-01-15T00:00", "2024-01-15T01:00", ...],
    "snowfall": [0.0, 0.2, 1.1, ...],
    "snow_depth": [0.05, 0.06, ...] 
  }
}
```
- `snowfall`: cm per hour (convert to inches: ÷ 2.54)
- `snow_depth`: meters (convert to inches: × 39.37)
- `past_days=2` gives the last 48 hours plus the current hour
- Array has exactly 48 entries (previous 48 hours up to current hour)

### Time window

Window options: 6h, 12h, 24h, 48h. Default: 24h. Stored in `state.snowWindow = 24`.

To sum snowfall for the last N hours: take the last N entries of `hourly.snowfall` and sum them.
To get snow depth: use `hourly.snow_depth[lastIndex]` (most recent non-null value).

### Snow data cache

```js
var snowCache = {};
// snowCache[areaId] = { fetchedAt: Date.now(), snowfall6h, snowfall12h, snowfall24h, snowfall48h, depthIn }
// or { fetchedAt, error: true } on failure
```

Cache is in-memory per session. Expire after 30 minutes: `Date.now() - entry.fetchedAt > 30 * 60 * 1000`.

### Fetch function

```js
function fetchSnowData(feature, callback) {
  var id = feature.properties.id;
  var coords = feature.geometry.coordinates;
  var lat = coords[1], lon = coords[0];

  // Check cache
  var cached = snowCache[id];
  if (cached && !cached.error && Date.now() - cached.fetchedAt < 30 * 60 * 1000) {
    callback(null, cached);
    return;
  }

  var url = 'https://api.open-meteo.com/v1/forecast'
    + '?latitude=' + lat.toFixed(4)
    + '&longitude=' + lon.toFixed(4)
    + '&hourly=snowfall,snow_depth'
    + '&past_days=2&forecast_days=0&timezone=auto';

  fetch(url)
    .then(function (r) { return r.json(); })
    .then(function (data) {
      var sf = data.hourly.snowfall;   // cm/hr
      var sd = data.hourly.snow_depth; // meters

      // Most recent values are at the end of the arrays
      function sumLast(arr, n) {
        var slice = arr.slice(Math.max(0, arr.length - n));
        return slice.reduce(function (a, b) { return a + (b || 0); }, 0);
      }

      function lastNonNull(arr) {
        for (var i = arr.length - 1; i >= 0; i--) {
          if (arr[i] !== null) return arr[i];
        }
        return 0;
      }

      var entry = {
        fetchedAt: Date.now(),
        snowfall6h:  (sumLast(sf, 6)  / 2.54),   // inches
        snowfall12h: (sumLast(sf, 12) / 2.54),
        snowfall24h: (sumLast(sf, 24) / 2.54),
        snowfall48h: (sumLast(sf, 48) / 2.54),
        depthIn: lastNonNull(sd) * 39.37           // inches
      };

      snowCache[id] = entry;
      callback(null, entry);
    })
    .catch(function (err) {
      snowCache[id] = { fetchedAt: Date.now(), error: true };
      callback(err, null);
    });
}
```

### Batch loading

After `renderListings()` renders cards for visible features, kick off snow data loading in the background. Batch 10 concurrent requests:

```js
function loadSnowForVisible(features) {
  var toFetch = features.filter(function (f) {
    var c = snowCache[f.properties.id];
    return !c || c.error || (Date.now() - c.fetchedAt > 30 * 60 * 1000);
  });

  var BATCH = 10;
  var idx = 0;

  function next() {
    if (idx >= toFetch.length) return;
    var batch = toFetch.slice(idx, idx + BATCH);
    idx += BATCH;
    var pending = batch.length;
    batch.forEach(function (f) {
      fetchSnowData(f, function () {
        pending--;
        updateSnowBadge(f.properties.id);
        if (pending === 0) next();
      });
    });
  }
  next();
}
```

Call `loadSnowForVisible(filtered)` at the end of `renderListings()`.

### Snow badge — list cards

Add to each card in `renderListings()`:

```js
var snowBadge = document.createElement('span');
snowBadge.className = 'tag tag--newsnow';
snowBadge.id = 'snow-badge-' + prop.id;

var cached = snowCache[prop.id];
if (cached && !cached.error) {
  var inches = snowInchesForWindow(cached, state.snowWindow);
  snowBadge.textContent = '❄ ' + inches.toFixed(1) + '"';
  if (inches === 0) snowBadge.style.display = 'none';
} else {
  snowBadge.textContent = '❄ …';
  snowBadge.style.color = 'var(--muted)';
}
tags.appendChild(snowBadge);
```

Helper:
```js
function snowInchesForWindow(entry, hours) {
  if (hours === 6)  return entry.snowfall6h;
  if (hours === 12) return entry.snowfall12h;
  if (hours === 24) return entry.snowfall24h;
  return entry.snowfall48h;
}
```

`updateSnowBadge(id)` function to update a single badge after async fetch completes:
```js
function updateSnowBadge(id) {
  var el = document.getElementById('snow-badge-' + id);
  if (!el) return;
  var cached = snowCache[id];
  if (!cached || cached.error) {
    el.textContent = '❄ ?';
    return;
  }
  var inches = snowInchesForWindow(cached, state.snowWindow);
  el.textContent = '❄ ' + inches.toFixed(1) + '"';
  el.style.display = inches === 0 ? 'none' : '';
  el.style.color = '';
}
```

### Filter bar — Snow chip

Add in filter bar HTML:
```html
<div class="chip-wrap">
  <button class="chip" id="chip-snow-filter" type="button" aria-haspopup="true" aria-expanded="false">
    ❄ Fresh snow <span aria-hidden="true">▾</span>
  </button>
  <div class="popover" id="popover-snow" role="dialog" aria-label="Fresh snow filter">
    <div class="popover__body">
      <div class="snow-window-label">Time window</div>
      <div class="snow-window-chips" role="group" aria-label="Snow time window">
        <button class="snow-win-btn" data-hours="6"  type="button">6h</button>
        <button class="snow-win-btn" data-hours="12" type="button">12h</button>
        <button class="snow-win-btn is-active" data-hours="24" type="button">24h</button>
        <button class="snow-win-btn" data-hours="48" type="button">48h</button>
      </div>
      <div class="snow-min-label">Minimum new snow: <span id="snow-min-readout">0"</span></div>
      <input type="range" id="snow-min-slider" min="0" max="12" step="0.5" value="0"
             aria-label="Minimum new snow in inches">
    </div>
    <div class="popover__footer">
      <button class="btn-show-areas" id="btn-show-snow" type="button">Show areas</button>
    </div>
  </div>
</div>
```

Add to `state.filters`:
```js
state.filters.snowMinIn = 0;   // minimum new snowfall in inches for selected window
```

Add to `state`:
```js
state.snowWindow = 24;   // 6 | 12 | 24 | 48
```

### Snow filter logic

In `featureMatchesFilters()`:
```js
if (state.filters.snowMinIn > 0) {
  var sc = snowCache[prop.id];
  if (!sc || sc.error) return false;  // exclude if data not yet loaded
  var inches = snowInchesForWindow(sc, state.snowWindow);
  if (inches < state.filters.snowMinIn) return false;
}
```

In `hasActiveFilters()`:
```js
|| state.filters.snowMinIn > 0
```

### Snow chip JS

Wire up the time window buttons to update `state.snowWindow` and re-render badges:
```js
document.querySelectorAll('.snow-win-btn').forEach(function (btn) {
  btn.addEventListener('click', function () {
    state.snowWindow = parseInt(btn.dataset.hours, 10);
    document.querySelectorAll('.snow-win-btn').forEach(function (b) {
      b.classList.toggle('is-active', b.dataset.hours === btn.dataset.hours);
    });
    // Re-render all visible badges
    (state.sync ? visibleFeatures : allFeatures).forEach(function (f) {
      updateSnowBadge(f.properties.id);
    });
    if (state.filters.snowMinIn > 0) applyFilters();
  });
});
```

Wire slider:
```js
var snowMinSlider = document.getElementById('snow-min-slider');
var snowMinReadout = document.getElementById('snow-min-readout');
snowMinSlider.addEventListener('input', function () {
  state.filters.snowMinIn = parseFloat(snowMinSlider.value);
  snowMinReadout.textContent = state.filters.snowMinIn.toFixed(1) + '"';
});
snowMinSlider.addEventListener('change', function () {
  applyFilters();
});
```

### CSS additions

```css
.tag--newsnow {
  background: #dbeeff;
  color: #0a4a8a;
}

.snow-window-chips { display: flex; gap: 6px; margin: 8px 0; }
.snow-win-btn {
  flex: 1; height: 30px; border: 1px solid var(--line);
  border-radius: var(--radius-pill); background: #fff;
  color: var(--navy); font-size: 13px; font-weight: 600; cursor: pointer;
}
.snow-win-btn.is-active { background: var(--navy); color: #fff; border-color: var(--navy); }
.snow-min-label { font-size: 13px; color: var(--muted); margin: 8px 0 4px; }
```

---

## Step 12 — Recent Snow: Map Visualization

**Goal**: Color map pins by snowfall amount when snow data is available. Add a legend. Toggle between "snow color" and "grooming type color" mode.

### Color mode state

```js
state.mapColorMode = 'groom';  // 'groom' | 'snow'
```

### Map toggle button

Add a small button to the map overlay controls (top-right corner, stacked with existing zoom buttons):
```html
<button id="btn-color-mode" class="map-overlay-btn" type="button" title="Toggle map color">
  ❄ Snow
</button>
```

When clicked, toggle `state.mapColorMode` and call `updatePinColors()`.

### Snow color scale

```js
var SNOW_COLORS = {
  none:   '#9aa4ad',   // no snow
  trace:  '#b8d4e8',   // 0-0.5"
  light:  '#6db3e0',   // 0.5-2"
  moderate: '#2b87c8', // 2-4"
  heavy:  '#ffffff'    // 4"+
};
```

### updatePinColors function

```js
function updatePinColors() {
  if (!map.getLayer('unclustered-point')) return;

  if (state.mapColorMode === 'groom') {
    map.setPaintProperty('unclustered-point', 'circle-color', [
      'match', ['get', 'groomType'],
      'both',    '#1b4f8a',
      'skate',   '#e07b24',
      'classic', '#6cc0e5',
      '#9aa4ad'
    ]);
    document.getElementById('snow-legend').style.display = 'none';
  } else {
    // Build a match expression from snowCache
    var matchExpr = ['match', ['get', 'id']];
    allFeatures.forEach(function (f) {
      var id = f.properties.id;
      var cached = snowCache[id];
      var color = SNOW_COLORS.none;
      if (cached && !cached.error) {
        var inches = snowInchesForWindow(cached, state.snowWindow);
        if (inches >= 4)   color = SNOW_COLORS.heavy;
        else if (inches >= 2) color = SNOW_COLORS.moderate;
        else if (inches >= 0.5) color = SNOW_COLORS.light;
        else if (inches > 0)  color = SNOW_COLORS.trace;
      }
      matchExpr.push(id, color);
    });
    matchExpr.push(SNOW_COLORS.none);  // fallback
    map.setPaintProperty('unclustered-point', 'circle-color', matchExpr);
    document.getElementById('snow-legend').style.display = '';
  }
}
```

Call `updatePinColors()` after each `updateSnowBadge()` batch completes, and when `state.snowWindow` changes while in snow color mode.

### Snow legend HTML

Add to the map's overlay area:
```html
<div id="snow-legend" class="map-legend" style="display:none">
  <div class="map-legend__title">New snow (24h)</div>
  <div class="map-legend__row"><span class="map-legend__swatch" style="background:#ffffff;border:1px solid #ccc"></span> 4"+</div>
  <div class="map-legend__row"><span class="map-legend__swatch" style="background:#2b87c8"></span> 2–4"</div>
  <div class="map-legend__row"><span class="map-legend__swatch" style="background:#6db3e0"></span> 0.5–2"</div>
  <div class="map-legend__row"><span class="map-legend__swatch" style="background:#b8d4e8"></span> trace</div>
  <div class="map-legend__row"><span class="map-legend__swatch" style="background:#9aa4ad"></span> None</div>
</div>
```

### CSS

```css
.map-overlay-btn {
  display: block;
  width: 36px;
  height: 36px;
  border: 1px solid var(--line);
  border-radius: 4px;
  background: #fff;
  box-shadow: var(--shadow-control);
  cursor: pointer;
  font-size: 14px;
  color: var(--navy);
  padding: 0;
}
.map-overlay-btn.is-active { background: var(--navy); color: #fff; }

.map-legend {
  position: absolute;
  bottom: 32px;
  left: 10px;
  background: rgba(255,255,255,0.92);
  border-radius: var(--radius-input);
  border: 1px solid var(--line);
  padding: 8px 10px;
  font-size: 12px;
  box-shadow: var(--shadow-control);
  z-index: 5;
}
.map-legend__title { font-weight: 700; color: var(--ink); margin-bottom: 6px; }
.map-legend__row { display: flex; align-items: center; gap: 6px; margin-bottom: 3px; color: var(--muted); }
.map-legend__swatch { width: 12px; height: 12px; border-radius: 50%; flex-shrink: 0; }
```

---

## Step 13 — Trail Lines on Map

**Goal**: Show nordic ski trail lines on the map. Use OpenSkiMap's vector tile layer (which aggregates OSM `piste:*` data) as the primary source. Show/hide with a toggle button, only enabled at zoom ≥ 11. Link to OpenSkiMap for full detail.

### Trail line source — OpenSkiMap vector tiles

**Important**: Verify the exact OpenSkiMap tile endpoint before implementation. Check the OpenSkiMap GitHub repo (`openskimap/openskimap.org`) for the current tile URL. Expected format (verify this):
```
https://tiles.openskimap.org/ski/{z}/{x}/{y}.pbf
```
or possibly:
```
https://api.openskimap.org/tiles/{z}/{x}/{y}
```

These are Mapbox Vector Tile (MVT) format. Add as a vector source in Mapbox GL JS:

```js
map.addSource('openskimap-trails', {
  type: 'vector',
  tiles: ['https://tiles.openskimap.org/ski/{z}/{x}/{y}.pbf'],  // VERIFY URL
  minzoom: 8,
  maxzoom: 14,
  attribution: '© OpenSkiMap contributors | © OpenStreetMap'
});
```

**Fallback if tile URL cannot be verified**: Query the Overpass API for the current viewport on demand. This is more complex but is a reliable fallback:
```
https://overpass-api.de/api/interpreter?data=[out:json];way[piste:type=nordic](south,west,north,east);out geom;
```
Convert the result to GeoJSON and add as a `geojson` source. Throttle to not fire more than once per 2 seconds after map move. This approach is slower and less complete than the tile layer.

### OpenSkiMap layer schema

OpenSkiMap MVT tiles contain features with these relevant properties (verify against their schema):
- `piste:type`: `nordic` | `downhill` | `backcountry` | etc.
- `piste:grooming`: `classic` | `skating` | `classic+skating` | `backcountry` | etc.
- `name`: trail name (optional)
- `difficulty`: `easy` | `intermediate` | `advanced` (optional)

Filter to only show nordic piste features: `['==', ['get', 'piste:type'], 'nordic']` (or the equivalent for the actual property name in their schema).

### Layer name from source

You will need to know the **layer name** inside the MVT tiles to reference it. OpenSkiMap likely uses `piste` or `ski` as the source layer name. Determine this from their GitHub or by inspecting the tile response. Example:
```js
'source-layer': 'piste'  // VERIFY
```

### Trail line layers

Add BELOW the existing unclustered point layers (so trails appear under pins) but ABOVE terrain:

```js
// Trail line casing (slightly wider, white) for legibility
map.addLayer({
  id: 'ski-trails-casing',
  type: 'line',
  source: 'openskimap-trails',
  'source-layer': 'piste',          // VERIFY
  filter: ['==', ['get', 'piste:type'], 'nordic'],
  layout: {
    'line-cap': 'round',
    'line-join': 'round',
    'visibility': 'none'           // hidden by default
  },
  paint: {
    'line-color': '#fff',
    'line-width': ['interpolate', ['linear'], ['zoom'], 10, 3, 14, 6],
    'line-opacity': 0.7
  }
}, 'unclustered-point');  // insert before this layer

// Trail line colored by grooming type
map.addLayer({
  id: 'ski-trails',
  type: 'line',
  source: 'openskimap-trails',
  'source-layer': 'piste',          // VERIFY
  filter: ['==', ['get', 'piste:type'], 'nordic'],
  layout: {
    'line-cap': 'round',
    'line-join': 'round',
    'visibility': 'none'
  },
  paint: {
    'line-color': [
      'match', ['get', 'piste:grooming'],
      ['classic+skating', 'skating+classic'], '#1b4f8a',
      'skating',  '#e07b24',
      'classic',  '#6cc0e5',
      '#9aa4ad'  // unknown/backcountry
    ],
    'line-width': ['interpolate', ['linear'], ['zoom'], 10, 1.5, 14, 4],
    'line-opacity': 0.85
  }
}, 'unclustered-point');
```

### Trail toggle button

Add to the map overlay controls (beside zoom buttons):
```html
<button id="btn-trails" class="map-overlay-btn" type="button" title="Show ski trails" disabled>
  <svg>...</svg>  <!-- trail/path icon -->
</button>
```

JS:
```js
var trailsVisible = false;

function updateTrailsButton() {
  var zoom = map.getZoom();
  var btn = document.getElementById('btn-trails');
  if (zoom < 10) {
    btn.disabled = true;
    btn.title = 'Zoom in to see trails';
  } else {
    btn.disabled = false;
    btn.title = trailsVisible ? 'Hide trails' : 'Show trails';
  }
}

document.getElementById('btn-trails').addEventListener('click', function () {
  trailsVisible = !trailsVisible;
  var vis = trailsVisible ? 'visible' : 'none';
  map.setLayoutProperty('ski-trails-casing', 'visibility', vis);
  map.setLayoutProperty('ski-trails', 'visibility', vis);
  this.classList.toggle('is-active', trailsVisible);
  updateTrailsButton();
});

map.on('zoom', updateTrailsButton);
map.on('load', updateTrailsButton);
```

### Trail legend

Add a small legend (similar to snow legend) showing:
- Blue = Skate + classic
- Orange = Skate only
- Light blue = Classic only
- Grey = Unknown

Show/hide this legend when trail visibility toggles.

### OpenSkiMap link in detail panel

In `openDetailPanel()`, add a link at the bottom of the panel:
```js
var coords = feature.geometry.coordinates;
var osmLink = document.createElement('a');
osmLink.href = 'https://openskimap.org/#12/' + coords[1].toFixed(4) + '/' + coords[0].toFixed(4);
osmLink.target = '_blank';
osmLink.rel = 'noopener noreferrer';
osmLink.className = 'detail-external-link';
osmLink.textContent = 'View trails on OpenSkiMap →';
```

Always show this link regardless of whether the trail layer has coverage for that area.

### CSS

```css
.detail-external-link {
  display: block;
  margin-top: 16px;
  font-size: 14px;
  color: var(--link);
  text-decoration: none;
  border-top: 1px solid var(--line);
  padding-top: 16px;
}
.detail-external-link:hover { text-decoration: underline; }
```

---

## Step 14 — Grooming Status

**Goal**: Show which trails were recently groomed (< 12h, < 24h). Let users filter by "has recent grooming." Requires Nordic Pulse API access (see blocker below).

### Blocker: Nordic Pulse API

Nordic Pulse (`nordic-pulse.com`) is the only XC-specific grooming data source. They do not have public API documentation. To get access:
1. Email `hello@nordic-pulse.com`
2. Frame the request as promoting their partner areas to skiers
3. Ask about developer/aggregator API terms

Until API access is obtained, Step 14 uses the **fallback mode** only (groomingUrl link). Build the full architecture now so it's plug-and-play when the API key arrives.

### Architecture

Nordic Pulse returns grooming data per area keyed by their own area ID. Since our GeoJSON has no Nordic Pulse IDs, lookup must be by lat/lng proximity or by matching area name/URL.

Expected Nordic Pulse API shape (based on their website's data — verify against actual API docs when obtained):
```json
{
  "area_id": "np-1234",
  "name": "Kincaid Park",
  "last_groomed": "2024-01-15T06:30:00Z",
  "groomed_km": 12.5,
  "trails": [
    { "trail_id": "np-trail-456", "name": "Lower Loop", "groomed_at": "2024-01-15T06:30:00Z", "km": 3.2 }
  ]
}
```

### Grooming cache

```js
var groomingCache = {};
// groomingCache[areaId] = { fetchedAt, lastGroomedAt: Date, groomedKm24h: number, groomedKm12h: number }
// or { fetchedAt, error: true }
var NORDIC_PULSE_KEY = '';  // set when API key is obtained
```

### featureMatchesFilters additions

```js
if (state.filters.groomedRecently) {
  var gc = groomingCache[prop.id];
  if (!gc || gc.error) return false;
  var hoursAgo = (Date.now() - gc.lastGroomedAt) / 3600000;
  var threshold = state.filters.groomedWithin;  // 12 or 24
  if (hoursAgo > threshold) return false;
}
```

Add to `state.filters`:
```js
state.filters.groomedRecently = false;
state.filters.groomedWithin = 24;   // 12 | 24
```

### Filter chip HTML

Add to filter bar:
```html
<div class="chip-wrap">
  <button class="chip" id="chip-groomed-recently" type="button" aria-haspopup="true" aria-expanded="false">
    Recently groomed <span aria-hidden="true">▾</span>
  </button>
  <div class="popover" id="popover-groomed-recently" role="dialog" aria-label="Recently groomed filter">
    <div class="popover__body">
      <div class="groom-window-chips" role="group" aria-label="Grooming window">
        <button class="snow-win-btn" data-hours="12" type="button">Last 12h</button>
        <button class="snow-win-btn is-active" data-hours="24" type="button">Last 24h</button>
      </div>
    </div>
    <div class="popover__footer">
      <button class="btn-show-areas" id="btn-show-groomed" type="button">Show areas</button>
    </div>
  </div>
</div>
```

### Grooming badge on list cards

Add to each card (when data is available):
```js
if (gc && !gc.error && gc.groomedKm24h > 0) {
  var groomBadge = document.createElement('span');
  groomBadge.className = 'tag tag--groomed-recent';
  var hoursAgo = (Date.now() - gc.lastGroomedAt) / 3600000;
  groomBadge.textContent = '✓ ' + gc.groomedKm24h.toFixed(0) + ' km groomed'
    + (hoursAgo < 12 ? ' < 12h' : '');
  tags.appendChild(groomBadge);
}
```

### Grooming in detail panel

Show in the detail grid:
- **Last groomed**: "3 hours ago" | "Yesterday" | "2 days ago"
- **Km groomed (24h)**: "18.5 km"
- **Grooming report**: link to `groomingUrl` if available (always show this as fallback)

### Trail line grooming colors (Step 14 extension of Step 13)

When grooming data is available, update the `ski-trails` layer paint to encode recency:
- `< 12h` groomed → bright green `#2ecc71`
- `12–24h` groomed → medium green `#27ae60`
- `> 24h` groomed → grey `#9aa4ad`
- unknown → grey `#9aa4ad`

This requires matching trail IDs from Nordic Pulse to the trail geometries from OpenSkiMap. This matching may require a shared identifier (OSM way ID or trail name). Verify what Nordic Pulse returns and how it relates to OpenSkiMap data.

### Fallback (before Nordic Pulse API)

In the detail panel, always show:
```js
if (prop.groomingUrl) {
  var groomLink = document.createElement('a');
  groomLink.href = prop.groomingUrl;
  groomLink.target = '_blank';
  groomLink.rel = 'noopener noreferrer';
  groomLink.className = 'detail-external-link';
  groomLink.textContent = 'View grooming report →';
}
```

The filter chip for "Recently groomed" can be disabled/hidden until Nordic Pulse data is available. In the fallback state, the trail lines from Step 13 remain but are not colored by grooming recency.

---

## General Implementation Notes

### Adding new map layers

Always add new layers inside `map.on('load', function () { ... })`. Use `map.addLayer(layerDef, beforeLayerId)` to control z-order. Keep the `unclustered-point` and `unclustered-point-hitarea` layers on top so pins remain clickable.

### Popover open/close pattern

All popovers use the same pattern already in the code. Each popover chip:
1. Toggles `.is-open` on the popover div and `aria-expanded` on the button
2. A document-level `mousedown` listener closes any open popover when clicking outside
3. "Show areas" button in the popover footer closes the popover and applies filters

Follow this exact pattern for new popovers. Do not reinvent it.

### State persistence (localStorage + URL)

Follow the existing pattern:
- Add new filter keys to `state.filters` (these get serialized in `saveToStorage()` / `buildQueryString()`)
- Add new non-filter state to `state` separately, with its own URL param and localStorage key if needed
- Update `buildQueryString()` to include new params
- Update the URL-parsing IIFE at top to read new params

### No ES6+

All JS must be written in ES5 style (no `const`/`let`, no arrow functions, no template literals, no destructuring, no `class`, no `Promise` chains beyond what's already used). Use `var`, named functions, and `.then()` for promises.

### CSS convention

All new CSS goes in the `<style>` block, grouped by component (matching the existing section comments). Add mobile overrides to the existing `@media (max-width: 760px)` block. Use only design tokens from `:root` for colors; do not hardcode colors without a good reason.

### Testing checklist for each step

- [ ] Works at desktop (1280px+)
- [ ] Works at tablet (768px)
- [ ] Works on mobile (375px): list view and map view
- [ ] Filter state persists in URL (copy URL and paste in new tab)
- [ ] Filter state persists in localStorage (refresh page)
- [ ] No JS errors in console
- [ ] Cards and map both update when filter changes
- [ ] "Clear all" resets the new filter
