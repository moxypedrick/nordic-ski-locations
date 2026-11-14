# UX fixes for Nordic Ski Centers

Instructions for fixing the problems found in the November 2026 UI review. Written for someone who has not seen the review.

## Before you start

- The whole app is one file: `index.html` (CSS, markup, and JS). Line numbers below match commit `f52966f`. They will drift as you edit, so each item also names the function or selector.
- Run it with `python -m http.server 8734` from the repo root, then open `http://localhost:8734`. There is no build step and no test suite. Verify each fix by hand using the "Check" steps.
- Test at two widths: desktop (1280 px or wider) and phone (375 x 812). The mobile breakpoint is `max-width: 760px`.
- `data/nordic-ski-areas-merged.geojson` and `data/merge-report.json` have uncommitted changes in the owner's working tree. Ask before regenerating or committing either file.
- Mapbox GL JS is pinned at v1.12.0. Do not upgrade it as part of this work.
- Work in the order given. Section A fixes wrong or broken behaviour. Section B fixes the mobile layout. Sections C and D are improvements.

Counts quoted below come from the current data file: 1,473 areas in total, 1,324 inside the default map view.

---

## A. Broken or misleading behaviour

### A1. Snow data: stop the request flood

**Problem.** `renderListings()` ends by calling `loadSnowForVisible(features)` (line 2473). That function sends one Open-Meteo request per ski area (`fetchSnowData`, line 4264). The default view holds 1,324 areas, so every page load sends about 1,324 requests, 10 at a time. Open-Meteo answers with `429 Too Many Requests` (499+ seen in one session). Other faults in the same code:

- `fetch(...).then(r => r.json())` never checks `r.ok`, so a 429 becomes a thrown error and the area is cached as `{ error: true }`.
- An errored area is re-fetched on every map move (`loadSnowForVisible` treats `c.error` as "needs fetch", line 4319).
- The Fresh snow filter drops any area without data (`featureMatchesFilters`, line 2200) and tells the user nothing.
- Snow badges never appear when data arrives. `renderListings` only adds the badge element to the card if snow data is already cached (lines 2425 to 2437), so `updateSnowBadge()` finds no element to update.

**What to change.**

1. Add a cap. Define `var SNOW_MAX_AREAS = 60;`. In `loadSnowForVisible`, return early when `features.length > SNOW_MAX_AREAS`. At continent zoom the app must send zero snow requests.
2. Debounce. `renderListings` runs on every `moveend` and every filter change. Wrap the snow load in a 400 ms debounce, and keep a generation counter so an older batch stops when a newer one starts.
3. Batch locations. Open-Meteo accepts comma-separated lists: `latitude=44.9,45.1&longitude=-93.2,-93.4`. The response is an array in the same order (a single location returns a plain object, so normalise to an array). Send up to 50 areas per request. Read Open-Meteo's current terms first: a multi-location call may count as several calls against the rate limit, which is why the cap in step 1 matters more than the batching.
4. Handle failure properly:
   - Check `r.ok` before parsing.
   - On a 429, stop the queue and set `snowBlockedUntil = Date.now() + 60000`. Do not start new fetches before then.
   - Do not retry an errored area for 5 minutes.
5. Persist the cache. Write `snowCache` to `sessionStorage` (key `nsc:v1:snow`) and read it at start-up, keeping the existing 30 minute freshness rule. A reload should not re-fetch.
6. Always create the badge element. In `renderListings`, append the `snow-badge-<id>` span to every card, hidden by default, so `updateSnowBadge()` can fill it in later. Remove the `'❄ ?'` text on error and hide the badge instead.
7. Tell the user what is happening. Add one status line under the result count (reuse `#result-subtitle` or add a sibling element). Show it only when the Fresh snow filter is on (`state.filters.snowMinIn > 0`) or pin colour mode is `snow`:
   - More than 60 areas in view: "Zoom in to load snow data."
   - Loading: "Loading snow data…"
   - Blocked or failed: "Snow data is unavailable right now."
8. When the Fresh snow filter is on and more than 60 areas are in view, do not filter by snow at all. Show the "Zoom in" message instead of an empty list.
9. Verify the time window. The request uses `past_days=2&forecast_days=0`, and the code sums the last 6, 12, 24, or 48 entries of the hourly array. Log `data.hourly.time` and confirm the last entry is the current hour. If the array ends at midnight, the "last 6 hours" figure is wrong. In that case switch to Open-Meteo's `past_hours=48&forecast_hours=0` parameters, or cut the array at the current hour before summing.

**Check.**
- Hard-reload at the default view with the Network tab open: no requests to `api.open-meteo.com`.
- Search "Minneapolis": one or two snow requests, no 429s.
- Pan and zoom for a minute: still no 429s.
- Turn on the Fresh snow filter at continent zoom: the list shows the "Zoom in" message, not "0 ski areas".

### A2. Stop showing missing data as "No"

**Problem.** In `openDetailPanel()` (lines 3330 and 3333) Snowmaking and Lit at night print "No" for anything other than `'Yes'`. The value is `null` for 1,001 areas (Snowmaking) and 994 areas (Lit). The panel states a fact the data does not hold.

**What to change.** For both fields: show "Yes" for `'Yes'`, "No" for `'No'`, and leave the row out for anything else. Use the existing `isValid()` helper. The same rule already applies to Day pass, Rentals, Lessons, and Dogs.

**Check.** Open "49 North Nordic Center" (data is null): no Snowmaking or Lit rows. Open "Theodore Wirth North": both rows say "Yes". Open "Chain of Lakes": both say "No".

### A3. Selecting an area from the list must show it on the map

**Problem.** `openDetailPanel()` only moves the map when the pin is outside the current bounds (line 3385). At continent zoom every pin is "in bounds" but hidden inside a cluster, so clicking a card changes nothing on the map.

There are also four separate copies of the "select an area" logic: the card click handler (line 2449), the pin click handler (line 2917), `pickSuggestion()` (line 3603), and the URL `area=` restore (line 2680).

**What to change.**

1. Write one function, `selectArea(feature, source)`, where `source` is `'card'`, `'pin'`, `'search'`, or `'url'`. It clears the old selection, sets `state.selectedId` and the `selected` feature-state, marks the matching card `is-active`, and calls `openDetailPanel(feature)`. Replace the four copies with calls to it.
2. Camera rule inside `selectArea` (desktop, and mobile list view):
   - `source === 'pin'`: do not move the map. The user can already see the pin.
   - Otherwise: if `map.getZoom() < 11` or the pin is out of bounds, call `map.flyTo({ center: coords, zoom: Math.max(map.getZoom(), 11) })`. Clusters stop at zoom 10 (`clusterMaxZoom: 10`), so zoom 11 always shows the pin on its own.
   - Mobile map view keeps its current path through `_openMapBottomSheet` (see B2).
3. Restore the view on "Back to results". Before flying, save `{ center, zoom }` in a variable such as `viewBeforeSelect`. Clear that variable if the user moves the map by hand (listen for `dragstart` and `zoomstart` where `e.originalEvent` exists). In `closeDetailPanel()`, if the variable is still set, `map.easeTo()` back to it and clear it.

**Check.** At the default view click the card "49 North Nordic Center": the map flies to Washington state and the pin is highlighted. Click "Back to results": the map returns to the continent view and the list scroll position is kept. Repeat, but drag the map before going back: the map stays where you left it.

### A4. Search suggestions appear under the "Update list as I move" pill

**Problem.** On desktop the search dropdown is covered by the pill at the top of the map. `.site-header` has `z-index: 10`, and `.sync-switch-wrap` inside `#map` also has `z-index: 10`. `#map` creates no stacking context, so the pill, being later in the document, wins.

**What to change.** Add `z-index: 0;` to the `#map` rule (line 749). `#map` is a flex item, so this creates a stacking context and keeps every map overlay below the header and the filter popovers.

**Check.** Type "Minneap" in the search box on desktop: all four suggestions are fully visible. Open each filter popover: none is clipped by map controls.

### A5. The Fresh snow popover ignores Escape and the shared popover logic

**Problem.** The Grooming and More filters popovers share `togglePopover()` and `closeAllPopovers()` (line 3104). The Fresh snow popover has its own open and close code (line 4353). Escape does not close it, and opening it does not close the others.

**What to change.** Move the snow popover onto the shared functions: add `popoverSnowEl` and its chip to `closeAllPopovers()`, to the outside-click handler (line 3124), and to the Escape handler (line 3132). Delete the separate `mousedown` handler (line 4405). After C5 below, the Escape handler must map each popover to its own chip for focus return.

**Check.** Open Fresh snow, press Escape: it closes and focus returns to the chip. Open Grooming, then click Fresh snow: Grooming closes.

### A6. Restore banner shows a raw key and restores only half the state

**Problem.** `initWelcomeBanner()` (line 3800) builds text like "sorted by dist" from the internal sort key. It also restores the saved location while the map stays at the continent view, so the list shows hundreds of areas "sorted by distance" instead of the user's last area.

**What to change.**

1. Add a lookup and use it at line 3828:
   ```js
   var SORT_LABELS = { name: 'name', dist: 'nearest first', kmDesc: 'most trail km',
                       kmAsc: 'fewest trail km', groom: 'grooming', snow: 'snowmaking first' };
   ```
2. When a location is restored from storage and the URL has no query string, set `pendingMapParam` to `lat + ',' + lng + ',9'`. `initWelcomeBanner()` runs before the data fetch resolves, so the existing `pendingMapParam` code (line 2688) will move the map.
3. Fix "Start fresh" (line 3921): its reset object omits `snowMinIn`, and it leaves `state.favorites` and the map view unchanged. Reset both and fit the map to `dataBounds`.

**Check.** Search "Minneapolis", then open `http://localhost:8734/` in the same browser: the banner reads "Restored: Near Minneapolis, Minnesota, United States, sorted by nearest first" and the map shows Minneapolis. Click "Start fresh": the map returns to the continent view, sort is Name, and no filter is active.

---

## B. Mobile layout

### B1. Give the map and the list more room

**Problem.** At 375 x 812 the header, search, Filters row, List/Map toggle, restore banner, and trail-length slider stack above the content. In list view the first card starts about two-thirds of the way down. In map view the slider is deliberately moved above the map by `showMap()` (lines 4158 to 4161), which costs the map about 110 px.

**What to change.**

1. Put the trail-length slider inside the collapsible filter area on mobile. At start-up, when `window.innerWidth <= 760`, move `.trail-length-area` into `#filter-bar-chips` as a full-width item (`flex: 0 0 100%`). Delete the code in `showMap()` and `showList()` that moves it back and forth (lines 4141 to 4145 and 4158 to 4161). Use a `matchMedia('(max-width: 760px)')` listener to move it back into the sidebar, before `.results-header`, if the window grows past the breakpoint.
2. Keep filters collapsed in both views. Remove the `_setFiltersExpanded(true)` call in `showList()` (line 4147). The toggle already shows "Filters (n)" when filters are active.
3. Hide the sync pill in mobile map view, where no list is visible:
   ```css
   @media (max-width: 760px) {
     body.mobile-map-view .sync-switch-wrap { display: none; }
   }
   ```
4. Shrink the Share button on mobile: hide `.btn-share__label` under 760 px and add `aria-label="Share this search"` to `#btn-share`.
5. Let the restore banner time out: hide it after 8 seconds or on the first map move or filter change, whichever comes first.

**Check.** At 375 x 812 with no restore banner: list view shows at least three full cards without scrolling. Map view: the map starts directly under the List/Map toggle. Open Filters: the slider is inside, works, and the histogram draws.

### B2. The selected pin lands under the bottom sheet

**Problem.** In mobile map view, tapping a pin opens the bottom sheet and flies the map, but the pin ends up hidden beneath the sheet. `openSheet()` (line 4017) computes:

```js
var offsetY = Math.round((mapH / 2) - (visibleH * 0.4));
map.flyTo({ center: coords, zoom: targetZoom, offset: [0, offsetY] });
```

Mapbox's `offset` places the target at "container centre plus offset", and positive y points down. The value above is positive, so the pin is pushed down, toward the sheet. The sign looks inverted.

**What to change.** Use `var offsetY = Math.round((visibleH * 0.4) - (mapH / 2));`. Then test, because the sheet height is measured while the sheet is still animating in. If the pin position is still off, read `sheetEl.offsetHeight` inside a `requestAnimationFrame` callback after adding `is-open`. Do B1 first, since it changes the map height.

Also cap the sheet relative to the map, not the viewport: change `.map-bottom-sheet { max-height: 50vh }` (line 1378) to `max-height: 60%`.

**Check.** At 375 x 812, map view, zoom to Minneapolis, tap "Chain of Lakes": the highlighted pin sits in the upper half of the map area that remains visible above the sheet. Repeat with a pin near each edge of the screen.

### B3. Let the bottom sheet lead to full details

**Problem.** The sheet shows name, region, grooming, trail km, and three buttons. Notes, snowmaking, lights, and the OpenSkiMap link exist only in the sidebar detail panel, which is hidden in map view.

**What to change.** Add a "More details" button to `#mbs-actions`. The pin click handler already calls `openDetailPanel()` before the sheet opens, so the detail panel is populated. On click, switch to list view without clearing the selection:

- Give `closeSheet()` a parameter, `closeSheet(keepSelection)`, and skip the selection-clearing block when it is true.
- Have `showList()` accept the same flag and pass it through.
- The button calls `showList(true)`. The user lands on the detail panel, and "Back to results" works as it does today.

Also replace the heart icon in `#mbs-save-icon` (line 1952) with a star, to match the star used on cards and in the detail panel.

**Check.** Tap a pin in mobile map view, tap "More details": the list view opens on that area's detail panel with notes visible. Tap "Back to results": the list appears.

---

## C. Clarity

### C1. One source of truth for grooming labels, and a label for "groomed, style unknown"

**Problem.** Cards and pins read `prop.groomType`. The detail panel and bottom sheet use `groomDisplayInfo()` (line 3238), which reads `groomSkate` and `groomClassic`. There are also two colour tables (`GROOM_COLOR` at line 2268, `GROOM_COLORS` at line 3236). Values of `groomType` in the data: `both` 364, `classic` 157, `unknown` 685, `null` 203, `yes` 64. The 64 `yes` areas are known to be groomed but are labelled "Grooming unknown".

**What to change.**

1. Keep one table and one function. Make `groomDisplayInfo(prop)` read `prop.groomType` and return `{ label, color }`. Use it in cards, the detail panel, and the bottom sheet. Delete `GROOM_COLORS`.
2. Add a `yes` entry: label "Groomed (style not listed)", colour `#5f7d95`. Add it to `GROOM_COLOR`, to both `match` expressions (`unclustered-point` paint at line 2832 and `GROOM_COLOR_EXPR` at line 4423), and to the grooming legend (`#legend-groom`, line 1895).
3. Treat `null` as `unknown`.

**Check.** Find an area with `groomType: 'yes'` (in the console: `allFeatures.find(f => f.properties.groomType === 'yes')`). Its card, pin, detail panel, and sheet all show "Groomed (style not listed)" in the new colour, and the legend lists it.

### C2. Make grey pins quieter

**Problem.** 888 of 1,473 pins are grey "unknown" pins drawn at the same size as the rest, so the colour legend carries little information at a glance.

**What to change.** In the `unclustered-point` layer, give unknown pins (`groomType` not in `both`, `skate`, `classic`, `yes`) a base `circle-radius` of 5 instead of 8 and a `circle-opacity` of 0.75. Keep the hover and selected radius of 14 for all pins. Leave the hit-area layer unchanged so small pins stay easy to click.

**Check.** At the Minneapolis view, coloured pins stand out and grey pins are smaller but still clickable.

### C3. Consistent region labels

**Problem.** Region text comes in several forms: "MI" (1,234 areas), "US-WA" (56), "Wolfeboro, US-NH" (163), and 20 others. The prefixed forms come from `dev/scripts/merge-skimap.js` line 285, which writes `place.iso3166_2` directly.

**What to change.**

1. In `index.html`, add `formatRegion(region)` that strips a leading `US-` or `CA-` from the state or province code (`/\b(US|CA)-(?=[A-Z]{2}\b)/g`). Use it wherever `prop.region` is displayed: cards (line 2382), detail panel (line 3262), bottom sheet (line 3972).
2. In `dev/scripts/merge-skimap.js`, strip the same prefix at line 285 so future merges are clean. Do not re-run the merge without checking with the owner (see "Before you start").
3. List the 20 "other" values and report them to the owner. In the console:
   ```js
   allFeatures.map(f => f.properties.region)
     .filter(r => r && !/^[A-Z]{2}$/.test(r) && !/^US-/.test(r) && !/, /.test(r))
   ```

**Check.** The first cards in the default list read "BC", "WA", "Wolfeboro, NH", "MI".

### C4. Units

**Problem.** Distance is in miles (`formatDist`, line 2531), trail length in km, snow in inches. There is no way to change any of them.

**What to change.** Trail length stays in km, which is how the sport measures trails. Add one setting for distance and snow:

- `state.units`: `'imperial'` or `'metric'`. Default to imperial when `navigator.language` is `en-US`, metric otherwise. Persist in `localStorage` under `nsc:v1:units`.
- Add a two-option control ("mi / km") beside the sort select in `.results-header`.
- `formatDist()` converts miles to km when metric (`prop._dist` is stored in miles).
- Snow readouts (`updateSnowBadge`, the card badge, the snow slider readout, the snow legend) show cm when metric. Keep `state.filters.snowMinIn` in inches internally.

**Check.** Switch to metric: card distances read "4.3 km", the snow slider reads in cm, and the choice survives a reload.

### C5. Simplify the filter bar

**Problem.**
- "More filters" opens a popover holding one checkbox.
- Popover footers have a button that reads "Show areas" until opened, then "Show N areas". It only closes the popover, because the checkboxes already apply at once. The Fresh snow button is the exception: its slider applies on release, and its button label never updates.

**What to change.**

1. Replace the More filters chip and popover with a toggle chip, `<button class="chip" id="chip-lit" aria-pressed="false">Lit at night</button>`, wired like the Snowmaking chip (line 3178). Remove `chipMoreEl`, `popoverMoreEl`, `filterLitEl`, `btnShowMoreEl`, `updateMoreChip()`, and every reference to them (`closeAllPopovers`, the outside-click and Escape handlers, `clearFilters`, `syncDomToState`, `updateFilterChipStates`).
2. Make the snow slider apply while dragging: call a 200 ms debounced `applyFilters()` from its `input` handler and drop the separate `change` handler (line 4399).
3. Make all popover footer buttons behave the same: update the label through `updateShowButton()` for the snow popover too, so each reads "Show N areas".

**Check.** Click "Lit at night": the list filters at once and the chip turns navy. "Clear all" resets it. A shared URL with `lit=yes` still restores it.

### C6. Label the map layer controls

**Problem.** The "colour pins by snow" button (`#btn-color-mode`) uses a target icon that looks like "locate me" and has no visible label. The trails button (`#btn-trails`) is disabled below zoom 10 with the reason available only on hover. A location search flies to zoom 9, so the trails button is disabled right after the most common action.

**What to change.**

1. Replace the two icon buttons with one "Layers" button in `.nav-controls` that opens a small panel (reuse the `.popover` styles, anchored to the right):
   - "Colour pins by": two radio buttons, "Grooming" and "Recent snow".
   - A checkbox, "Show trail lines". Below zoom 9 it is disabled with visible helper text: "Zoom in to show trails."
2. Change the threshold in `updateTrailsButton()` (line 4482) from 10 to 9. The trail tile source starts at zoom 8.
3. Keep `state.mapColorMode` and `trailsVisible` as they are. Only the controls change.

**Check.** After searching "Minneapolis" the trail lines checkbox is enabled. At continent zoom it is disabled and the helper text is visible without hovering. Choosing "Recent snow" swaps the legend.

---

## D. Polish

### D1. Loading state for the map

**Problem.** Pins take about 4 seconds to appear while the list already shows cards. The GeoJSON is also loaded twice: once by `addSource` with a URL (line 2637) and once by `fetch` (line 2645).

**What to change.** Create the source with an empty FeatureCollection, then call `setData(data)` in the fetch callback. Show a centred "Loading ski areas…" pill over the map (same style as `.sync-switch-wrap`) and remove it on the first `sourcedata` event where `map.isSourceLoaded('nordic-ski-areas')` is true. If the fetch fails, replace the skeleton cards with "Could not load ski areas. Reload to try again." There is no `.catch` today.

**Check.** Throttle the network to "Slow 3G": the pill shows until pins appear. Block the GeoJSON request: the error message appears.

### D2. List cap

**Problem.** The list stops at 100 cards with the note "Showing 100 of N areas. Zoom in to see fewer." (line 2468). There is no way to see the rest.

**What to change.** Replace the note with a button: "Show 100 more (N remaining)". Track a `listLimit` variable that starts at `CAP`, grows by 100 per click, and resets to `CAP` when the filters, sort, or map view change.

**Check.** At the default view, click the button: 200 cards. Change the sort: back to 100.

### D3. Hide the trail-length slider while a detail panel is open

**Problem.** On desktop the slider and histogram stay above the detail panel, where they do nothing useful.

**What to change.** In `openDetailPanel()` add a class to the sidebar (`is-detail-open`) and remove it in `closeDetailPanel()`. Replace the three inline `style.display` toggles for the list and results header with CSS:

```css
.sidebar.is-detail-open .trail-length-area,
.sidebar.is-detail-open .results-header,
.sidebar.is-detail-open .listing { display: none; }
```

**Check.** Open any area on desktop: the panel starts at the top of the sidebar. Go back: slider, header, and list return with scroll position kept.

### D4. One word and one icon for saved areas

**Problem.** The filter chip says "☆ Favorites", the detail button says "☆ Save", and the mobile sheet uses a heart.

**What to change.** Use a star and the word "Saved" throughout: chip "★ Saved", buttons "☆ Save" and "★ Saved". The heart is replaced in B3. Update the `aria-label` and `title` strings in `toggleFavorite()` (line 4211) and the card star (line 2367) to "Save" and "Remove from saved".

---

## Suggested commits

1. A1 (snow requests)
2. A2, A4, A5, A6 (small correctness fixes)
3. A3 (selection and camera)
4. B1, B2, B3 (mobile)
5. C1, C2, C3 (grooming labels, pins, regions)
6. C5, C6, D3, D4 (filter bar and controls)
7. C4, D1, D2 (units, loading, list cap)

## Regression pass when everything is done

Run these at both widths:

- Load with no saved state. Console shows no errors and no 429s.
- Search a town, pick a suggestion, open the first card, go back.
- Click a cluster, then a pin, then close the panel with Escape.
- Toggle each filter, then "Clear all".
- Copy the Share link, open it in a private window: same location, filters, sort, map view, and selected area.
- Save two areas, turn on the Saved filter, reload: both are still listed.
- Browser Back after opening an area closes the detail panel.

## Not covered by the review

These were not tested, so treat them as unknown: the geolocate button, the share popover, keyboard-only use, screen readers, and real phones (only an emulated 375 x 812 viewport was used). `admin.html` was not reviewed.
