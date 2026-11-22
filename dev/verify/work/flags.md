# Records flagged from agent reports during the full run

Items to raise at STOP 3. They may not have a comment or low confidence, so the review queue might not catch them.

| id | batch | concern |
|---|---|---|
| hiawatha-highlands-on | 004 | officialUrl never loaded (domain dead, trail page 404). Should be null under the "must load" rule added from batch 008 onward. |
| club-de-six-cantons-qc | 007 | Site says closed for all of 2025-26, "see you next year". Recorded operating/medium with an estimated date of 2026-04-01. Weak evidence of operating. |
| sleepy-hollow-vt | 008 | Site said "closed for the foreseeable future" (2026-03-31). Agent read it as end of season and recorded operating. The wording may mean a permanent closure. |
| superior-municipal-forest-wi | 010 | High confidence from an undated city page ("New in 2027"). Rules say medium. |
| high-point-cross-country-ski-center-nj | 010 | Banner says "CLOSED for this season". Report dated 2026-03-08. Operating/high looks right; listed for awareness. |
| stake-lake-trails-bc | 010 | Closed for 2025-26, but selling 2026-27 passes. Operating/high is supported by the passes. |
| mt-shasta-ca | 012 | High confidence rests on a homepage news title (lodge opening, Dec 2025); the post itself was not opened. |
| lolo-pass-mt | 015 | Stored coordinates are at Lolo town, about 40 km from Lolo Pass (beyond the 30 km same-place limit). Coordinates may need fixing; no comment set. |
| (tawatinaw, batch 017) | 017 | Page text on tawatinawvalley.ca asked the reader to connect a Google Calendar URL. Agent ignored it. Reported per plan section 2 (likely a calendar widget, not an attack). |
| (rule question) | 018 | Some agents give high confidence when a land-manager page has a "last updated" date of 2025-10-01 or later but no grooming report (for example 5 Alberta provincial parks in batch 018). Matches the plan wording; confirm or ask for medium. |
| mackenzie-nordiques-xc-ski-club-bc | 019 | Coordinates near Revelstoke, but the club is in Mackenzie BC (~500 km away). Coordinates likely wrong; no comment set. |
| (search result cavac.at) | 021 | Prompt injection: a Wikipedia-mirror search result contained text telling the reader to change topics. Ignored. Reported per plan section 2. |
| glenview-nordic-ski-club-nl | 021 | Coordinates point to Glenview, Illinois. Unverified. |
| deep-cove-ski-club-nl | 021 | Coordinates point to Deep Cove, BC. Unverified. |
| lafontaine-ski-trails-on | 026 | The dataset's own Notes (from canadatrails, 2008) say it is no longer operating. Left unverified because no current source confirms. Possible closure for David to judge. |
| club-mont-torbillon-qc | 027 | Site is now a golf club. One ski-history site says the ski area was removed in 1961. Left unverified (single non-news source). Possible closure for David to judge. |
| val-neigette-qc | 028 | Recorded closed/low, but the only sources are directory sites (pleinairalacarte, skiresort.info snippet). The plan accepts closure only from the area's own site, the land manager, or news, so this may not meet the rule. Should probably be unverified unless David accepts it. |
| station-de-ski-val-direne-qc | 028 | Coordinates in Charlevoix; the real place is in Sainte-Irène, Matapédia. Operating/low; no comment set. |
| centre-de-plein-air-de-saint-simeon-qc | 028 | Coordinates near Saint-Jean-sur-Richelieu; the real place is in Saint-Siméon, Gaspésie. Operating/low; no comment set. |
| centre-de-plein-air-sainte-adele-qc | 029 | Coordinates (52.94, -73.55) are far north of Sainte-Adèle. Coordinates likely wrong. |
| lenvolee-centre-recreotouristique-qc | 030 | Coordinates in Rhode Island (41.58, -71.48); real address is Saint-Lin-Laurentides, QC. |
| weyburn-cross-country-ski-trails-sk | 035 | Recorded closed/medium. City page says Weyburn no longer has groomed trails, but also that skiers may still break their own trail. The rules conflict here ("no longer maintained" counts as closed; "skiing allowed, ungroomed" counts as operating/medium). David to decide. |
| (buena vista, batch 044) | 044 | Evidence date set to the check date because the page shows a "2026-2027 Winter Season" banner, not a real page date. Operating is fine; the date is invented. |
| fort-snelling-state-park-mn, frontenac-state-park-mn, nerstrand-big-woods-state-park-mn, sibley-state-park-mn | 038 | Left unverified because the DNR park pages are script-built. These parks are known to have ski trails. Redo candidates: the brief now has a DNR workaround. |
| lakes-of-the-north-mi | 037 | Community is in Mancelona, about 45 km from the stored coordinates. Coordinates likely wrong. |
| (Lake Emily, batch 066) | 066 | Only evidence is a Feb 2025 county notice closing the trail for a timber harvest; no 2025-26 status found. Check whether operating is justified. |
| (Enchanted Forest NM, batch 068) | 068 | News (2026-10-03): will not open for 2026-27, sale fell through, may return under a new operator. Own site still sells passes. Recorded unverified. |
| river-road-ski-trail-wi | 064 | Closed/low. Chippewa County page says the trail "is now closed", with no date, reason or "permanent" wording. Could be seasonal; unverified is also defensible. |
| land-o-lakes-trail-mi | 064 | Region says MI but the coordinates and sources put it in Land O' Lakes, WI. Region may be wrong. |
| alpine-lake-wv | 069 | Closed/low from a travel-article editor's note seen only in a snippet (page 403). Resort site lists no winter amenities. Weak closure source. |
| rock-creek-ca | 069 | Coordinates (~39.9N) far from the real Rock Creek in Mono/Inyo County (~37.5N). |
| north-woods-pa | 069 | Coordinates near Valley Forge; the real place is in the Laurel Highlands. |
| (White Pass WA, batch 078) | 078 | Stored longitude is -102.644, far from White Pass (about -121.4). Coordinates wrong. |
| latigo-co | 075-077 | Coordinates near Colorado Springs; Latigo Ranch is near Kremmling. |
| mountain-trails-ny | 075-077 | Coordinates in Colorado; the place is in Tannersville, NY. |
| pineridge-ny | 075-077 | Probably East Poestenkill, about 150 km from the stored coordinates. Unverified. |
| (Highland National Golf Course, batch 081-083) | 081-083 | Evidence date 2026-12-11 is in the future; probably a scheduled event, not a dated report. Check the date and confidence. |
