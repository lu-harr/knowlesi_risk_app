# Enhancements to the *P. knowlesi* risk app — change notes

Upgrades to [lu-harr/knowlesi_risk_app](https://github.com/lu-harr/knowlesi_risk_app)
(Tobin et al. 2024, PLOS NTD, [PMID 38252650](https://pubmed.ncbi.nlm.nih.gov/38252650/)).
The data surfaces (`risk_masked.tif`, `idn_districts.shp`) are unchanged.

## Synced with upstream (Nov 2026)

Merged Lucy's latest app.R (to commit d6adf5e): "transmission suitability"
wording, horizontal legend, 2 decimal places, colour-graded histograms,
html2canvas PNG export (`www/`), basemap/label switching. Label tiles now
use Esri (no API key) for all basemaps.

## Files

- `app.R` — replaces the original; all new features are here.
- `prepare_district_stats.R` — **new**; run once to precompute per-district stats.
- `data_setup.R` — original with small fixes (loads `dplyr`, sets the
  `prediction_mean`/`prediction_variance` layer names, creates `data/` if missing).

## What changed

- **Entry pop-up** — model summary (Lucy's wording),
  shown **bilingually (English + Bahasa Indonesia together)**; reopenable via
  *About this map*. PMID link also under the title and in the Methods panel.
- **Basemap + layers** — switch clean / street / satellite; **Show risk layer**
  toggle, **Place & street labels on top** toggle, and an **opacity slider** to
  read landmarks against the risk.
- **Per-district values** — click or search a district for its **mean risk**,
  **uncertainty** and **range** (zonal averages), in the sidebar and a map popup.
- **Within-district distribution** — histogram of the district's risk cells with
  the mean marked.
- **EN ⇄ ID toggle** — retranslates all labels, legends, popups and plots;
  strings live in the `i18n` dictionary near the top of `app.R`.
- **Subdistrict (kecamatan) filtering** — once a district is selected its
  subdistrict outlines are drawn on the map and a *Filter to a subdistrict*
  dropdown appears; clicking or choosing one shows its mean risk, uncertainty,
  range and histogram (area-weighted, admin-3 boundaries from malariaAtlas),
  plus a per-district subdistrict CSV export. Built by
  `prepare_subdistrict_stats.R`; hidden if those files are absent.
- **Risk list category (SOP)** — third *Display* option colouring districts
  by the draft SOP's categories: evidence first (Cat. 1 confirmed / Cat. 2
  probable, from `data/district_evidence.csv`), then the map adds Cat. 3
  (receptive) and a watch list. Map tier = share of area at or above a
  suitability value (`risk_rule` in `app.R`), not the district mean. Areas
  outside the model's macaque/vector mask are shown grey as "outside model"
  instead of as risk 0 (Sabang, Tarakan, Manado...). Sidebar/popups show
  category, tier, model coverage, API and Pk case counts. **Risk list by area
  (SOP annex, CSV)** export lists every district plus its high/moderate
  kecamatan. Methods panel reports how many confirmed districts the rule recovers.
- **`data/district_evidence.csv`** — seeded from the draft SOP v5 case table
  (28 SISMAL districts) and research sites; the `api`/`api_year` columns are
  empty until SISMAL data is supplied. Edit this file yearly.
- **Pk incidence and Kemenkes API** — two extra *Display* options, using
  Indonesian MoH (Kemenkes/SISMAL) definitions: (1) Pk incidence per 1,000
  population/yr, 3-year median, as in the draft SOP stratification (>= 1 =
  high risk); (2) API as reported by Kemenkes: confirmed cases (microscopy/
  RDT), all species, per 1,000/yr, banded by Kemenkes endemicity strata
  (high >= 5, moderate 1-<5, low >0-<1). Both appear in popups, the sidebar
  (with the SOP stratum), the risk-list export and a definitions note in
  Methods. Fill `pk_api`, `pk_api_years`, `api`, `api_year` in
  `data/district_evidence.csv`; districts without data show grey.
- **Subdistrict risk list category** — kecamatan popups, sidebar card and
  the subdistrict CSV show the same risk-list category, Pk incidence and API
  as districts. Values come from optional `data/subdistrict_evidence.csv`
  (kecamatan/puskesmas data from SISMAL); where a kecamatan has none, the
  district's category/values are shown and flagged "from district".
- **Published research evidence (PCR-confirmed Pk)** — new
  `data/published_pk_records.csv` (79 study x location records from 2010-2025
  papers: Lubis, Herdiana, Coutrier, Ompusunggu, Setiadi, Gunawan, Salwati,
  Sugiarto, Fauziah and others; see README next to it). Records are matched to
  kecamatan/district; human, PCR-confirmed, non-imported cases make their
  district category 1 and, where located to village/kecamatan, mark that
  kecamatan "confirmed here". New *Display* option **Pk evidence: MoH vs
  research** (district fill by evidence basis + clickable case-site markers
  with citations). Evidence basis and sources shown in popups, sidebar,
  risk-list export and the Excel appendix. Adds Batu Bara, Nias Selatan, Tanah
  Laut, Gunung Mas, Kutai Kartanegara, Kutai Barat, Berau and Bungo as
  category 1. Negative PCR records are kept for a later map. Rows are
  AI-extracted and need verifying (`verified_by`).
- **Malaria elimination status layer** — *Display* option colouring
  districts as certified eliminated / not yet eliminated / not supplied, with
  status in popups, sidebar and the risk-list export. Eliminated districts
  that are on the Pk risk list get a note that Pk is not counted in
  elimination, so every positive needs PCR. Data in
  `data/elimination_status.csv`, seeded only from public Kemenkes sources:
  province certificates (Banten, DKI Jakarta, Jawa Barat, Jawa Timur, Bali by
  Jun 2024; Sumatera Selatan, DIY 28 Jul 2026), named district certificates
  (17 on 24 Jun 2024; 7 on 28 Jul 2026) and the 14 high-burden Papua
  districts in KMK HK.01.07/MENKES/1988/2024. 132 of ~414 eliminated
  districts are listed; replace with the full Kemenkes list when available.
- **Basemaps without API keys** — CARTO tiles now need a key, so "Light grey"
  and the labels overlay use Esri tiles. Street map is the default.
- **Filter usability** — reset link and blank "all" options with clear (x)
  buttons on the district and subdistrict dropdowns; optional *Show
  subdistrict boundaries* layer (drawn for the visible area when zoomed in).
- **Extras** — district search box; per-district CSV export (+ PNG if
  `leaflet.extras2` is installed); Methods & citation panel; mobile layout.

## Dependencies

Same as the base app (`shiny`, `leaflet`, `terra`, `sf`, `viridisLite`).
Optional `leaflet.extras2` enables the PNG export button.

## Run

```r
Rscript prepare_district_stats.R      # once: writes data/district_stats.rds + district_cells.rds
Rscript prepare_subdistrict_stats.R   # once: downloads admin-3 boundaries, writes data/*subdistrict*.rds
# both scripts read data/prediction_masking.tif (if present) for model coverage
shiny::runApp()
```

If the `.rds` files are absent the app computes them on first launch (slower start).


## Notes

Run locally against the real data (516 districts, WGS84 raster with
`prediction_mean`/`prediction_variance`). Code assumes shapefile columns
`name_2` (district) and `name_1` (province).
