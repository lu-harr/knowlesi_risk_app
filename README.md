## Zoomable map of *Plasmodium knowlesi* risk

An interactive RShiny app showing mapped risk estimates described in Tobin et al.: <https://doi.org/10.1371/journal.pntd.0011570>

Surfaces downloadable from <https://osf.io/k5bsa/overview>

Run the app with:

```r
shiny::runApp()
```

Deployed on Posit Connect Cloud [here](https://019f600f-482a-3ea7-f415-6f5bcf66d6ce.share.connect.posit.cloud/).
## Indonesia SOP extension (subdistricts, risk list, API, evidence)

Adds subdistrict (kecamatan) filtering, the draft Indonesian Pk SOP risk list
categories, Kemenkes API / Pk incidence and elimination status layers, and
published PCR-confirmed records. See `CHANGES.md` for details.

Build the data once (surfaces from OSF k5bsa in `data/` first):

```r
source("data_setup.R")                 # risk_masked.tif, idn_districts.shp
source("prepare_district_stats.R")     # district_stats.rds, district_cells.rds
source("prepare_subdistrict_stats.R")  # subdistrict boundaries and stats
shiny::runApp()
source("make_appendix.R")              # optional: Excel appendix of relative suitability
```

The curated tables in `data/` (`district_evidence.csv`,
`subdistrict_evidence.csv`, `elimination_status.csv`,
`published_pk_records.csv`) are tracked and should be updated yearly.
