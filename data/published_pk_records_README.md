# `published_pk_records.csv` — published *P. knowlesi* records in Indonesia

One row per **study × location**, at the finest level the paper reports. The
app matches each row to a kecamatan / district and uses it alongside the MoH
(SISMAL) evidence in `district_evidence.csv`.

| column | meaning |
|---|---|
| `source_short` | short citation, e.g. `Lubis 2017` |
| `citation`, `doi` | full reference and DOI |
| `host` | `human`, `macaque` or `mosquito` |
| `diagnostic` | e.g. `PCR (nested 18S)`, `microscopy only` |
| `sampling_years` | when samples were collected |
| `n_tested`, `n_pk_positive` | as written (ranges allowed, e.g. `114-148`) |
| `province`, `district`, `kecamatan` | names **as stated in the paper** (blank if not stated) |
| `site_text` | location exactly as written in the paper |
| `lat`, `lon` | only if the paper / supplement gives coordinates |
| `location_precision` | `point`, `village`, `kecamatan`, `district`, `province`, `unknown` |
| `case_origin` | `local`, `imported`, `unknown` (place of **infection** goes in the location columns) |
| `where_in_paper` | table / figure / section, for checking |
| `notes` | anything uncertain |
| `verified_by` | initials + date once someone has checked the row against the paper |

## How the app uses it

- A row counts as **research-confirmed transmission** when: host = human,
  diagnostic is molecular (PCR/LAMP/sequencing), ≥ 1 positive, and the case is
  not imported.
- Its **district** becomes risk-list category 1, with evidence basis
  "research" (or "MoH + research" if SISMAL also confirms it).
- If the row is located to **point / village / kecamatan**, that kecamatan is
  marked **"confirmed here"**; other kecamatan in the district stay
  "from district".
- Province-only rows (e.g. Fauziah et al. 2025 by province) stay in the table
  but do not change any district until district names are known.
- Matching: coordinates first (nearest kecamatan within ~5 km for coastal
  points), then district name + province, then kecamatan name within the
  district. Check the `matched_by` column in the app's records layer.

Rows were extracted from the papers with AI assistance and must be checked
against the source (`where_in_paper`) before use in the guidelines; fill
`verified_by` as rows are confirmed.
