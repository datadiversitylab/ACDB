# Animal Culture Database (ACDB)

The **Animal Culture Database (ACDB)** is a comparative database of cultural behaviors across nonhuman animal species. It synthesizes current literature on intra-species behavioral variation in contexts such as communication, foraging, and migration into a relational database accessible through an open-access website. A data paper on the first version of the ACDB can be found [here](https://www.nature.com/articles/s41597-025-05315-y). 
 


## Overview

The **ACDB** aims to:

- **Document socially transmitted behaviors**: Record instances of cultural behaviors observed in wild animal populations.
- **Analyze cultural diversity**: Examine patterns of cultural diversity globally and across different species.
- **Inform conservation research**: Incorporate knowledge of socially transmitted behaviors to better inform biodiversity conservation policies.

## Features

- **Interactive map**: Visualize the geographic distribution of recorded populations with cultural behaviors.
- **Search and filter**: Search and filter data by species, behavior types, transmission modes, and other variables.



## Getting Started

### Website

The ACDB website is at [datadiversitylab.github.io/ACDB](https://datadiversitylab.github.io/ACDB/). It has every page of the former Shiny app (map, populations, tree of life, species cards, statistics, citations) and one page per species, so search engines can find the data.

The website is built from the database by `site/build_site.R` and published by the `site` workflow whenever `db/`, `site/`, `www/`, or `VERSION` change. Nothing needs to be done by hand. To build it on your computer, run `Rscript site/build_site.R` from the project folder (it needs RSQLite) and open the pages in `_site/` with a local server, for example `python3 -m http.server --directory _site`.

### Repository Contents

- `site/build_site.R`: Builds the website from the database.
- `site/site.js`, `site/static.css`: Map, filters, and species pages in the browser.
- `site/vendor/`: Fixed copies of the libraries the website uses.
- `www/`: Styles, scripts, and logos shared by the website and the Shiny app.
- `app.R`, `ui.R`, `server.R`: The Shiny app.
- `db/`: Dataset files.
- `README.md`: This file.
- `LICENSE`: License information.

## Planned updates and expansions

We are continuing to integrate data and plan to cover the majority of existing animal behavior literature for version 2.0, as well as developing a pipeline for external data contributions and updates. A description of each table with variable definitions, as well as planned changes, can be viewed in the ACDB_codebook_2026 file. 

## Versioning

Major releases (e.g. v1.0.0, v2.0.0) are based on the addition of 100 or more populations. Minor releases (e.g. v1.1.0, v1.2.0) are based on the addition of fewer than 100 populations. Changes to the contents of the database (e.g. inclusion of a missing paper, fixing taxonomy, adding locations) are treated as patches (e.g. v1.1.1, v1.1.2).

To publish a release, open the Actions tab, run the release workflow, and pick patch, minor, or major. The workflow raises the number in `VERSION`, tags it, and attaches a zip with the SQLite database, the table CSVs, and the codebook. It warns if the bump does not match the number of new populations since the last release.

## Checks

The workflow `check-db` tests the database and the app files on every pull request and push that touches them, and before every release. It looks for broken links between tables, missing sources, bad coordinates, empty taxonomy, stale table CSVs, and R files that do not parse. Errors fail the run. Warnings, like a population without coordinates, only show up in the summary. To run it on your computer, use `Rscript tests/check_db.R` from the project folder.

## Contribution guidelines

We welcome contributions from researchers to expand and update the database. We are working on developing a standardized pipeline for users to submit their own data. For now, please contact `kcb7@arizona.edu` if you have data you wish to submit. 

## License

This project is licensed under the [MIT License](LICENSE).

## Local render

Please use the following code from the terminal
```
python3 -m http.server 8000 --directory _site
```