# Animal Culture Database (ACDB)

The **Animal Culture Database (ACDB)** is a comparative database of cultural behaviors across nonhuman animal species. It synthesizes current literature on intra-species behavioral variation in contexts such as communication, foraging, and migration into a relational database accessible through an open-access Shiny app. A data paper on the first version of the ACDB can be found [here](https://www.nature.com/articles/s41597-025-05315-y). 
 


## Overview

The **ACDB** aims to:

- **Document socially transmitted behaviors**: Record instances of cultural behaviors observed in wild animal populations.
- **Analyze cultural diversity**: Examine patterns of cultural diversity globally and across different species.
- **Inform conservation research**: Incorporate knowledge of socially transmitted behaviors to better inform biodiversity conservation policies.

## Features

- **Interactive map**: Visualize the geographic distribution of recorded populations with cultural behaviors.
- **Search and filter**: Search and filter data by species, behavior types, transmission modes, and other variables.



## Getting Started

### Access the Shiny App

You can access the ACDB Shiny app [here](https://datadiversitylab.github.io/ACDB/). The app provides an interactive interface to explore the database.

### Repository Contents

- `app.R`: The Shiny app script.
- `db/`: Dataset files used by the app.
- `www/`: Additional resources for the app (images, stylesheets).
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

