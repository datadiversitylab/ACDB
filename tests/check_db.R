###################
# check_db.R
#
# Checks that the database and the files around it meet what the app expects.
# Run it from the project folder: Rscript tests/check_db.R
# Errors would break or mislead the app, and they fail the run.
# Warnings are worth a look, and they do not fail the run.
###################

library(RSQLite)

errors <- character()
notes <- character()
fail <- function(...) errors <<- c(errors, paste0(...))
warn <- function(...) notes <<- c(notes, paste0(...))

# Helpers
blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
show <- function(x, n = 8) paste0(paste(head(x, n), collapse = ", "), if (length(x) > n) paste0(" and ", length(x) - n, " more") else "")
plural <- function(n, one, many = paste0(one, "s")) paste(n, if (n == 1) one else many)

# What the app reads
required <- list(
  species = c("species_id", "common_name", "GBIF", "canonicalName", "Genus", "Family", "Ordr", "Class", "Phylum",
              "primary_social_unit", "unit_evidence", "unit_source", "IUCN"),
  groups = c("group_id", "species_id", "group_name", "group_level", "group_above", "size", "size_evidence",
             "size_date", "size_source", "location_evidence", "location_source", "lat", "long"),
  behaviors = c("behavior_id", "behavior", "group_id", "behavior_description", "behavior_source", "start_date",
                "end_date", "vertical", "vertical_evidence", "vertical_source", "horizontal", "horizontal_evidence",
                "horizontal_source", "oblique", "oblique_evidence", "oblique_source", "domains", "domains_evidence",
                "domains_source", "anth_effects", "anth_source"),
  sources = c("source_id", "title", "year", "authors", "doi", "methods")
)
known_classes <- c("Aves", "Mammalia", "Insecta")
known_iucn <- c("LC", "NT", "VU", "EN", "CR", "DD", "EW", "EX")
known_transmission <- c("yes", "no", "inferred yes", "inferred no", "maybe", "possible", "maybe/not clear", "unclear", "suspected unknown")
known_domains <- c("antipredation", "architecture", "foraging", "mating", "migration", "other", "play", "social", "vocal communication")
placeholders <- c("", "not searched", "suspected unknown", "not applicable")
asset_files <- c("acdb.css", "card.css", "photos.js", "tree.js", "print.js", "DDLLogo_white.png", "DDLLogo_white_small.png")

finish <- function() {
  cat("\n")
  for (m in errors) cat("::error::", m, "\n", sep = "")
  for (m in notes) cat("::warning::", m, "\n", sep = "")
  lines_out <- c(
    "### Database checks",
    "",
    paste0("**", plural(length(errors), "error"), "**, ", plural(length(notes), "warning"), "."),
    "",
    if (length(errors) > 0) c("Errors", paste0("- ", errors), ""),
    if (length(notes) > 0) c("Warnings", paste0("- ", notes))
  )
  out <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(out)) write(lines_out, out, append = TRUE)
  cat(plural(length(errors), "error"), ", ", plural(length(notes), "warning"), "\n", sep = "")
  quit(status = if (length(errors) > 0) 1 else 0)
}

# Files and version
if (!file.exists("VERSION")) {
  fail("VERSION is missing.")
} else if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", trimws(readLines("VERSION", warn = FALSE)[1]))) {
  fail("VERSION must look like 1.2.3.")
}

for (f in asset_files) if (!file.exists(file.path("www", f))) fail("www/", f, " is missing, and the app loads it.")

# The app files must be valid R
for (f in c("ui.R", "server.R", "app.R")) {
  if (!file.exists(f)) {
    fail(f, " is missing.")
  } else {
    parsed <- tryCatch(parse(file = f), error = function(e) e)
    if (inherits(parsed, "error")) fail(f, " does not parse: ", conditionMessage(parsed))
  }
}

# Papers that are not coded yet
if (!file.exists("db/uncoded_papers.csv")) {
  warn("db/uncoded_papers.csv is missing. The New papers tab will be empty.")
} else {
  header <- trimws(sub("^﻿", "", readLines("db/uncoded_papers.csv", n = 1, warn = FALSE, encoding = "UTF-8")))
  if (!identical(header, "authors,year,title,doi")) warn("db/uncoded_papers.csv should start with the header authors,year,title,doi.")
}

# Database
db_path <- "db/ACDB_dev.sql"
if (!file.exists(db_path)) {
  fail(db_path, " is missing.")
  finish()
}
conn <- dbConnect(SQLite(), dbname = db_path)
present <- dbListTables(conn)
tables <- list()
for (t in names(required)) {
  if (!t %in% present) {
    fail("Table ", t, " is missing.")
    next
  }
  tables[[t]] <- dbReadTable(conn, t)
  missing <- setdiff(required[[t]], colnames(tables[[t]]))
  if (length(missing) > 0) fail("Table ", t, " has no column ", show(missing), ".")
}
dbDisconnect(conn)
if (length(errors) > 0) finish()

species <- tables$species
groups <- tables$groups
behaviors <- tables$behaviors
sources <- tables$sources

# Identifiers. They end up in links and in JavaScript, so they must be plain
for (t in names(required)) {
  id <- tables[[t]][[required[[t]][1]]]
  if (any(blank(id))) fail(t, ": ", plural(sum(blank(id)), "empty id"), ".")
  dup <- unique(id[duplicated(id) & !blank(id)])
  if (length(dup) > 0) fail(t, ": repeated ids ", show(dup), ".")
}
odd <- species$species_id[!grepl("^[A-Za-z0-9_.-]+$", species$species_id)]
if (length(odd) > 0) fail("species_id may only have letters, numbers, and _ . -. Check ", show(odd), ".")
odd <- groups$group_id[is.na(suppressWarnings(as.integer(groups$group_id)))]
if (length(odd) > 0) fail("group_id must be a whole number. Check ", show(odd), ".")

# Links between tables
orphans <- unique(groups$species_id[!groups$species_id %in% species$species_id])
if (length(orphans) > 0) fail("Groups point to species that do not exist: ", show(orphans), ".")
orphans <- unique(behaviors$group_id[!behaviors$group_id %in% groups$group_id])
if (length(orphans) > 0) fail("Behaviors point to groups that do not exist: ", show(orphans), ".")

# Sources. The behavior and species columns feed the species window and the cards
used_in <- list(
  behaviors = c("behavior_source", "vertical_source", "horizontal_source", "oblique_source", "domains_source", "anth_source"),
  species = "unit_source",
  groups = c("size_source", "location_source")
)
cited <- character()
for (t in names(used_in)) {
  for (col in used_in[[t]]) {
    x <- as.character(tables[[t]][[col]])
    x <- x[!is.na(x)]
    cited <- c(cited, x)
    bad <- unique(x[!x %in% sources$source_id & !tolower(trimws(x)) %in% placeholders])
    if (length(bad) > 0) {
      msg <- paste0(t, ".", col, " cites sources that are not in the sources table: ", show(bad), ".")
      if (t == "groups") warn(msg) else fail(msg)
    }
  }
}
unused <- setdiff(sources$source_id, cited)
if (length(unused) > 0) warn(plural(length(unused), "source"), " not cited anywhere: ", show(unused), ".")
bad <- sources$source_id[blank(sources$authors) | blank(sources$title)]
if (length(bad) > 0) fail("Sources without authors or title: ", show(bad), ".")
bad <- sources$source_id[is.na(suppressWarnings(as.integer(sources$year)))]
if (length(bad) > 0) warn("Sources without a year: ", show(bad), ".")
bad <- sources$source_id[blank(sources$doi)]
if (length(bad) > 0) warn(plural(length(bad), "source"), " without a DOI: ", show(bad), ".")

# Taxonomy, used by the tree, the cards, and the statistics
for (col in c("canonicalName", "common_name", "Genus", "Family", "Ordr", "Class", "Phylum")) {
  bad <- species$species_id[blank(species[[col]])]
  if (length(bad) > 0) fail("Species without ", col, ": ", show(bad), ".")
}
bad <- setdiff(unique(species$Class), known_classes)
if (length(bad) > 0) warn("New class ", show(bad), ". It works, but the tree and the cards show it in a neutral color.")
bad <- unique(species$IUCN[!is.na(species$IUCN) & !species$IUCN %in% known_iucn])
if (length(bad) > 0) warn("Unknown IUCN codes: ", show(bad), ". The app shows them as not evaluated.")
bad <- unique(species$canonicalName[duplicated(species$canonicalName)])
if (length(bad) > 0) warn("The same scientific name is used for more than one species: ", show(bad), ".")
bad <- species$species_id[!species$species_id %in% groups$species_id]
if (length(bad) > 0) warn(plural(length(bad), "species", "species"), " without populations: ", show(bad), ".")

# Populations and their places
lat <- suppressWarnings(as.numeric(groups$lat))
long <- suppressWarnings(as.numeric(groups$long))
bad <- groups$group_id[(!is.na(groups$lat) & is.na(lat)) | (!is.na(groups$long) & is.na(long))]
if (length(bad) > 0) fail("lat and long must be numbers. Check groups ", show(bad), ".")
bad <- groups$group_id[which(lat < -90 | lat > 90 | long < -180 | long > 180)]
if (length(bad) > 0) fail("lat or long out of range in groups ", show(bad), ". Latitude goes from -90 to 90, longitude from -180 to 180.")
bad <- groups$group_id[is.na(lat) | is.na(long)]
if (length(bad) > 0) warn(plural(length(bad), "population"), " without coordinates, so not on the map: groups ", show(bad), ".")
bad <- groups$group_id[blank(groups$group_name)]
if (length(bad) > 0) fail("Groups without a name: ", show(bad), ".")
bad <- groups$group_id[!groups$group_id %in% behaviors$group_id]
if (length(bad) > 0) warn(plural(length(bad), "population"), " without behaviors: groups ", show(bad), ".")
key <- paste(groups$species_id, tolower(trimws(groups$group_name)))
bad <- unique(groups$group_name[duplicated(key)])
if (length(bad) > 0) warn("Group names repeated within a species: ", show(bad), ".")

# Behaviors
bad <- behaviors$behavior_id[blank(behaviors$behavior)]
if (length(bad) > 0) fail("Behaviors without a name: ", show(bad), ".")
bad <- behaviors$behavior_id[blank(behaviors$domains)]
if (length(bad) > 0) fail("Behaviors without a domain: ", show(bad), ".")
bad <- behaviors$behavior_id[blank(behaviors$behavior_description)]
if (length(bad) > 0) warn("Behaviors without a description: ", show(bad), ".")
domains <- unique(trimws(unlist(strsplit(as.character(behaviors$domains), "[;,]"))))
domains <- domains[!is.na(domains) & domains != ""]
bad <- setdiff(domains, known_domains)
if (length(bad) > 0) warn("New behavior domains: ", show(bad), ". They work, but the colors repeat after 10 domains.")
if (length(unique(c(domains, known_domains))) > 10) warn("There are more than 10 domains. Colors will repeat.")
for (col in c("vertical", "horizontal", "oblique")) {
  x <- tolower(trimws(as.character(behaviors[[col]])))
  bad <- unique(x[!is.na(x) & x != "" & !x %in% known_transmission])
  if (length(bad) > 0) warn(col, " has new answers: ", show(bad), ". The app shows them as unclear.")
}

# The CSV tables in the release must match the database
for (t in names(required)) {
  f <- file.path("db/table_csvs", paste0(t, ".csv"))
  if (!file.exists(f)) {
    fail(f, " is missing. The release zip needs it.")
    next
  }
  csv <- tryCatch(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(csv)) {
    fail(f, " cannot be read.")
  } else {
    names(csv)[1] <- sub("^﻿", "", names(csv)[1])
    if (!identical(colnames(csv), colnames(tables[[t]]))) {
      fail(f, " has different columns than the database. Export it again.")
    } else if (nrow(csv) != nrow(tables[[t]]) || !setequal(as.character(csv[[1]]), as.character(tables[[t]][[1]]))) {
      fail(f, " has different rows than the database (", nrow(csv), " and ", nrow(tables[[t]]), "). Export it again.")
    }
  }
}

cat("Checked", nrow(species), "species,", nrow(groups), "populations,", nrow(behaviors), "behaviors, and", nrow(sources), "sources.\n")
finish()
