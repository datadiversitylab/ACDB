###################
# check_site.R
#
# Checks the website that site/build_site.R wrote in _site/, before it is published.
# Run it from the project folder after the build: Rscript tests/check_site.R
# Errors fail the run, so a broken site is never published.
###################

errors <- character()
fail <- function(...) errors <<- c(errors, paste0(...))
show <- function(x, n = 8) paste0(paste(head(x, n), collapse = ", "), if (length(x) > n) paste0(" and ", length(x) - n, " more") else "")

out <- "_site"
site_url <- "https://datadiversitylab.github.io/ACDB/"

finish <- function() {
  for (m in errors) cat("::error::", m, "\n", sep = "")
  summary_lines <- c("### Website checks", "", if (length(errors) == 0) "No errors." else paste0("- ", errors))
  if (nzchar(Sys.getenv("GITHUB_STEP_SUMMARY"))) write(summary_lines, Sys.getenv("GITHUB_STEP_SUMMARY"), append = TRUE)
  cat(length(errors), if (length(errors) == 1) "error\n" else "errors\n")
  quit(status = if (length(errors) > 0) 1 else 0)
}

if (!dir.exists(out)) {
  fail(out, " is missing. Run Rscript site/build_site.R first.")
  finish()
}

# Every page of the app and the files the pages load
pages <- c("index.html", "map.html", "populations.html", "tree.html", "cards.html", "statistics.html",
           "citations.html", "papers.html", "about.html", "contribute.html", "help.html", "404.html", "sitemap.xml")
missing <- pages[!file.exists(file.path(out, pages))]
if (length(missing) > 0) fail("Pages missing: ", show(missing), ".")

assets <- c("data.js", "site.js", "static.css", "acdb.css", "card.css", "photos.js", "tree.js", "print.js", "DDLLogo_white_small.png")
missing <- assets[!file.exists(file.path(out, "assets", assets))]
if (length(missing) > 0) fail("Files missing in assets: ", show(missing), ".")
if (file.exists(file.path(out, "assets", "data.js")) && !startsWith(readLines(file.path(out, "assets", "data.js"), n = 2, warn = FALSE)[2], "window.ACDB = {")) {
  fail("assets/data.js does not start with window.ACDB = {.")
}

# One page per species in the database
if (requireNamespace("RSQLite", quietly = TRUE) && file.exists("db/ACDB_dev.sql")) {
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), "db/ACDB_dev.sql")
  ids <- RSQLite::dbGetQuery(conn, "select species_id from species")$species_id
  RSQLite::dbDisconnect(conn)
  missing <- ids[!file.exists(file.path(out, "species", paste0(ids, ".html")))]
  if (length(missing) > 0) fail("Species without a page: ", show(missing), ".")
}

# The sitemap lists only pages that exist
if (file.exists(file.path(out, "sitemap.xml"))) {
  lines <- readLines(file.path(out, "sitemap.xml"), warn = FALSE)
  urls <- regmatches(lines, regexpr("(?<=<loc>)[^<]+", lines, perl = TRUE))
  paths <- sub("^$", "index.html", sub(site_url, "", urls, fixed = TRUE))
  missing <- paths[!file.exists(file.path(out, paths))]
  if (length(missing) > 0) fail("The sitemap lists pages that do not exist: ", show(missing), ".")
}

# Every page has what search engines read, and every link inside the site leads to a file
html_files <- list.files(out, pattern = "\\.html$", recursive = TRUE)
broken <- character()
for (f in html_files) {
  text <- paste(readLines(file.path(out, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl("<title>[^<]+</title>", text)) fail(f, " has no title.")
  if (!grepl("<meta name=\"description\" content=\"[^\"]+\"", text)) fail(f, " has no description.")
  if (!grepl("<link rel=\"canonical\"", text)) fail(f, " has no canonical link.")
  if (grepl("noindex", text, fixed = TRUE)) fail(f, " asks search engines not to index it.")
  if (f == "404.html") next
  targets <- regmatches(text, gregexpr("(?<=href=\"|src=\")[^\"]+", text, perl = TRUE))[[1]]
  targets <- targets[!grepl("^(https?:|mailto:|#|data:)", targets)]
  targets <- unique(sub("[?#].*$", "", targets))
  targets <- targets[targets != ""]
  resolved <- normalizePath(file.path(out, dirname(f), targets), mustWork = FALSE)
  gone <- targets[!file.exists(resolved)]
  if (length(gone) > 0) broken <- c(broken, paste0(f, " -> ", gone))
}
if (length(broken) > 0) fail(length(broken), " broken links inside the site: ", show(broken), ".")

cat("Checked", length(html_files), "pages.\n")
finish()
