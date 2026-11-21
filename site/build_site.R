###################
# build_site.R
#
# Builds the ACDB website in _site/ from the database. Every tab of the app is a page, and every
# species has its own page, so search engines can index them. The browser adds the map, filters,
# tree, and PDF cards (site/site.js and www/). Only base R and RSQLite are needed.
# Run from the project folder: Rscript site/build_site.R
###################

library(RSQLite)

out <- "_site"
site_url <- "https://datadiversitylab.github.io/ACDB/"
paper_url <- "https://doi.org/10.1038/s41597-025-05315-y"
repo_url <- "https://github.com/datadiversitylab/ACDB"
acdb_version <- trimws(readLines("VERSION", warn = FALSE)[1])

# Data -------------------------------------------------------------------------

conn <- dbConnect(SQLite(), "db/ACDB_dev.sql")
groups_table <- dbReadTable(conn, "groups")
species_table <- dbReadTable(conn, "species")
behaviors_table <- dbReadTable(conn, "behaviors")
sources_table <- dbReadTable(conn, "sources")
dbDisconnect(conn)

# A few cells are stored as Latin-1 instead of UTF-8
fix_encoding <- function(d) {
  for (col in names(d)) {
    if (is.character(d[[col]])) {
      bad <- !is.na(d[[col]]) & !validUTF8(d[[col]])
      d[[col]][bad] <- iconv(d[[col]][bad], "latin1", "UTF-8")
    }
  }
  d
}
groups_table <- fix_encoding(groups_table)
species_table <- fix_encoding(species_table)
behaviors_table <- fix_encoding(behaviors_table)
sources_table <- fix_encoding(sources_table)

# IDs as text, so that 700000 is never written as 7e+05
id_text <- function(x) sprintf("%.0f", as.numeric(x))

# GBIF keys are numbers (GBIF Backbone) or letters and digits (Catalogue of Life), both go in the link as they are
gbif_key <- function(x) if (is.numeric(x)) id_text(x) else as.character(x)

# Same definitions as server.R ---------------------------------------------------

acdb_palette <- c("#E76F51", "#3A86C8", "#F2A93B", "#1B7F79", "#7B5EA7",
                  "#6A9B2F", "#D6457A", "#4F5BD5", "#A9713A", "#5F7480")
slot <- function(i) (i - 1) %% length(acdb_palette) + 1

class_names <- c(Aves = "Birds", Mammalia = "Mammals", Insecta = "Insects")
iucn_names <- c(LC = "Least Concern", NT = "Near Threatened", VU = "Vulnerable", EN = "Endangered",
                CR = "Critically Endangered", DD = "Data Deficient", EW = "Extinct in the Wild", EX = "Extinct")
iucn_colors <- c(LC = "#60C659", NT = "#CCE226", VU = "#F9E814", EN = "#FC7F3F",
                 CR = "#D81E05", DD = "#D1D1C6", EW = "#542344", EX = "#000000")
region_order <- c("North America", "Central & South America", "Europe", "Africa", "Asia",
                  "Oceania", "Antarctica", "Open ocean", "No location")
source_cols <- c("behavior_source", "vertical_source", "horizontal_source",
                 "oblique_source", "domains_source", "anth_source")

cap <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
has_text <- function(x) length(x) == 1 && !is.na(x) && nzchar(trimws(as.character(x)))
title_case <- function(x) tools::toTitleCase(ifelse(is.na(x), "", as.character(x)))
class_label <- function(x) ifelse(is.na(class_names[x]), x, unname(class_names[x]))

split_domains <- function(x) {
  d <- unique(trimws(unlist(strsplit(as.character(x), "[;,]"))))
  d[!is.na(d) & d != ""]
}
has_domain <- function(domains, dm) vapply(strsplit(domains, "; "), function(x) dm %in% x, logical(1))

tstate <- function(x) {
  x <- tolower(trimws(ifelse(is.na(x), "", as.character(x))))
  if (x %in% c("", "suspected unknown")) return(c("unknown", "Unknown"))
  if (x == "yes") return(c("yes", "Yes"))
  if (x == "inferred yes") return(c("yes", "Inferred yes"))
  if (x == "no") return(c("no", "No"))
  if (x == "inferred no") return(c("no", "Inferred no"))
  if (x %in% c("maybe", "possible", "maybe/not clear")) return(c("maybe", "Possible"))
  c("maybe", "Unclear")
}

lead_text <- function(x, n = 300) {
  x <- as.character(x)
  if (!has_text(x)) return("")
  first <- trimws(strsplit(x, "\"")[[1]][1])
  if (is.na(first) || nchar(first) < 20) first <- trimws(x)
  if (nchar(first) > n) first <- paste0(sub("\\s+\\S*$", "", substr(first, 1, n)), "...")
  cap(first)
}

short_author <- function(a) {
  parts <- strsplit(as.character(a), " and ")[[1]]
  last <- sub(",.*", "", parts)
  if (length(last) > 2) paste(last[1], "et al.") else paste(last, collapse = " and ")
}

when_text <- function(b) {
  ok <- function(x) has_text(x) && as.character(x) != "0"
  if (ok(b$start_date) && ok(b$end_date)) paste("Recorded from", b$start_date, "to", b$end_date)
  else if (ok(b$start_date)) paste("Recorded since", b$start_date)
  else if (ok(b$end_date)) paste("Recorded until", b$end_date)
}

# Derived tables, as in server.R
behavior_counts <- aggregate(behavior_id ~ group_id, data = behaviors_table, FUN = length)
colnames(behavior_counts)[2] <- "behavior_count"
groups_with_details <- merge(groups_table, species_table, by = "species_id", all.x = TRUE)
groups_with_details <- merge(groups_with_details, behavior_counts, by = "group_id", all.x = TRUE)
groups_with_details$behavior_count[is.na(groups_with_details$behavior_count)] <- 0

group_domains <- tapply(behaviors_table$domains, id_text(behaviors_table$group_id), function(x) paste(split_domains(x), collapse = "; "))
groups_with_details$domains <- as.vector(group_domains[id_text(groups_with_details$group_id)])
groups_with_details$domains[is.na(groups_with_details$domains)] <- ""

lat <- groups_with_details$lat
long <- groups_with_details$long
region <- rep("Other", nrow(groups_with_details))
region[which(long >= -20 & long <= 52 & lat >= -36 & lat <= 36)] <- "Africa"
region[which(long >= -12 & long <= 40 & lat >= 36)] <- "Europe"
region[which((long >= 52 & lat > -10) | (long > 34 & lat > 28))] <- "Asia"
region[which(long >= -170 & long <= -30 & lat >= 15)] <- "North America"
region[which(long >= -170 & long <= -30 & lat < 15)] <- "Central & South America"
region[which(lat < 25 & long > -165 & long < -100)] <- "Open ocean"
region[which((long >= 110 & lat < -10) | (long <= -165 & lat < 0) | (long >= 160 & lat < 0))] <- "Oceania"
region[which(lat < -60)] <- "Antarctica"
region[is.na(lat) | is.na(long)] <- "No location"
groups_with_details$region <- region

sp_info <- species_table
sp_info$groups <- as.integer(table(factor(groups_table$species_id, levels = sp_info$species_id)))
behaviors_by_species <- tapply(groups_with_details$behavior_count, groups_with_details$species_id, sum)
sp_info$behaviors <- as.vector(behaviors_by_species[sp_info$species_id])
sp_info$behaviors[is.na(sp_info$behaviors)] <- 0
domains_by_species <- tapply(groups_with_details$domains, groups_with_details$species_id, function(x) paste(split_domains(x), collapse = "; "))
sp_info$domains <- as.vector(domains_by_species[sp_info$species_id])
sp_info$domains[is.na(sp_info$domains)] <- ""

domain_levels <- sort(split_domains(behaviors_table$domains))
classes <- sort(unique(groups_with_details$Class))
sp_sorted <- sp_info[order(tolower(sp_info$common_name)), ]

# HTML ---------------------------------------------------------------------------

# A small version of htmltools: tags$div(class = "x", "text", tags$b("bold")). Text is escaped, HTML() is not
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

HTML <- function(...) structure(paste0(...), class = "html")

render <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (inherits(x, "html")) return(paste(unclass(x), collapse = ""))
  if (is.list(x)) return(paste(vapply(x, render, ""), collapse = ""))
  x <- as.character(x)
  paste(esc(x[!is.na(x)]), collapse = "")
}

void_tags <- c("img", "input", "meta", "link", "br", "hr")

# The tag name is .name, so that name = "..." stays free for the attribute
tag <- function(.name, ...) {
  args <- list(...)
  keys <- names(args)
  if (is.null(keys)) keys <- rep("", length(args))
  attrs <- args[keys != "" & !vapply(args, is.null, logical(1))]
  kids <- args[keys == ""]
  a <- vapply(names(attrs), function(k) {
    v <- attrs[[k]]
    if (length(v) == 1 && is.na(v)) k else sprintf("%s=\"%s\"", k, esc(paste(v, collapse = " ")))
  }, "")
  open <- paste0("<", .name, if (length(a) > 0) paste0(" ", paste(a, collapse = " ")), ">")
  if (.name %in% void_tags) HTML(open) else HTML(open, render(kids), "</", .name, ">")
}

tags <- sapply(c("a", "b", "body", "br", "button", "details", "div", "em", "footer", "h1", "h2", "h3", "h4", "h5", "header",
                 "i", "img", "input", "label", "li", "link", "meta", "nav", "ol", "option", "p", "script", "section",
                 "select", "small", "span", "strong", "summary", "sup", "table", "tbody", "td", "template", "th",
                 "thead", "tr", "ul"),
               function(.name) function(...) tag(.name, ...), simplify = FALSE)

tagList <- function(...) list(...)
icon <- function(name) tags$i(class = paste0("fas fa-", name), role = "presentation", `aria-hidden` = "true")

# Pieces shared by the pages, as in server.R --------------------------------------

wiki_names <- function(g) {
  name <- as.character(g$canonicalName)
  if (is.na(name)) "" else paste(unique(c(name, paste(head(strsplit(name, " ")[[1]], 2), collapse = " "))), collapse = "|")
}

about_box <- function(g, class = "") tags$div(class = paste("acdb-about", class), `data-species` = wiki_names(g))

# Photos far down a long list wait until they are shown (data-lazy-species), so the page does not ask Wikipedia for all of them
photo_box <- function(g, size = "", lazy = FALSE) {
  name <- as.character(g$canonicalName)
  names <- unique(c(name, paste(head(strsplit(name, " ")[[1]], 2), collapse = " ")))
  panel <- unname(c(Aves = "birds", Mammalia = "mammals")[as.character(g$Class)])
  species <- if (is.na(name)) "" else paste(names, collapse = "|")
  tags$div(
    class = paste("acdb-photo", if (is.na(panel)) "other" else panel, size),
    `data-species` = if (!lazy) species,
    `data-lazy-species` = if (lazy) species,
    `data-size` = size,
    tags$img(alt = as.character(g$common_name)),
    tags$div(class = "none", icon("paw")),
    tags$span(class = "acdb-credit", tags$a(href = "#", target = "_blank", rel = "noopener", "Photo: Wikipedia"))
  )
}

domain_chips <- function(d) lapply(d, function(x) tags$span(class = paste0("chip chip-", slot(match(x, domain_levels))), cap(x)))

bars <- function(counts, colors = rep_len(acdb_palette, length(counts))) {
  tags$div(
    class = "acdb-bars",
    lapply(seq_along(counts), function(i) {
      tags$div(
        class = "acdb-bar",
        tags$span(class = "lab", names(counts)[i]),
        tags$div(class = "track", tags$div(class = "fill", style = sprintf("width: %.1f%%; background: %s;", 100 * counts[i] / max(counts), colors[i]))),
        tags$span(class = "num", counts[i])
      )
    })
  )
}

heat_table <- function(counts, row_head, col_labels) {
  tags$div(
    class = "acdb-heatwrap",
    tags$table(
      class = "acdb-heat",
      tags$thead(tags$tr(tags$th(row_head), lapply(col_labels, tags$th), tags$th("Total"))),
      tags$tbody(
        lapply(rownames(counts), function(r) {
          tags$tr(
            tags$th(r),
            lapply(counts[r, ], function(n) {
              tags$td(style = if (n > 0) sprintf("background: rgba(27, 127, 121, %.2f);", 0.08 + 0.55 * n / max(counts)), if (n > 0) n)
            }),
            tags$td(class = "total", sum(counts[r, ]))
          )
        })
      )
    )
  )
}

transmission_pills <- function(b) {
  lapply(c("vertical", "horizontal", "oblique"), function(m) {
    s <- tstate(b[[m]])
    tags$span(class = paste("tpill", s[1]), paste0(cap(m), ": ", s[2]))
  })
}

lineage_ribbon <- function(sp) {
  parts <- as.character(unlist(sp[, c("Phylum", "Class", "Ordr", "Family", "Genus")]))
  items <- unlist(lapply(seq_along(parts), function(i) list(if (i > 1) tags$i(HTML("&rsaquo;")), tags$span(parts[i]))), recursive = FALSE)
  tags$div(class = "acdb-lineage", items)
}

fact <- function(i, value, label) tags$div(class = paste0("fact chip-", i), tags$b(value), tags$span(label))

facts_row <- function(pk, doms) {
  unit <- if (has_text(pk$sp$primary_social_unit)) cap(as.character(pk$sp$primary_social_unit)) else "Unknown"
  tags$div(
    class = "acdb-facts",
    fact(1, nrow(pk$gr), if (nrow(pk$gr) == 1) "Population" else "Populations"),
    fact(2, sum(vapply(pk$bh, nrow, 1L)), "Behaviors"),
    fact(3, length(doms), if (length(doms) == 1) "Domain" else "Domains"),
    fact(4, unit, "Social unit")
  )
}

species_pack <- function(sid) {
  sp <- species_table[species_table$species_id == sid, ][1, ]
  gr <- groups_table[groups_table$species_id == sid, ]
  gr <- gr[order(gr$group_name), ]
  bh <- lapply(gr$group_id, function(g) behaviors_table[behaviors_table$group_id == g, ])
  refs <- unlist(lapply(bh, function(b) unlist(b[, source_cols], use.names = FALSE)))
  refs <- unique(c(refs, sp$unit_source))
  refs <- refs[refs %in% sources_table$source_id]
  list(sp = sp, gr = gr, bh = bh, refs = refs)
}

behavior_refs <- function(b, refs) {
  ids <- unique(unlist(b[, source_cols], use.names = FALSE))
  sort(match(ids[ids %in% refs], refs))
}

ref_text <- function(s) paste0(short_author(s$authors), " (", s$year, "). ", sub("\\.$", "", gsub("<[^>]+>", "", s$title)), ".")

ref_item <- function(id) {
  s <- sources_table[sources_table$source_id == id, ][1, ]
  tags$li(
    ref_text(s), " ",
    if (has_text(s$doi)) tags$a(href = paste0("https://doi.org/", s$doi), target = "_blank", rel = "noopener", paste0("https://doi.org/", s$doi))
  )
}

relatives_of <- function(sid) {
  sp <- sp_info[sp_info$species_id == sid, ]
  rel <- sp_info[sp_info$Ordr == sp$Ordr & sp_info$species_id != sid, ]
  if (nrow(rel) == 0) rel <- sp_info[sp_info$Class == sp$Class & sp_info$species_id != sid, ]
  rel[order(tolower(rel$common_name)), ]
}

order_domains <- function(sid) {
  sp <- sp_info[sp_info$species_id == sid, ]
  o <- sp_info[sp_info$Ordr == sp$Ordr, ]
  counts <- vapply(domain_levels, function(dm) sum(has_domain(o$domains, dm)), numeric(1))
  counts <- sort(counts[counts > 0], decreasing = TRUE)
  if (nrow(o) < 2 || length(counts) == 0) return(NULL)
  bars(setNames(counts, cap(names(counts))), acdb_palette[slot(match(names(counts), domain_levels))])
}

species_href <- function(sid, root = "") paste0(root, "species/", sid, ".html")

# Page shell ---------------------------------------------------------------------

nav_items <- list(
  list(file = "index.html", icon = "globe", label = "Explore"),
  list(file = "map.html", icon = "map", label = "Map"),
  list(file = "populations.html", icon = "th-large", label = "Populations"),
  list(file = "tree.html", icon = "sitemap", label = "Tree of life"),
  list(file = "cards.html", icon = "id-card", label = "Cards"),
  list(file = "statistics.html", icon = "chart-bar", label = "Statistics"),
  list(menu = "Literature", icon = "book", items = list(
    list(file = "citations.html", icon = "book", label = "Citations"),
    list(file = "papers.html", icon = "newspaper", label = "New papers"))),
  list(file = "about.html", icon = "info-circle", label = "About"),
  list(menu = "More", icon = "ellipsis-h", items = list(
    list(file = "contribute.html", icon = "envelope", label = "Contributions"),
    list(file = "help.html", icon = "question-circle", label = "Help")))
)

navbar <- function(active, root) {
  item <- function(x, class) {
    tags$a(href = paste0(root, x$file), class = paste(class, if (identical(x$file, active)) "active"),
           tags$div(icon(x$icon), " ", x$label))
  }
  tags$nav(
    class = "navbar navbar-default navbar-static-top navbar-expand-lg", role = "navigation",
    tags$div(
      class = "container-fluid",
      tags$div(class = "navbar-header", tags$a(
        class = "navbar-brand", href = paste0(root, "index.html"),
        tags$div(
          style = "font-size: 20px; font-weight: bold; padding: 0 20px;",
          tags$img(src = paste0(root, "assets/DDLLogo_white_small.png"), alt = "Data Diversity Lab", style = "height: 40px; margin-right: 10px;"),
          "Animal Culture Database",
          tags$span(class = "acdb-ver", paste0("v", acdb_version))
        )
      )),
      tags$button(class = "navbar-toggler", type = "button", `data-bs-toggle` = "collapse", `data-bs-target` = "#acdb-nav",
                  `aria-controls` = "acdb-nav", `aria-expanded` = "false", `aria-label` = "Toggle navigation",
                  tags$span(class = "navbar-toggler-icon")),
      tags$div(
        class = "navbar-collapse collapse", id = "acdb-nav",
        tags$ul(
          class = "nav navbar-nav",
          lapply(nav_items, function(x) {
            if (is.null(x$menu)) return(tags$li(class = "nav-item", item(x, "nav-link")))
            open <- any(vapply(x$items, function(k) identical(k$file, active), logical(1)))
            tags$li(
              class = "dropdown nav-item",
              tags$a(href = "#", class = paste("dropdown-toggle nav-link", if (open) "active"), `data-bs-toggle` = "dropdown",
                     role = "button", `aria-expanded` = "false", icon(x$icon), " ", x$menu),
              tags$ul(class = "dropdown-menu", lapply(x$items, function(k) tags$li(item(k, "dropdown-item"))))
            )
          })
        )
      )
    )
  )
}

vendor_css <- c("vendor/bootstrap/bootstrap-cosmo.min.css", "vendor/fontawesome/css/all.min.css",
                "vendor/leaflet/leaflet.css", "vendor/markercluster/MarkerCluster.css", "vendor/markercluster/MarkerCluster.Default.css",
                "vendor/datatables/dataTables.bootstrap5.min.css", "static.css", "acdb.css", "card.css")
vendor_js <- c("vendor/jquery/jquery.min.js", "vendor/bootstrap/bootstrap.bundle.min.js",
               "vendor/datatables/jquery.dataTables.min.js", "vendor/datatables/dataTables.bootstrap5.min.js",
               "vendor/leaflet/leaflet.js", "vendor/markercluster/leaflet.markercluster.js",
               "data.js", "photos.js", "tree.js", "print.js", "site.js")

page <- function(file, title, description, active, body, json_ld = NULL) {
  root <- if (grepl("/", file)) "../" else ""
  canonical <- paste0(site_url, sub("^index\\.html$", "", file))
  head <- tagList(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    HTML("<title>", esc(title), "</title>"),
    tags$meta(name = "description", content = description),
    tags$link(rel = "canonical", href = canonical),
    tags$meta(property = "og:title", content = title),
    tags$meta(property = "og:description", content = description),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = canonical),
    tags$link(rel = "icon", href = paste0(root, "assets/DDLLogo_white_small.png")),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,300..900;1,300..900&display=swap"),
    lapply(vendor_css, function(f) tags$link(rel = "stylesheet", href = paste0(root, "assets/", f))),
    if (!is.null(json_ld)) HTML("<script type=\"application/ld+json\">", json_ld, "</script>"),
    tags$script(defer = NA, src = "https://umami.datadiversitylab.synology.me/script.js",
                `data-website-id` = "d6d0486a-5f2c-4ddc-bab6-93e1d25801f6",
                `data-domains` = "datadiversitylab.github.io")
  )
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n", render(head), "\n</head>\n",
    render(tags$body(
      `data-root` = root,
      navbar(active, root),
      tags$div(class = "container-fluid", tags$div(class = "tab-content", tags$div(class = "tab-pane active", body))),
      lapply(vendor_js, function(f) tags$script(src = paste0(root, "assets/", f)))
    )),
    "\n</html>\n"
  )
  path <- file.path(out, file)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(enc2utf8(html), path, useBytes = TRUE)
  invisible(file)
}

# Form controls in the markup Shiny uses
select_input <- function(id, choices, width = NULL, label = names(choices)[1]) {
  tags$div(
    class = "form-group shiny-input-container", style = if (!is.null(width)) paste0("width: ", width, ";"),
    tags$select(id = id, class = "form-select", `aria-label` = label,
                lapply(seq_along(choices), function(i) tags$option(value = unname(choices[i]), names(choices)[i])))
  )
}

class_choices <- c("All taxonomic groups" = "All", setNames(classes, class_label(classes)))
domain_choices <- c("All behavior domains" = "All", setNames(domain_levels, cap(domain_levels)))

# JSON for the browser, in base R --------------------------------------------------

to_json <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    inner <- vapply(x, to_json, "")
    if (!is.null(names(x)) && all(names(x) != "")) return(paste0("{", paste0(vapply(names(x), to_json, ""), ":", inner, collapse = ","), "}"))
    return(paste0("[", paste(inner, collapse = ","), "]"))
  }
  one <- function(v) {
    if (is.na(v)) return("null")
    if (is.logical(v)) return(if (v) "true" else "false")
    if (is.numeric(v)) return(trimws(formatC(v, digits = 15, format = "fg")))
    v <- gsub("\\", "\\\\", v, fixed = TRUE)
    v <- gsub("\"", "\\\"", v, fixed = TRUE)
    v <- gsub("\n", "\\n", v, fixed = TRUE)
    v <- gsub("\r", "\\r", v, fixed = TRUE)
    v <- gsub("\t", "\\t", v, fixed = TRUE)
    paste0("\"", gsub("</", "<\\/", v, fixed = TRUE), "\"")
  }
  vals <- vapply(as.list(x), one, "")
  if (length(x) == 1 && !inherits(x, "AsIs")) vals else paste0("[", paste(vals, collapse = ","), "]")
}

# Explore ------------------------------------------------------------------------------

json_ld <- to_json(list(
  `@context` = "https://schema.org",
  `@type` = "Dataset",
  name = "Animal Culture Database (ACDB)",
  alternateName = "ACDB",
  description = sprintf(paste(
    "The Animal Culture Database (ACDB) is an open-access, comparative database of socially transmitted behaviors and traditions in wild animal populations.",
    "Version %s documents %d behaviors in %d populations of %d species, including vocal communication, foraging, migration, mating displays, and play,",
    "with the evidence for social transmission and the effects of human activity reported in the literature."
  ), acdb_version, nrow(behaviors_table), nrow(groups_table), nrow(species_table)),
  url = site_url,
  version = acdb_version,
  license = "https://opensource.org/licenses/MIT",
  isAccessibleForFree = TRUE,
  citation = paper_url,
  keywords = I(c("animal culture", "social learning", "behavioral traditions", "cultural transmission", "conservation", "animal behavior")),
  creator = lapply(c("Kiran Basava", "Md Nafis Ul Alam", "Liam Roberts", "Kristen Martinet", "Paige Cherry", "Héctor García-Verdugo", "Cristian Román-Palacios"),
                   function(n) list(`@type` = "Person", name = n)),
  spatialCoverage = "Global",
  distribution = lapply(c("species", "groups", "behaviors", "sources"), function(t) {
    list(`@type` = "DataDownload", encodingFormat = "text/csv", contentUrl = paste0(site_url, "data/", t, ".csv"))
  })
))

page(
  "index.html",
  "Animal Culture Database (ACDB) | Animal traditions and social learning",
  "The Animal Culture Database (ACDB) brings together published cases of socially learned behavior in wild animals: dialects, foraging methods, migration routes, and mating displays.",
  "index.html",
  tagList(
    tags$div(
      class = "acdb-hero",
      tags$div(
        class = "acdb-pills",
        tags$a(class = "acdb-pill", href = paste0(repo_url, "/releases"), target = "_blank", rel = "noopener",
               tags$span(class = "dot"), paste("Version", acdb_version)),
        tags$a(class = "acdb-pill", href = paper_url, target = "_blank", rel = "noopener", "Read the data paper")
      ),
      tags$h1("Animal Culture Database"),
      tags$p(class = "acdb-lead", "Wild animals learn from each other. Dialects, foraging methods, migration routes, and mating displays spread through groups and last across generations. The ACDB brings these cases together."),
      tags$div(class = "acdb-chips", tags$span(class = "lab", "Traditions we track"), domain_chips(setdiff(domain_levels, "other"))),
      tags$div(
        class = "acdb-actions",
        tags$a(class = "btn btn-default acdb-btn-amber", href = "map.html", "Explore the map"),
        tags$a(class = "btn btn-default acdb-btn-ghost", href = "populations.html", "Browse populations"),
        tags$a(class = "btn btn-default acdb-btn-ghost", href = "tree.html", "Tree of life"),
        tags$a(class = "btn btn-default acdb-btn-ghost", href = "contribute.html", "Contribute data")
      )
    ),
    tags$div(
      class = "acdb-wrap",
      tags$div(
        class = "acdb-example",
        tags$div(class = "acdb-ex-top", tags$span(class = "acdb-eyebrow", "Meet a population"),
                 tags$a(href = "#", id = "another", icon("sync"), " Show another")),
        tags$div(id = "featured")
      )
    ),
    tags$div(
      class = "acdb-foot",
      paste("Version", acdb_version), " | ",
      tags$a(href = paper_url, target = "_blank", rel = "noopener", "Data paper"), " | ",
      tags$a(href = repo_url, target = "_blank", rel = "noopener", "GitHub")
    )
  ),
  json_ld
)

# Map ------------------------------------------------------------------------------------

page(
  "map.html",
  "Map of animal cultures | Animal Culture Database",
  "World map of the wild animal populations in the Animal Culture Database, with the species in view and their socially learned behaviors.",
  "map.html",
  tags$div(
    class = "acdb-section",
    tags$h3("Map"),
    tags$p("Points show the approximate location of each population. For species with wide ranges (e.g. whales), the point is somewhere in the middle of the range. Move the map and the list on the right follows. Click a species or a point for its card."),
    tags$div(
      class = "acdb-mapsplit",
      tags$div(
        class = "acdb-mapbox",
        tags$div(class = "acdb-maptools", select_input("map_class", class_choices, "220px"), select_input("map_domain", domain_choices, "220px")),
        tags$div(id = "population_map", style = "height: 600px;")
      ),
      tags$div(
        class = "acdb-side",
        tags$div(class = "acdb-side-head", tags$h4("In view"), tags$div(id = "side_note", class = "shiny-text-output")),
        tags$div(class = "acdb-side-list", id = "map_species")
      )
    )
  )
)

# Populations ---------------------------------------------------------------------------

pop <- groups_with_details[order(tolower(groups_with_details$group_name)), ]

pop_cards <- lapply(seq_len(nrow(pop)), function(i) {
  g <- pop[i, ]
  gid <- id_text(g$group_id)
  href <- paste0(species_href(g$species_id), "?g=", gid)
  tags$div(
    class = "acdb-card", `data-href` = href, `data-id` = gid,
    photo_box(g, lazy = TRUE),
    tags$div(
      class = "acdb-card-body",
      tags$div(tags$span(class = "chip chip-class", class_label(g$Class)), head(domain_chips(split_domains(g$domains)), 2)),
      tags$h5(tags$a(href = href, g$group_name)),
      tags$div(class = "sp", tags$i(g$canonicalName)),
      tags$div(class = "where", g$behavior_count, if (g$behavior_count == 1) " behavior | " else " behaviors | ", g$location_evidence)
    )
  )
})

pop_rows <- lapply(seq_len(nrow(pop)), function(i) {
  g <- pop[i, ]
  gid <- id_text(g$group_id)
  tags$tr(
    `data-id` = gid, `data-href` = paste0(species_href(g$species_id), "?g=", gid),
    tags$td(photo_box(g, "mini", lazy = TRUE)),
    tags$td(tags$a(href = paste0(species_href(g$species_id), "?g=", gid), g$group_name)),
    tags$td(tags$i(g$canonicalName), tags$br(), tags$small(title_case(g$common_name))),
    tags$td(tags$span(class = "chip chip-class", class_label(g$Class))),
    tags$td(domain_chips(split_domains(g$domains))),
    tags$td(g$behavior_count),
    tags$td(g$location_evidence)
  )
})

page(
  "populations.html",
  "Populations with animal traditions | Animal Culture Database",
  sprintf("Browse the %d wild animal populations in the Animal Culture Database by taxonomic group, behavior domain, or place, and download them as CSV.", nrow(groups_table)),
  "populations.html",
  tags$div(
    class = "acdb-section",
    tags$h3("Populations"),
    tags$details(
      tags$summary("What is a group?"),
      tags$p("A \u2018group\u2019 in this database is a population of animals within a species that displays one or more socially transmitted behaviors forming a tradition. There can be multiple groups within a species, and groups nested within other groups in species with multilevel social structures such as killer whales and elephants. For instance, there may be multiple pods of killer whales in a clan, and they may share a whistle dialect at the pod level and a pulsed call dialect at the clan level.")
    ),
    tags$div(
      class = "acdb-filters",
      tags$div(class = "form-group shiny-input-container", style = "width: 100%;",
               tags$input(id = "pop_q", type = "text", class = "form-control", placeholder = "Search groups, species, or places", `aria-label` = "Search")),
      select_input("pop_class", class_choices, "100%"),
      select_input("pop_domain", domain_choices, "100%"),
      select_input("pop_sort", c("Name (A to Z)" = "name", "Most behaviors" = "behaviors"), "100%")
    ),
    tags$div(class = "acdb-count", tags$div(id = "pop_note", class = "shiny-text-output", sprintf("Showing %d of %d populations", nrow(pop), nrow(pop))),
             tags$a(href = "#", id = "pop_csv", icon("download"), " Download these populations (CSV)")),
    tags$ul(
      class = "nav nav-pills", role = "tablist",
      tags$li(class = "nav-item", tags$a(class = "nav-link active", href = "#pop-gallery", `data-bs-toggle` = "tab", role = "tab", "Gallery")),
      tags$li(class = "nav-item", tags$a(class = "nav-link", href = "#pop-table", `data-bs-toggle` = "tab", role = "tab", "Table"))
    ),
    tags$div(
      class = "tab-content",
      tags$div(class = "tab-pane active", id = "pop-gallery", role = "tabpanel",
               tags$div(class = "acdb-grid", id = "pop_cards", pop_cards),
               tags$p(class = "acdb-hint", id = "pop_empty", style = "display: none;", "No populations match these filters."),
               tags$button(type = "button", class = "btn btn-default acdb-btn-more", id = "more", "Show more")),
      tags$div(class = "tab-pane", id = "pop-table", role = "tabpanel",
               tags$p(class = "acdb-hint", "Click on a row to display taxonomic information, a list of behaviors and descriptions, and a focused map of the group location."),
               tags$table(id = "groups_table", class = "table hover", style = "width: 100%;",
                          tags$thead(tags$tr(lapply(c("Photo", "Group", "Species", "Taxon", "Domains", "Behaviors", "Location"), tags$th))),
                          tags$tbody(pop_rows)))
    )
  )
)

# Tree of life -------------------------------------------------------------------------------

page(
  "tree.html",
  "Tree of life of animal cultures | Animal Culture Database",
  "Every species in the Animal Culture Database placed on a tree built from its taxonomy, with the behavior domains recorded for each species.",
  "tree.html",
  tags$div(
    class = "acdb-section",
    tags$h3("Tree of life"),
    tags$p("Every species in the database, placed by taxonomy (phylum, class, order, family, genus). Branch lengths do not show evolutionary time. Each ring is a behavior domain, and a colored dot means the domain has been recorded for that species. Highlight a domain to see where it occurs. Click a species to open its card."),
    tags$div(class = "acdb-treecard", tags$div(id = "acdb_legend", class = "acdb-legend"), tags$div(id = "acdb_tree")),
    tags$div(id = "acdb_tip"),
    tags$details(class = "acdb-alllist", tags$summary(sprintf("All %d species in the tree", nrow(sp_sorted))),
                 tags$ul(lapply(seq_len(nrow(sp_sorted)), function(i) {
                   tags$li(tags$a(href = species_href(sp_sorted$species_id[i]), title_case(sp_sorted$common_name[i])), " ", tags$i(sp_sorted$canonicalName[i]))
                 })))
  )
)

# Cards -----------------------------------------------------------------------------------------

card_choices <- setNames(sp_sorted$species_id, sprintf("%s (%s)", title_case(sp_sorted$common_name), sp_sorted$canonicalName))

page(
  "cards.html",
  "Species cards | Animal Culture Database",
  "A printable one-page card for each species in the Animal Culture Database, with the photo, taxonomy, behaviors, and sources.",
  "cards.html",
  tags$div(
    class = "acdb-section",
    tags$h3("Species cards"),
    tags$p("A one-page card for each species, with the photo, taxonomy, behaviors, sources, and the version of the database. Pick a species, then download it as a PDF to print or share."),
    tags$div(
      class = "acdb-cardtools",
      select_input("card_species", card_choices, "420px", "Species"),
      tags$button(type = "button", id = "card_print", class = "btn btn-primary", icon("download"), " Download PDF"),
      tags$button(type = "button", id = "card_random", class = "btn btn-default", icon("random"), " Surprise me")
    ),
    tags$div(class = "acdb-paper", tags$div(id = "card_preview"))
  )
)

# Statistics -----------------------------------------------------------------------------------

kpis <- list(
  list(n = nrow(groups_table), label = "Populations"),
  list(n = length(unique(species_table$species_id)), label = "Species"),
  list(n = nrow(behaviors_table), label = "Behaviors"),
  list(n = nrow(sources_table), label = "Sources")
)

stats_taxa <- lapply(c("Class", "Ordr", "Family"), function(rank) {
  counts <- head(sort(table(groups_with_details[[rank]]), decreasing = TRUE), 10)
  labels <- if (rank == "Class") class_label(names(counts)) else names(counts)
  tags$div(class = "acdb-rank", `data-rank` = rank, style = if (rank != "Class") "display: none;", bars(setNames(as.integer(counts), labels)))
})

region_counts <- sort(table(groups_with_details$region), decreasing = TRUE)

domain_counts <- vapply(domain_levels, function(dm) sum(has_domain(groups_with_details$domains, dm)), numeric(1))
domain_colors <- acdb_palette[slot(match(names(domain_counts), domain_levels))]
o <- order(domain_counts, decreasing = TRUE)

status <- species_table$IUCN
status[is.na(status)] <- "Not evaluated"
iucn_counts <- table(factor(status, levels = c(names(iucn_names), "Not evaluated")))
iucn_counts <- iucn_counts[iucn_counts > 0]
iucn_labels <- ifelse(names(iucn_counts) %in% names(iucn_names), iucn_names[names(iucn_counts)], names(iucn_counts))
iucn_cols <- ifelse(names(iucn_counts) %in% names(iucn_colors), iucn_colors[names(iucn_counts)], "#B8C4C8")

cross <- table(factor(groups_with_details$region, levels = region_order), groups_with_details$Class)
cross <- cross[rowSums(cross) > 0, , drop = FALSE]

domain_class <- sapply(classes, function(cl) {
  s <- sp_info[sp_info$Class == cl, ]
  sapply(domain_levels, function(dm) sum(has_domain(s$domains, dm)))
})
rownames(domain_class) <- cap(domain_levels)

page(
  "statistics.html",
  "Statistics | Animal Culture Database",
  sprintf("What is in the Animal Culture Database: %d populations, %d species, %d behaviors, and %d sources, by taxonomic group, region, behavior domain, and conservation status.",
          nrow(groups_table), nrow(species_table), nrow(behaviors_table), nrow(sources_table)),
  "statistics.html",
  tags$div(
    class = "acdb-section",
    tags$h3("Statistics"),
    tags$p("What is in the database today. Counts are populations unless noted."),
    tags$div(class = "acdb-kpis", lapply(seq_along(kpis), function(i) tags$div(class = paste0("kpi chip-", i), tags$b(kpis[[i]]$n), tags$span(kpis[[i]]$label)))),
    tags$div(
      class = "acdb-statgrid",
      tags$div(class = "acdb-panel",
               tags$div(class = "top", tags$h4("By taxonomic group"), select_input("stats_rank", c("Class" = "Class", "Order" = "Ordr", "Family" = "Family"), "130px", "Taxonomic rank")),
               stats_taxa),
      tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("By region")), bars(setNames(as.integer(region_counts), names(region_counts)))),
      tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("By behavior domain")), bars(setNames(domain_counts[o], cap(names(domain_counts))[o]), domain_colors[o])),
      tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("By conservation status (species)")), bars(setNames(as.integer(iucn_counts), iucn_labels), unname(iucn_cols)))
    ),
    tags$div(class = "acdb-panel", tags$div(class = "top", tags$h4("Regions and taxonomic groups")), heat_table(cross, "Region", class_label(colnames(cross)))),
    tags$div(class = "acdb-panel", style = "margin-top: 16px;", tags$div(class = "top", tags$h4("Behavior domains across the animal tree (species)")),
             heat_table(domain_class, "Behavior domain", class_label(colnames(domain_class))))
  )
)

# Citations -----------------------------------------------------------------------------------------

info_cols <- c(behavior_source = "Behavior", vertical_source = "Vertical transmission",
               horizontal_source = "Horizontal transmission", oblique_source = "Oblique transmission",
               domains_source = "Domains", anth_source = "Anthropogenic effects")
info_levels <- c(unname(info_cols), "Social unit", "Group size", "Location")
species_of_behavior <- groups_table$species_id[match(behaviors_table$group_id, groups_table$group_id)]
uses <- rbind(
  do.call(rbind, lapply(names(info_cols), function(col) data.frame(source_id = behaviors_table[[col]], species_id = species_of_behavior, info = info_cols[[col]]))),
  data.frame(source_id = species_table$unit_source, species_id = species_table$species_id, info = "Social unit"),
  data.frame(source_id = groups_table$size_source, species_id = groups_table$species_id, info = "Group size"),
  data.frame(source_id = groups_table$location_source, species_id = groups_table$species_id, info = "Location")
)
uses <- uses[uses$source_id %in% sources_table$source_id, ]

cites <- sources_table[order(-sources_table$year), ]
cite_rows <- lapply(seq_len(nrow(cites)), function(i) {
  s <- cites[i, ]
  u <- uses[uses$source_id == s$source_id, ]
  sids <- unique(u$species_id)
  sids <- sids[order(species_table$canonicalName[match(sids, species_table$species_id)])]
  tags$tr(
    tags$td(tags$span(title = s$authors, short_author(s$authors))),
    tags$td(s$year),
    tags$td(gsub("<[^>]+>", "", s$title)),
    tags$td(if (has_text(s$doi)) tags$a(href = paste0("https://doi.org/", s$doi), target = "_blank", rel = "noopener", s$doi) else "No DOI"),
    tags$td(lapply(seq_along(sids), function(k) tagList(if (k > 1) ", ", tags$a(href = species_href(sids[k]), tags$i(species_table$canonicalName[match(sids[k], species_table$species_id)]))))),
    tags$td(lapply(unique(u$info), function(x) tags$span(class = paste0("chip chip-", slot(match(x, info_levels))), x)))
  )
})

page(
  "citations.html",
  "Citations | Animal Culture Database",
  sprintf("The %d papers behind the Animal Culture Database, with the species coded from each paper and what the paper informs.", nrow(sources_table)),
  "citations.html",
  tags$div(
    class = "acdb-section",
    tags$h3("Citations"),
    tags$p("Every paper behind the data. The table shows the species coded from each paper and what the paper informs."),
    tags$table(id = "citations_table", class = "table hover", style = "width: 100%;",
               tags$thead(tags$tr(lapply(c("Authors", "Year", "Title", "DOI", "Species", "Informs"), tags$th))),
               tags$tbody(cite_rows))
  )
)

# New papers ---------------------------------------------------------------------------------------

papers <- tryCatch(read.csv("db/uncoded_papers.csv", stringsAsFactors = FALSE), error = function(e) NULL)
if (is.null(papers) || ncol(papers) < 4) papers <- data.frame(authors = character(), year = character(), title = character(), doi = character())
papers <- papers[, 1:4]
names(papers) <- c("authors", "year", "title", "doi")

page(
  "papers.html",
  "New papers | Animal Culture Database",
  "Papers on animal culture that will be coded into a future release of the Animal Culture Database.",
  "papers.html",
  tags$div(
    class = "acdb-section",
    tags$h3("New papers"),
    tags$p("These papers are not coded yet. They will be added to the database in a future release. If you know of a paper that is missing, please send it to us through the Contributions tab."),
    tags$table(id = "papers_table", class = "table hover", style = "width: 100%;",
               tags$thead(tags$tr(lapply(c("Authors", "Year", "Title", "DOI"), tags$th))),
               tags$tbody(lapply(seq_len(nrow(papers)), function(i) {
                 p <- papers[i, ]
                 tags$tr(tags$td(p$authors), tags$td(p$year), tags$td(p$title),
                         tags$td(if (has_text(p$doi)) tags$a(href = paste0("https://doi.org/", p$doi), target = "_blank", rel = "noopener", p$doi) else "No DOI"))
               })))
  )
)

# About, contributions, and help -------------------------------------------------------------------

person <- function(i, initials, name, email = NULL) {
  tags$div(class = "acdb-person", tags$div(class = paste0("dot chip-", i), initials),
           tags$div(tags$b(name), if (!is.null(email)) tags$small(tags$a(href = paste0("mailto:", email), email))))
}

page(
  "about.html",
  "About | Animal Culture Database",
  "What the Animal Culture Database is, how the data are organized, how versions work, who built it, and how to cite it.",
  "about.html",
  tags$div(
    class = "acdb-section acdb-narrow",
    tags$h3("About the ACDB"),
    tags$p("The ACDB (Animal Culture Database) is a database of variables on socially learned behavioral traditions in wild animal populations worldwide. It brings together the current literature on behavioral variation within species, in contexts such as communication, foraging, and migration. The data are open access and can be explored on this website."),
    tags$div(
      class = "acdb-aims",
      tags$div(tags$h5("Document"), tags$p("Record socially transmitted behaviors observed in wild animal populations.")),
      tags$div(tags$h5("Analyze"), tags$p("Examine patterns of cultural diversity across the globe and across species.")),
      tags$div(tags$h5("Inform"), tags$p("Use what we know about animal culture to support biodiversity conservation."))
    ),
    tags$h4("How the data are organized"),
    tags$p("The database has four tables: groups, species, behaviors, and sources. A group is a population of one species that shares one or more traditions. Each behavior belongs to a group and cites the sources that support it. The ",
           tags$a(href = paste0(repo_url, "/blob/main/ACDB_codebook_2026.pdf"), target = "_blank", rel = "noopener", "codebook"),
           " defines every variable. The tables can be downloaded as CSV: ",
           tags$a(href = "data/species.csv", "species"), ", ", tags$a(href = "data/groups.csv", "groups"), ", ",
           tags$a(href = "data/behaviors.csv", "behaviors"), ", and ", tags$a(href = "data/sources.csv", "sources"), "."),
    tags$h4("Versions and updates"),
    tags$p("Major releases (e.g. v1.0.0, v2.0.0) are based on the addition of 100 or more populations. Minor releases (e.g. v1.1.0) are based on the addition of fewer than 100 populations. Changes to the contents of the database, such as a missing paper, a taxonomy fix, or a new location, are patches (e.g. v1.1.1). We plan to cover most of the existing animal behavior literature in version 2.0, and we are building a pipeline for external contributions."),
    tags$h4("Who we are"),
    tags$p("The ACDB was developed at the University of Arizona. Contact Dr. Basava about the data and Dr. Román-Palacios about the website."),
    tags$div(
      class = "acdb-team",
      person(1, "KB", "Kiran Basava", "kcb7@arizona.edu"), person(2, "MA", "Md Nafis Ul Alam"), person(3, "LR", "Liam Roberts"),
      person(4, "KM", "Kristen Martinet"), person(5, "PC", "Paige Cherry"), person(6, "HG", "Héctor García-Verdugo"),
      person(1, "CR", "Cristian Román-Palacios", "cromanpa@arizona.edu")
    ),
    tags$h4("How to cite"),
    tags$div(class = "acdb-cite",
             "Basava K, Alam MNU, Roberts L, Martinet K, Cherry P, García-Verdugo H, Román-Palacios C (2025). Mapping nonhuman cultures with the Animal Culture Database. ",
             tags$i("Scientific Data"), " 12. ", tags$a(href = paper_url, target = "_blank", rel = "noopener", paper_url)),
    tags$p(style = "margin-top: 16px;", "The code and data are on ", tags$a(href = repo_url, target = "_blank", rel = "noopener", "GitHub"), " under the MIT license.")
  )
)

page(
  "contribute.html",
  "Contribute data | Animal Culture Database",
  "How to send data on a socially learned behavior in a wild animal population to the Animal Culture Database.",
  "contribute.html",
  tags$div(
    class = "acdb-section acdb-narrow",
    tags$h3("Contribute to the ACDB"),
    tags$p("The database grows with help from researchers. If you have data on a socially learned behavior in a wild animal population, we would like to hear from you."),
    tags$p("We are still building a submission pipeline. For now, please send your contribution by email to ", tags$a(href = "mailto:kcb7@arizona.edu", "kcb7@arizona.edu"), "."),
    tags$h4("What to include"),
    tags$ul(tags$li("The reference, with the DOI if there is one."), tags$li("The species and the population."),
            tags$li("The behavior, and the evidence that it is socially learned."), tags$li("Where the population lives.")),
    tags$p("A missing paper, a taxonomy fix, or a better location is also welcome. These are released as patches."),
    tags$a(class = "btn btn-primary", href = "mailto:kcb7@arizona.edu?subject=ACDB%20contribution", "Send an email")
  )
)

faq <- function(question, ...) {
  tags$div(style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
           tags$h4(style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;", question),
           tags$p(style = "text-align: justify; margin: 0;", ...))
}

page(
  "help.html",
  "Help and FAQs | Animal Culture Database",
  "Answers to common questions about searching, downloading, and contributing to the Animal Culture Database.",
  "help.html",
  tags$div(
    class = "container-fluid",
    tags$div(
      style = "max-width: 900px; margin: 0 auto; padding: 40px 20px; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; color: #333333; line-height: 1.8;",
      tags$div(style = "background-color: #f9f9f9; padding: 30px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
               tags$h1(style = "font-size: 36px; font-weight: bold; text-align: center; color: #1B7F79; margin-bottom: 20px;", "Help and FAQs"),
               tags$p(style = "text-align: center; color: #555555; margin-bottom: 40px;", "Please contact ",
                      tags$a(href = "mailto:kcb7@arizona.edu", "Dr. Kiran Basava"), " if your questions are not answered below!")),
      tags$div(
        style = "margin-top: 20px;",
        faq("How do I search for populations?", "Use the search bar on the Populations page to search for specific groups, species, or locations."),
        faq("How can I download data?", "Download each table as CSV from the About page, or get the SQLite database and the codebook from each ",
            tags$a(href = paste0(repo_url, "/releases"), "release on GitHub"), "."),
        faq("How can I contribute data to the database?", "We are currently working on a pipeline for contributions from researchers. For now, please see the Contributions page, or email ",
            tags$a(href = "mailto:kcb7@arizona.edu", "Dr. Basava"), " with a description of the sort of data you would like to be added. Thank you for your interest in contributing data!"),
        faq("How do I get a printable card for a species?", "Open the Cards page, pick a species, and click Download PDF. You can also download the card from the page of each species. The card has the photo, taxonomy, behaviors, sources, and the version of the database."),
        faq("What does the tree of life show?", "It places every species in the database on a tree built from the GBIF taxonomy (phylum, class, order, family, genus, species). The branch lengths do not represent evolutionary time. The colored rings show which behavior domains have been recorded for each species. Click a domain at the top to highlight it, and click a species to open its card."),
        tags$div(style = "margin-bottom: 30px; background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                 tags$h4(style = "font-size: 20px; font-weight: bold; color: #1B7F79; margin-bottom: 10px;", "Where can I learn more about animal culture?"),
                 tags$p("Some recent and accessible reviews of the research on animal culture include:", tags$br(),
                        tags$a(href = "https://onlinelibrary.wiley.com/doi/10.1002/bies.201900060", "Allen, Jenny A. (2019). Community through Culture: From Insects to Whales: How Social Learning and Culture Manifest across Diverse Animal Communities."), tags$br(),
                        tags$a(href = "https://www.science.org/doi/10.1126/science.abe6514", "Whiten, Andrew. (2021). The Burgeoning Reach of Animal Culture."), tags$br(), tags$br(),
                        "For the relevance of animal culture to conservation:", tags$br(),
                        tags$a(href = "https://www.science.org/doi/abs/10.1126/science.aaw3557", "Brakes, Philippa, et al. (2019). Animal Cultures Matter for Conservation."))),
        faq("Who can I contact for technical support?", "Please contact ", tags$a(href = "mailto:cromanpa@arizona.edu", "Dr. Román-Palacios."))
      )
    )
  )
)

# Species pages: the species window of the app as a page, with the printable card ------------------

species_card <- function(sid, root) {
  pk <- species_pack(sid)
  sp <- pk$sp
  info <- sp_info[sp_info$species_id == sid, ]
  doms <- split_domains(info$domains)
  panel <- unname(c(Aves = "birds", Mammalia = "mammals")[as.character(sp$Class)])
  if (is.na(panel)) panel <- "other"
  rel <- relatives_of(sid)
  n_order <- sum(sp_info$Ordr == sp$Ordr)

  groups_ui <- lapply(seq_len(nrow(pk$gr)), function(i) {
    g <- pk$gr[i, ]
    b <- pk$bh[[i]]
    tags$div(
      class = "pdf-group",
      tags$h3(g$group_name),
      tags$div(class = "loc", g$location_evidence),
      lapply(seq_len(nrow(b)), function(j) {
        x <- b[j, ]
        nums <- behavior_refs(x, pk$refs)
        tags$div(
          class = "pdf-beh",
          style = sprintf("border-left-color: %s;", acdb_palette[slot(match(split_domains(x$domains)[1], domain_levels))]),
          tags$h4(x$behavior),
          domain_chips(split_domains(x$domains)),
          tags$p(lead_text(x$behavior_description), if (length(nums) > 0) tags$sup(paste(nums, collapse = ", "))),
          tags$div(class = "trans", transmission_pills(x))
        )
      })
    )
  })

  tree_text <- if (n_order > 1) paste0(sp$Ordr, " has ", n_order, " species in the ACDB. Relatives: ") else paste0("It is the only ", sp$Ordr, " in the ACDB. Others in ", sp$Class, ": ")
  tree_p <- if (nrow(rel) == 0) tags$p("It is the only species of its class in the ACDB.") else
    tags$p(tree_text, lapply(seq_len(min(nrow(rel), 8)), function(k) tags$span(if (k > 1) ", ", tags$i(rel$canonicalName[k]))), ".")

  tags$div(
    class = "acdb-pdf",
    tags$div(
      class = paste("pdf-band", panel),
      tags$img(class = "pdf-logo", src = paste0(root, "assets/DDLLogo_white_small.png"), alt = ""),
      tags$div(class = "t", tags$small("ANIMAL CULTURE DATABASE"), tags$b("Species card")),
      tags$div(class = "v", tags$b(paste("Version", acdb_version)), tags$span(class = "card-date"))
    ),
    tags$div(
      class = "pdf-hero",
      photo_box(sp, "large"),
      tags$div(
        class = "pdf-id",
        tags$h1(title_case(sp$common_name)),
        tags$div(class = "sci", sp$canonicalName),
        tags$span(class = "chip chip-class", class_label(sp$Class)),
        if (!is.na(sp$IUCN) && sp$IUCN %in% names(iucn_names)) tags$span(class = paste0("chip iucn-", sp$IUCN), iucn_names[[sp$IUCN]]),
        domain_chips(doms),
        lineage_ribbon(sp),
        facts_row(pk, doms)
      )
    ),
    about_box(sp, "pdf-about"),
    tags$h2(class = "pdf-h", "Populations and behaviors"),
    groups_ui,
    tags$h2(class = "pdf-h", "Where it sits on the tree"),
    tags$div(class = "pdf-tree", tree_p, order_domains(sid)),
    tags$h2(class = "pdf-h", "Sources"),
    tags$ol(class = "pdf-refs", lapply(pk$refs, ref_item)),
    tags$div(
      class = "pdf-foot",
      tags$img(src = paste0(root, "assets/DDLLogo_white_small.png"), alt = ""),
      tags$div(
        paste0("Data from the Animal Culture Database (ACDB), version ", acdb_version, ". Basava K, Alam MNU, Roberts L, Martinet K, Cherry P, García-Verdugo H, Román-Palacios C (2025). Mapping nonhuman cultures with the Animal Culture Database. Scientific Data 12. "),
        tags$a(href = paper_url, paper_url), ". Explore the data at ",
        tags$a(href = site_url, "datadiversitylab.github.io/ACDB"), " | MIT license"
      )
    )
  )
}

species_page <- function(sid) {
  root <- "../"
  pk <- species_pack(sid)
  sp <- pk$sp
  info <- sp_info[sp_info$species_id == sid, ]
  doms <- split_domains(info$domains)

  behaviors_ui <- lapply(seq_len(nrow(pk$gr)), function(i) {
    g <- pk$gr[i, ]
    b <- pk$bh[[i]]
    tags$div(
      class = "acdb-bgroup", id = paste0("g", id_text(g$group_id)), `data-id` = id_text(g$group_id),
      tags$h4(g$group_name),
      tags$div(class = "loc", icon("map-marker-alt"), " ", g$location_evidence),
      if (nrow(b) == 0) tags$p(class = "acdb-hint", "No behaviors are coded for this population yet."),
      lapply(seq_len(nrow(b)), function(j) {
        x <- b[j, ]
        nums <- behavior_refs(x, pk$refs)
        evid <- function(label, text) if (has_text(text) && !tolower(trimws(text)) %in% c("not searched", "0")) tags$p(tags$b(label), text)
        tags$div(
          class = "acdb-bcard",
          style = sprintf("border-left-color: %s;", acdb_palette[slot(match(split_domains(x$domains)[1], domain_levels))]),
          tags$h5(x$behavior),
          domain_chips(split_domains(x$domains)),
          tags$p(style = "margin-top: 8px;", lead_text(x$behavior_description)),
          tags$div(class = "when", when_text(x)),
          transmission_pills(x),
          tags$details(
            tags$summary("Evidence and details"),
            evid("Description: ", x$behavior_description),
            evid("Vertical: ", x$vertical_evidence),
            evid("Horizontal: ", x$horizontal_evidence),
            evid("Oblique: ", x$oblique_evidence),
            evid("Domains: ", x$domains),
            evid("Anthropogenic effects: ", x$anth_effects)
          ),
          if (length(nums) > 0) tags$div(class = "src", "Sources: ", paste0("[", nums, "]", collapse = " "))
        )
      })
    )
  })

  rel <- relatives_of(sid)
  tree_ui <- tagList(
    tags$div(class = "acdb-sechead", "Lineage"),
    lineage_ribbon(sp),
    tags$div(class = "acdb-sechead", "Relatives in the ACDB", tags$small(" | click to open")),
    if (nrow(rel) == 0) tags$p(class = "acdb-hint", "This is the only species in its class in the database."),
    tags$div(class = "acdb-rel", lapply(seq_len(min(nrow(rel), 12)), function(k) {
      r <- rel[k, ]
      tags$div(class = "it", `data-href` = paste0(r$species_id, ".html"), photo_box(r, lazy = TRUE),
               tags$div(class = "nm", tags$b(tags$a(href = paste0(r$species_id, ".html"), title_case(r$common_name))), tags$i(r$canonicalName)))
    })),
    if (!is.null(order_domains(sid))) tagList(tags$div(class = "acdb-sechead", paste("Species per behavior domain in", sp$Ordr)), order_domains(sid)),
    tags$p(style = "margin-top: 14px;", tags$a(href = "../tree.html", icon("sitemap"), " See every species on the tree of life"))
  )

  sources_ui <- tagList(
    if (has_text(sp$primary_social_unit)) tags$p(
      tags$b("Social unit: "), cap(as.character(sp$primary_social_unit)),
      if (has_text(sp$unit_evidence)) paste0(". ", sp$unit_evidence),
      if (has_text(sp$unit_source) && sp$unit_source %in% pk$refs) paste0(" [", match(sp$unit_source, pk$refs), "]")
    ),
    tags$p(tags$b("Conservation status: "), if (!is.na(sp$IUCN) && sp$IUCN %in% names(iucn_names)) paste0(iucn_names[[sp$IUCN]], " (", sp$IUCN, ")") else "Not evaluated"),
    tags$p(tags$b("GBIF: "), if (has_text(sp$GBIF)) tags$a(href = paste0("https://www.gbif.org/species/", gbif_key(sp$GBIF)), target = "_blank", rel = "noopener", gbif_key(sp$GBIF)) else "Not available"),
    tags$div(class = "acdb-sechead", "References"),
    if (length(pk$refs) == 0) tags$p(class = "acdb-hint", "No sources are linked to this species."),
    tags$ol(class = "acdb-refs", lapply(pk$refs, ref_item))
  )

  pills <- c(Behaviors = "m-behaviors", Map = "m-map", `On the tree` = "m-tree", Sources = "m-sources")
  map_points <- lapply(seq_len(nrow(pk$gr)), function(i) {
    g <- pk$gr[i, ]
    if (is.na(g$lat) || is.na(g$long)) return(NULL)
    list(name = g$group_name, loc = ifelse(is.na(g$location_evidence), "", g$location_evidence), lat = g$lat, lng = g$long)
  })
  map_points <- Filter(Negate(is.null), map_points)

  body <- tags$div(
    class = "acdb-mpage", `data-species` = sid,
    tags$div(
      class = "modal-dialog modal-xl",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-body",
          tags$div(
            class = "acdb-m",
            tags$button(type = "button", class = "acdb-mclose", `aria-label` = "Close", HTML("&times;")),
            tags$div(
              class = "acdb-mhero",
              photo_box(sp, "large"),
              tags$div(
                class = "acdb-mid",
                tags$div(class = "eyebrow", paste(class_label(sp$Class), "|", sp$Ordr)),
                tags$h1(class = "acdb-mtitle", title_case(sp$common_name)),
                tags$div(class = "sci", sp$canonicalName),
                tags$div(
                  if (!is.na(sp$IUCN) && sp$IUCN %in% names(iucn_names)) tags$span(class = paste0("chip iucn-", sp$IUCN), iucn_names[[sp$IUCN]]),
                  domain_chips(doms)
                ),
                lineage_ribbon(sp),
                facts_row(pk, doms)
              )
            ),
            tags$div(
              class = "acdb-mbody",
              about_box(sp),
              tags$ul(class = "nav nav-pills", role = "tablist", lapply(seq_along(pills), function(i) {
                tags$li(class = "nav-item", tags$a(class = paste("nav-link", if (i == 1) "active"), href = paste0("#", pills[i]),
                                                   `data-bs-toggle` = "tab", role = "tab", names(pills)[i]))
              })),
              tags$div(
                class = "tab-content",
                tags$div(class = "tab-pane active", id = "m-behaviors", role = "tabpanel", behaviors_ui),
                tags$div(class = "tab-pane", id = "m-map", role = "tabpanel", tags$div(id = "modal_map", style = "height: 400px;")),
                tags$div(class = "tab-pane", id = "m-tree", role = "tabpanel", tree_ui),
                tags$div(class = "tab-pane", id = "m-sources", role = "tabpanel", sources_ui)
              )
            )
          )
        ),
        tags$div(
          class = "modal-footer",
          tags$button(type = "button", id = "modal_pdf", class = "btn btn-primary", icon("download"), " Download card (PDF)"),
          tags$button(type = "button", id = "copy_link", class = "btn btn-default", icon("link"), " Copy link"),
          tags$button(type = "button", id = "modal_close", class = "btn btn-default", "Close")
        )
      )
    ),
    tags$template(id = "acdb-card", species_card(sid, root)),
    HTML("<script type=\"application/json\" id=\"acdb-points\">", to_json(map_points), "</script>")
  )

  common <- title_case(sp$common_name)
  behaviors <- unique(unlist(lapply(pk$bh, function(b) b$behavior)))
  description <- sprintf("%s (%s): %d population%s with socially learned behaviors in the Animal Culture Database%s",
                         common, sp$canonicalName, nrow(pk$gr), if (nrow(pk$gr) == 1) "" else "s",
                         if (length(behaviors) > 0) paste0(", including ", paste(head(behaviors, 4), collapse = ", "), ".") else ".")
  page(species_href(sid), sprintf("%s (%s): animal culture | Animal Culture Database", common, sp$canonicalName), description, "", body)
}

invisible(lapply(sp_sorted$species_id, species_page))

# Data for the browser ---------------------------------------------------------------------------------

first_behavior <- function(gid) behaviors_table[id_text(behaviors_table$group_id) == gid, ][1, ]

data_groups <- lapply(seq_len(nrow(groups_with_details)), function(i) {
  g <- groups_with_details[i, ]
  gid <- id_text(g$group_id)
  b <- first_behavior(gid)
  list(
    id = gid, sp = g$species_id, name = g$group_name, common = g$common_name, sci = g$canonicalName,
    cls = g$Class, ord = g$Ordr, fam = g$Family, iucn = g$IUCN, domains = I(split_domains(g$domains)),
    n = g$behavior_count, loc = g$location_evidence, lat = g$lat, lng = g$long, region = g$region,
    beh = if (g$behavior_count > 0) b$behavior else NA, lead = if (g$behavior_count > 0) lead_text(b$behavior_description, 260) else NA
  )
})

data_species <- setNames(lapply(seq_len(nrow(sp_info)), function(i) {
  s <- sp_info[i, ]
  list(common = title_case(s$common_name), sort = tolower(s$common_name), sci = s$canonicalName, cls = s$Class)
}), sp_info$species_id)

na0 <- function(x) ifelse(is.na(x), "", as.character(x))
tree <- list(
  tips = lapply(seq_len(nrow(sp_info)), function(i) {
    s <- sp_info[i, ]
    list(id = s$species_id, common = title_case(s$common_name), sci = na0(s$canonicalName),
         gen = na0(s$Genus), fam = na0(s$Family), ord = na0(s$Ordr), cls = na0(s$Class), phy = na0(s$Phylum),
         iucn = na0(s$IUCN), groups = s$groups, behaviors = s$behaviors, domains = I(split_domains(s$domains)))
  }),
  domains = I(domain_levels),
  domainColors = setNames(as.list(slot(seq_along(domain_levels))), domain_levels),
  domainHex = setNames(as.list(acdb_palette[slot(seq_along(domain_levels))]), domain_levels),
  classColors = list(Aves = "#3A86C8", Mammalia = "#E76F51", Insecta = "#7B5EA7")
)

data_js <- paste0(
  "// Made by site/build_site.R from the database. Do not edit by hand\n",
  "window.ACDB = ", to_json(list(
    version = acdb_version,
    domains = I(domain_levels),
    classNames = as.list(class_names),
    iucnNames = as.list(iucn_names),
    groups = data_groups,
    species = data_species,
    tree = tree
  )), ";\n"
)

dir.create(file.path(out, "assets"), recursive = TRUE, showWarnings = FALSE)
writeLines(enc2utf8(data_js), file.path(out, "assets", "data.js"), useBytes = TRUE)

# Files ------------------------------------------------------------------------------------------------

# The app's own styles, scripts, and logos, then the libraries and the site files
for (f in c("acdb.css", "card.css", "photos.js", "tree.js", "print.js", "DDLLogo_white_small.png", "DDLLogo_white.png")) {
  stopifnot(file.copy(file.path("www", f), file.path(out, "assets", f), overwrite = TRUE))
}
stopifnot(file.copy("site/vendor", file.path(out, "assets"), recursive = TRUE, overwrite = TRUE))
for (f in c("site.js", "static.css")) stopifnot(file.copy(file.path("site", f), file.path(out, "assets", f), overwrite = TRUE))

# Clean UTF-8 copies of the tables
dir.create(file.path(out, "data"), showWarnings = FALSE)
for (name in c("species", "groups", "behaviors", "sources")) {
  write.csv(get(paste0(name, "_table")), file.path(out, "data", paste0(name, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
}

# Old links to the app (?species=, ?tab=, #species=) land on the new pages, see site.js. Unknown addresses get the 404 page
page("404.html", "Page not found | Animal Culture Database", "This page does not exist.", "",
     tags$div(class = "acdb-section acdb-narrow", tags$h3("Page not found"),
              tags$p("This page does not exist. Try the ", tags$a(href = paste0(site_url, "populations.html"), "populations"), " or the ",
                     tags$a(href = site_url, "home page"), ".")))
# The 404 page can be served at any address, so its links and files use the full address of the site
html <- readLines(file.path(out, "404.html"), warn = FALSE, encoding = "UTF-8")
html <- gsub("(href|src)=\"(assets/|index.html|map.html|populations.html|tree.html|cards.html|statistics.html|citations.html|papers.html|about.html|contribute.html|help.html)",
             paste0("\\1=\"", site_url, "\\2"), html)
writeLines(html, file.path(out, "404.html"), useBytes = TRUE)

pages <- c("", "map.html", "populations.html", "tree.html", "cards.html", "statistics.html", "citations.html",
           "papers.html", "about.html", "contribute.html", "help.html", species_href(sp_sorted$species_id))
writeLines(c(
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
  "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
  sprintf("<url><loc>%s%s</loc></url>", site_url, pages),
  "</urlset>"
), file.path(out, "sitemap.xml"))

message("Built ", length(pages), " pages in ", out)
