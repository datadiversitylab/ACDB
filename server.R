###################
# server.R
# 
# Server controller. 
# Used to define the back-end aspects of the app.
###################

library(RSQLite)

# Version shown on the species cards
acdb_version <- if (file.exists("VERSION")) trimws(readLines("VERSION", warn = FALSE)[1]) else ""

# Colors, in the same order as the chip classes in acdb.css
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

# Text for HTML: no tags (some titles carry <i>), and escaped
esc <- function(x) htmltools::htmlEscape(gsub("<[^>]+>", "", ifelse(is.na(x), "", as.character(x))))
cap <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
has_text <- function(x) !is.na(x) && nzchar(trimws(as.character(x)))

# Names to try on Wikipedia, from the scientific name
wiki_names <- function(g) {
  name <- as.character(g$canonicalName)
  if (is.na(name)) "" else paste(unique(c(name, paste(head(strsplit(name, " ")[[1]], 2), collapse = " "))), collapse = "|")
}

# First paragraph from Wikipedia, with its credit. The text is loaded in the browser by photos.js
about_box <- function(g, class = "") {
  tags$div(class = paste("acdb-about", class), `data-species` = wiki_names(g))
}

# Photo box for a group or a species. The photo itself is loaded in the browser by photos.js
photo_box <- function(g, size = "") {
  name <- as.character(g$canonicalName)
  names <- unique(c(name, paste(head(strsplit(name, " ")[[1]], 2), collapse = " ")))
  panel <- unname(c(Aves = "birds", Mammalia = "mammals")[as.character(g$Class)])
  tags$div(
    class = paste("acdb-photo", if (is.na(panel)) "other" else panel, size),
    `data-species` = if (is.na(name)) "" else paste(names, collapse = "|"),
    `data-size` = size,
    tags$img(alt = as.character(g$common_name)),
    tags$div(class = "none", icon("paw")),
    tags$span(class = "acdb-credit", tags$a(href = "#", target = "_blank", onclick = "event.stopPropagation();", "Photo: Wikipedia"))
  )
}

# Horizontal bars for a named vector of counts
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

# Table with shaded cells. Rows and columns come from a matrix of counts
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

# Transmission answers, as a style and a label
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

# The description starts with a short summary, followed by quoted evidence
lead_text <- function(x, n = 300) {
  x <- as.character(x)
  if (!has_text(x)) return("")
  first <- trimws(strsplit(x, "\"")[[1]][1])
  if (is.na(first) || nchar(first) < 20) first <- trimws(x)
  if (nchar(first) > n) first <- paste0(sub("\\s+\\S*$", "", substr(first, 1, n)), "...")
  cap(first)
}

# First author and et al. for long author lists
short_author <- function(a) {
  parts <- strsplit(as.character(a), " and ")[[1]]
  last <- sub(",.*", "", parts)
  if (length(last) > 2) paste(last[1], "et al.") else paste(last, collapse = " and ")
}

# Dates of a behavior, when they were coded
when_text <- function(b) {
  ok <- function(x) has_text(x) && as.character(x) != "0"
  if (ok(b$start_date) && ok(b$end_date)) paste("Recorded from", b$start_date, "to", b$end_date)
  else if (ok(b$start_date)) paste("Recorded since", b$start_date)
  else if (ok(b$end_date)) paste("Recorded until", b$end_date)
}

source_cols <- c("behavior_source", "vertical_source", "horizontal_source",
                 "oblique_source", "domains_source", "anth_source")

server <- function(input, output, session) {
  # Connect to SQLite database
  db_path <- "db/ACDB_dev.sql"
  conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Read tables from the SQLite database
  groups_table <- dbReadTable(conn, "groups")
  species_table <- dbReadTable(conn, "species")
  behaviors_table <- dbReadTable(conn, "behaviors")
  sources_table <- dbReadTable(conn, "sources")
  
  # Ensures that the connection is closed when the session ends
  session$onSessionEnded(function() dbDisconnect(conn))
  
  # Calculate the number of recorded behaviors per group
  behavior_counts <- aggregate(behavior_id ~ group_id, data = behaviors_table, FUN = length)
  colnames(behavior_counts)[2] <- "behavior_count"  # Rename column for clarity
  
  # Join groups with species (names) and behavior counts
  groups_with_details <- merge(groups_table, species_table, by = "species_id", all.x = TRUE)
  groups_with_details <- merge(groups_with_details, behavior_counts, by = "group_id", all.x = TRUE)
  
  # Replace NA behavior counts with 0 (for groups with no behaviors)
  groups_with_details$behavior_count[is.na(groups_with_details$behavior_count)] <- 0
  
  # Behavior domains of each group, as one text
  split_domains <- function(x) {
    d <- unique(trimws(unlist(strsplit(as.character(x), "[;,]"))))
    d[!is.na(d) & d != ""]
  }
  group_domains <- tapply(behaviors_table$domains, behaviors_table$group_id, function(x) paste(split_domains(x), collapse = "; "))
  groups_with_details$domains <- as.vector(group_domains[as.character(groups_with_details$group_id)])
  groups_with_details$domains[is.na(groups_with_details$domains)] <- ""
  
  # Region of each group, from its coordinates. Later rules override earlier ones
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
  
  # One row per species, with its number of populations, behaviors, and domains
  sp_info <- species_table
  sp_info$groups <- as.integer(table(factor(groups_table$species_id, levels = sp_info$species_id)))
  behaviors_by_species <- tapply(groups_with_details$behavior_count, groups_with_details$species_id, sum)
  sp_info$behaviors <- as.vector(behaviors_by_species[sp_info$species_id])
  sp_info$behaviors[is.na(sp_info$behaviors)] <- 0
  domains_by_species <- tapply(groups_with_details$domains, groups_with_details$species_id, function(x) paste(split_domains(x), collapse = "; "))
  sp_info$domains <- as.vector(domains_by_species[sp_info$species_id])
  sp_info$domains[is.na(sp_info$domains)] <- ""
  
  # Names, colors, and choices for the filters
  domain_levels <- sort(split_domains(behaviors_table$domains))
  class_label <- function(x) ifelse(is.na(class_names[x]), x, unname(class_names[x]))
  domain_chips <- function(d) lapply(d, function(x) tags$span(class = paste0("chip chip-", slot(match(x, domain_levels))), cap(x)))
  has_domain <- function(domains, dm) vapply(strsplit(domains, "; "), function(x) dm %in% x, logical(1))
  
  classes <- sort(unique(groups_with_details$Class))
  class_choices <- c("All taxonomic groups" = "All", setNames(classes, class_label(classes)))
  domain_choices <- c("All behavior domains" = "All", setNames(domain_levels, cap(domain_levels)))
  for (id in c("map_class", "pop_class")) updateSelectInput(session, id, choices = class_choices)
  for (id in c("map_domain", "pop_domain")) updateSelectInput(session, id, choices = domain_choices)
  
  # Species choices for the cards tab, by common name
  sp_sorted <- sp_info[order(tolower(sp_info$common_name)), ]
  card_choices <- setNames(sp_sorted$species_id, sprintf("%s (%s)", tools::toTitleCase(as.character(sp_sorted$common_name)), sp_sorted$canonicalName))
  updateSelectizeInput(session, "card_species", choices = card_choices, selected = sample(sp_sorted$species_id, 1))
  
  # Filter the groups by class, behavior domain, and a text search
  filter_groups <- function(d, class, domain, q = "") {
    if (!is.null(class) && class != "All") d <- d[which(d$Class == class), ]
    if (!is.null(domain) && domain != "All") d <- d[has_domain(d$domains, domain), ]
    if (!is.null(q) && nzchar(q)) {
      text <- tolower(paste(d$group_name, d$canonicalName, d$common_name, d$location_evidence))
      d <- d[grepl(tolower(q), text, fixed = TRUE), ]
    }
    d
  }
  
  # Move between tabs from buttons
  go_to <- c(go_map = "Map_tab", go_table = "Populations_tab", go_tree = "Tree_tab", go_contribute = "Contributions_tab")
  lapply(names(go_to), function(id) {
    observeEvent(input[[id]], { updateTabsetPanel(session = session, inputId = "tabs", selected = go_to[[id]]) })
  })
  
  # Landing page
  output$hero_chips <- renderUI({
    tags$div(class = "acdb-chips", tags$span(class = "lab", "Traditions we track"), domain_chips(setdiff(domain_levels, "other")))
  })
  
  # Example population
  pick_example <- function() {
    ok <- groups_with_details[groups_with_details$behavior_count > 0, ]
    ok$group_id[sample.int(nrow(ok), 1)]
  }
  example_id <- reactiveVal(pick_example())
  
  observeEvent(input$another, { example_id(pick_example()) })
  
  output$featured <- renderUI({
    g <- groups_with_details[groups_with_details$group_id == example_id(), ]
    b <- behaviors_table[behaviors_table$group_id == example_id(), ]
    text <- lead_text(b$behavior_description[1], 260)
    
    chips <- tagList(
      tags$span(class = "chip chip-class", class_label(g$Class)),
      if (!is.na(g$IUCN) && g$IUCN %in% names(iucn_names)) tags$span(class = paste0("chip iucn-", g$IUCN), iucn_names[[g$IUCN]]),
      domain_chips(split_domains(b$domains))
    )
    
    tags$div(
      class = "acdb-ex-body",
      photo_box(g, "large"),
      tags$div(
        class = "acdb-ex-text",
        chips,
        tags$h4(g$group_name),
        tags$div(class = "sp", tags$i(g$canonicalName), " | ", tools::toTitleCase(g$common_name)),
        tags$div(class = "beh", b$behavior[1]),
        tags$p(text),
        tags$div(class = "where", icon("map-marker-alt"), " ", g$location_evidence, " | ", nrow(b), if (nrow(b) == 1) " behavior recorded" else " behaviors recorded"),
        actionButton("example_open", "See full details", class = "btn-primary"),
        actionButton("example_card", "Get the card (PDF)", icon = icon("download"), class = "btn-default", style = "margin-left: 8px;")
      )
    )
  })
  
  # Map
  map_data <- reactive({
    d <- filter_groups(groups_with_details, input$map_class, input$map_domain)
    d[!is.na(d$lat) & !is.na(d$long), ]
  })
  
  output$population_map <- renderLeaflet({
    d <- map_data()
    req(nrow(d) > 0)
    leaflet(data = d, options = leafletOptions(minZoom = 2, maxBoundsViscosity = 1)) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, options = providerTileOptions(noWrap = TRUE)) %>%
      setMaxBounds(-180, -85, 180, 85) %>%
      fitBounds(min(d$long), min(d$lat), max(d$long), max(d$lat)) %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat,
        radius = 7,  # Marker size
        color = "#ffffff",  # Outline color
        fillColor = "#E76F51",  # Fill color
        fillOpacity = 0.8,  # Transparency
        weight = 1.5,  # Border weight
        label = ~paste0(group_name, " (", behavior_count, ifelse(behavior_count == 1, " behavior)", " behaviors)")),  # Quick hover label
        popup = ~paste0(
          "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.6; color: #333;'>",
          "<b style='font-size: 16px; color: #1B7F79;'>", htmltools::htmlEscape(group_name), "</b><br>",
          "<b>Species:</b> <i>", canonicalName, "</i><br>",
          "<b>Common name:</b> ", htmltools::htmlEscape(common_name), "<br>",
          "<b>Recorded behaviors:</b> ", behavior_count, "<br>",
          "<b>Location:</b> ", htmltools::htmlEscape(location_evidence), "<br>",
          "<a href='#' style='color: #0F5F5A; text-decoration: underline;' ",
          "onclick='Shiny.setInputValue(\"map_click\", \"", group_id, "\", {priority: \"event\"}); return false;'>Open the species card</a>",
          "</div>"
        )
        , clusterOptions = markerClusterOptions()
      )
  })
  
  # Species in view. The list follows the map as the user pans and zooms
  map_bounds <- debounce(reactive(input$population_map_bounds), 300)
  
  in_view <- reactive({
    d <- map_data()
    b <- map_bounds()
    if (!is.null(b)) d <- d[d$lat >= b$south & d$lat <= b$north & d$long >= b$west & d$long <= b$east, ]
    d
  })
  
  output$side_note <- renderText({
    d <- in_view()
    n <- length(unique(d$species_id))
    paste(n, "species |", nrow(d), if (nrow(d) == 1) "population" else "populations")
  })
  
  output$map_species <- renderUI({
    d <- in_view()
    if (nrow(d) == 0) return(tags$p(class = "acdb-hint", style = "padding: 12px;", "No populations in this view. Zoom out or move the map."))
    ids <- unique(d$species_id)
    ids <- ids[order(tolower(sp_info$common_name[match(ids, sp_info$species_id)]))]
    shown_ids <- head(ids, 40)
    tagList(
      lapply(shown_ids, function(id) {
        s <- sp_info[sp_info$species_id == id, ]
        n <- sum(d$species_id == id)
        tags$div(
          class = "acdb-sp",
          onclick = sprintf("Shiny.setInputValue('species_click', '%s', {priority: 'event'});", id),
          photo_box(s, "mini"),
          tags$div(
            tags$b(tools::toTitleCase(as.character(s$common_name))),
            tags$i(s$canonicalName),
            tags$small(n, if (n == 1) " population here | " else " populations here | ", class_label(s$Class))
          )
        )
      }),
      if (length(ids) > length(shown_ids)) tags$p(class = "acdb-hint", style = "padding: 8px 12px;", "Zoom in to see the other species.")
    )
  })
  
  # Populations: filters shared by the gallery and the table
  pop_data <- reactive({
    d <- filter_groups(groups_with_details, input$pop_class, input$pop_domain, input$pop_q)
    if (identical(input$pop_sort, "behaviors")) d[order(-d$behavior_count, d$group_name), ] else d[order(tolower(d$group_name)), ]
  })
  
  output$pop_note <- renderText({ paste("Showing", nrow(pop_data()), "of", nrow(groups_with_details), "populations") })
  
  # Gallery, 12 cards at a time
  shown <- reactiveVal(12)
  observeEvent(list(input$pop_q, input$pop_class, input$pop_domain, input$pop_sort), { shown(12) })
  observeEvent(input$more, { shown(shown() + 12) })
  
  output$pop_cards <- renderUI({
    d <- head(pop_data(), shown())
    if (nrow(d) == 0) return(tags$p(class = "acdb-hint", "No populations match these filters."))
    tags$div(
      class = "acdb-grid",
      lapply(seq_len(nrow(d)), function(i) {
        g <- d[i, ]
        tags$div(
          class = "acdb-card",
          onclick = sprintf("Shiny.setInputValue('map_click', '%s', {priority: 'event'});", g$group_id),
          photo_box(g),
          tags$div(
            class = "acdb-card-body",
            tags$div(tags$span(class = "chip chip-class", class_label(g$Class)), head(domain_chips(split_domains(g$domains)), 2)),
            tags$h5(g$group_name),
            tags$div(class = "sp", tags$i(g$canonicalName)),
            tags$div(class = "where", g$behavior_count, if (g$behavior_count == 1) " behavior | " else " behaviors | ", g$location_evidence)
          )
        )
      })
    )
  })
  
  output$pop_more <- renderUI({
    if (nrow(pop_data()) > shown()) actionButton("more", "Show more", class = "acdb-btn-more")
  })
  
  # Download the populations that match the filters
  output$pop_csv <- downloadHandler(
    filename = function() paste0("ACDB_populations_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- pop_data()
      track("csv_download", rows = nrow(d))
      out <- data.frame(
        group = d$group_name, species = d$canonicalName, common_name = d$common_name, class = d$Class,
        order = d$Ordr, family = d$Family, domains = d$domains, behaviors = d$behavior_count,
        location = d$location_evidence, lat = d$lat, long = d$long, region = d$region
      )
      write.csv(out, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # Table
  output$groups_table <- renderDataTable({
    d <- pop_data()
    table_df <- data.frame(
      Photo = vapply(seq_len(nrow(d)), function(i) as.character(photo_box(d[i, ], "mini")), ""),
      Group = esc(d$group_name),
      Species = sprintf("<i>%s</i><br><small>%s</small>", esc(d$canonicalName), esc(tools::toTitleCase(as.character(d$common_name)))),
      Taxon = sprintf('<span class="chip chip-class">%s</span>', class_label(d$Class)),
      Domains = vapply(strsplit(d$domains, "; "), function(x) paste(vapply(domain_chips(x), as.character, ""), collapse = ""), ""),
      Behaviors = d$behavior_count,
      Location = esc(d$location_evidence),
      stringsAsFactors = FALSE
    )
    datatable(
      table_df,
      selection = "single",
      escape = FALSE,
      class = "hover",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = c(0, 3, 4))))
    )
  })
  
  # Statistics
  output$stats_kpis <- renderUI({
    kpis <- list(
      list(n = nrow(groups_table), label = "Populations"),
      list(n = length(unique(species_table$species_id)), label = "Species"),
      list(n = nrow(behaviors_table), label = "Behaviors"),
      list(n = nrow(sources_table), label = "Sources")
    )
    tags$div(
      class = "acdb-kpis",
      lapply(seq_along(kpis), function(i) {
        tags$div(class = paste0("kpi chip-", i), tags$b(kpis[[i]]$n), tags$span(kpis[[i]]$label))
      })
    )
  })
  
  output$stats_taxa <- renderUI({
    req(input$stats_rank)
    counts <- sort(table(groups_with_details[[input$stats_rank]]), decreasing = TRUE)
    counts <- head(counts, 10)
    labels <- if (input$stats_rank == "Class") class_label(names(counts)) else names(counts)
    bars(setNames(as.integer(counts), labels))
  })
  
  output$stats_region <- renderUI({
    counts <- sort(table(groups_with_details$region), decreasing = TRUE)
    bars(setNames(as.integer(counts), names(counts)))
  })
  
  output$stats_domain <- renderUI({
    counts <- vapply(domain_levels, function(dm) sum(has_domain(groups_with_details$domains, dm)), numeric(1))
    colors <- acdb_palette[slot(match(names(counts), domain_levels))]
    o <- order(counts, decreasing = TRUE)
    bars(setNames(counts[o], cap(names(counts))[o]), colors[o])
  })
  
  output$stats_iucn <- renderUI({
    status <- species_table$IUCN
    status[is.na(status)] <- "Not evaluated"
    counts <- table(factor(status, levels = c(names(iucn_names), "Not evaluated")))
    counts <- counts[counts > 0]
    labels <- ifelse(names(counts) %in% names(iucn_names), iucn_names[names(counts)], names(counts))
    colors <- ifelse(names(counts) %in% names(iucn_colors), iucn_colors[names(counts)], "#B8C4C8")
    bars(setNames(as.integer(counts), labels), unname(colors))
  })
  
  output$stats_cross <- renderUI({
    counts <- table(factor(groups_with_details$region, levels = region_order), groups_with_details$Class)
    counts <- counts[rowSums(counts) > 0, , drop = FALSE]
    heat_table(counts, "Region", class_label(colnames(counts)))
  })
  
  # Species with each behavior domain, by taxonomic group. The domains are not tied to the taxonomy
  output$stats_domain_class <- renderUI({
    counts <- sapply(classes, function(cl) {
      s <- sp_info[sp_info$Class == cl, ]
      sapply(domain_levels, function(dm) sum(has_domain(s$domains, dm)))
    })
    rownames(counts) <- cap(domain_levels)
    heat_table(counts, "Behavior domain", class_label(colnames(counts)))
  })
  
  # Tree of life, built from the taxonomy. The browser draws it (tree.js)
  tree_sent <- reactiveVal(FALSE)
  observeEvent(input$tabs, {
    req(input$tabs == "Tree_tab", !tree_sent())
    na0 <- function(x) ifelse(is.na(x), "", as.character(x))
    tips <- lapply(seq_len(nrow(sp_info)), function(i) {
      s <- sp_info[i, ]
      list(id = s$species_id, common = tools::toTitleCase(na0(s$common_name)), sci = na0(s$canonicalName),
           gen = na0(s$Genus), fam = na0(s$Family), ord = na0(s$Ordr), cls = na0(s$Class), phy = na0(s$Phylum),
           iucn = na0(s$IUCN), groups = s$groups, behaviors = s$behaviors, domains = as.list(split_domains(s$domains)))
    })
    session$sendCustomMessage("acdb_tree", list(
      tips = tips,
      domains = as.list(domain_levels),
      domainColors = setNames(as.list(slot(seq_along(domain_levels))), domain_levels),
      domainHex = setNames(as.list(acdb_palette[slot(seq_along(domain_levels))]), domain_levels),
      classColors = list(Aves = "#3A86C8", Mammalia = "#E76F51", Insecta = "#7B5EA7")
    ))
    tree_sent(TRUE)
  })
  
  # Everything we know about one species: the row, its populations with their behaviors, and its sources
  species_pack <- function(sid, gid = NULL) {
    sp <- species_table[species_table$species_id == sid, ][1, ]
    gr <- groups_table[groups_table$species_id == sid, ]
    gr <- gr[order(gr$group_name), ]
    if (!is.null(gid)) gr <- gr[order(gr$group_id != gid), ]
    bh <- lapply(gr$group_id, function(g) behaviors_table[behaviors_table$group_id == g, ])
    refs <- unlist(lapply(bh, function(b) unlist(b[, source_cols], use.names = FALSE)))
    refs <- unique(c(refs, sp$unit_source))
    refs <- refs[refs %in% sources_table$source_id]
    list(sp = sp, gr = gr, bh = bh, refs = refs)
  }
  
  # Sources of one behavior, as numbers in the list of the species
  behavior_refs <- function(b, refs) {
    ids <- unique(unlist(b[, source_cols], use.names = FALSE))
    sort(match(ids[ids %in% refs], refs))
  }
  
  # Reference in the style used in the app
  ref_text <- function(s) {
    paste0(short_author(s$authors), " (", s$year, "). ", sub("\\.$", "", gsub("<[^>]+>", "", s$title)), ".")
  }
  
  ref_item <- function(id) {
    s <- sources_table[sources_table$source_id == id, ][1, ]
    tags$li(
      ref_text(s), " ",
      if (has_text(s$doi)) tags$a(href = paste0("https://doi.org/", s$doi), target = "_blank", paste0("https://doi.org/", s$doi))
    )
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
  
  transmission_pills <- function(b) {
    lapply(c("vertical", "horizontal", "oblique"), function(m) {
      s <- tstate(b[[m]])
      tags$span(class = paste("tpill", s[1]), paste0(cap(m), ": ", s[2]))
    })
  }
  
  # Relatives of a species in the tree: same order, or same class when it is alone
  relatives_of <- function(sid) {
    sp <- sp_info[sp_info$species_id == sid, ]
    rel <- sp_info[sp_info$Ordr == sp$Ordr & sp_info$species_id != sid, ]
    if (nrow(rel) == 0) rel <- sp_info[sp_info$Class == sp$Class & sp_info$species_id != sid, ]
    rel[order(tolower(rel$common_name)), ]
  }
  
  # Species in the order of a species with each behavior domain
  order_domains <- function(sid) {
    sp <- sp_info[sp_info$species_id == sid, ]
    o <- sp_info[sp_info$Ordr == sp$Ordr, ]
    counts <- vapply(domain_levels, function(dm) sum(has_domain(o$domains, dm)), numeric(1))
    counts <- sort(counts[counts > 0], decreasing = TRUE)
    if (nrow(o) < 2 || length(counts) == 0) return(NULL)
    bars(setNames(counts, cap(names(counts))), acdb_palette[slot(match(names(counts), domain_levels))])
  }
  
  # Tell the page around the app (see www/embed.js) what people do, so that the host site can count it
  track <- function(name, ...) session$sendCustomMessage("acdb_track", list(name = name, data = list(...)))
  
  # Species window, opened from the map, the gallery, the table, the tree, and the cards
  modal_species <- reactiveVal(NULL)
  
  showSpeciesModal <- function(sid, gid = NULL) {
    req(sid %in% species_table$species_id)
    pk <- species_pack(sid, gid)
    sp <- pk$sp
    info <- sp_info[sp_info$species_id == sid, ]
    doms <- split_domains(info$domains)
    modal_species(sid)
    track("species_open", species = sid)
    
    # Populations and their behaviors
    behaviors_ui <- lapply(seq_len(nrow(pk$gr)), function(i) {
      g <- pk$gr[i, ]
      b <- pk$bh[[i]]
      tags$div(
        class = paste("acdb-bgroup", if (!is.null(gid) && g$group_id == gid) "selected"),
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
              evid("Domains: ", x$domains_evidence),
              evid("Anthropogenic effects: ", x$anth_effects)
            ),
            if (length(nums) > 0) tags$div(class = "src", "Sources: ", paste0("[", nums, "]", collapse = " "))
          )
        })
      )
    })
    
    # Place on the tree
    rel <- relatives_of(sid)
    tree_ui <- tagList(
      tags$div(class = "acdb-sechead", "Lineage"),
      lineage_ribbon(sp),
      tags$div(class = "acdb-sechead", paste("Relatives in the ACDB"), tags$small(" | click to open")),
      if (nrow(rel) == 0) tags$p(class = "acdb-hint", "This is the only species in its class in the database."),
      tags$div(
        class = "acdb-rel",
        lapply(seq_len(min(nrow(rel), 12)), function(k) {
          r <- rel[k, ]
          tags$div(
            class = "it",
            onclick = sprintf("Shiny.setInputValue('species_click', '%s', {priority: 'event'});", r$species_id),
            photo_box(r),
            tags$div(class = "nm", tags$b(tools::toTitleCase(as.character(r$common_name))), tags$i(r$canonicalName))
          )
        })
      ),
      if (!is.null(order_domains(sid))) tagList(tags$div(class = "acdb-sechead", paste("Species per behavior domain in", sp$Ordr)), order_domains(sid)),
      tags$p(style = "margin-top: 14px;", actionLink("modal_tree", tagList(icon("sitemap"), " See every species on the tree of life")))
    )
    
    # Sources
    sources_ui <- tagList(
      if (has_text(sp$primary_social_unit)) tags$p(
        tags$b("Social unit: "), cap(as.character(sp$primary_social_unit)),
        if (has_text(sp$unit_evidence)) paste0(". ", sp$unit_evidence),
        if (has_text(sp$unit_source) && sp$unit_source %in% pk$refs) paste0(" [", match(sp$unit_source, pk$refs), "]")
      ),
      tags$p(tags$b("Conservation status: "), if (!is.na(sp$IUCN) && sp$IUCN %in% names(iucn_names)) paste0(iucn_names[[sp$IUCN]], " (", sp$IUCN, ")") else "Not evaluated"),
      tags$p(tags$b("GBIF: "), if (has_text(sp$GBIF)) tags$a(href = paste0("https://www.gbif.org/species/", sp$GBIF), target = "_blank", as.character(sp$GBIF)) else "Not available"),
      tags$div(class = "acdb-sechead", "References"),
      if (length(pk$refs) == 0) tags$p(class = "acdb-hint", "No sources are linked to this species."),
      tags$ol(class = "acdb-refs", lapply(pk$refs, ref_item))
    )
    
    showModal(modalDialog(
      size = "xl",
      easyClose = TRUE,
      title = NULL,
      footer = tagList(
        actionButton("modal_pdf", "Download card (PDF)", icon = icon("download"), class = "btn-primary"),
        tags$button(
          type = "button", class = "btn btn-default",
          onclick = sprintf("acdbCopyLink('%s', this);", sid),
          icon("link"), " Copy link"
        ),
        modalButton("Close")
      ),
      tags$div(
        class = "acdb-m",
        tags$button(type = "button", class = "acdb-mclose", `data-dismiss` = "modal", `data-bs-dismiss` = "modal", `aria-label` = "Close", HTML("&times;")),
        tags$div(
          class = "acdb-mhero",
          photo_box(sp, "large"),
          tags$div(
            class = "acdb-mid",
            tags$div(class = "eyebrow", paste(class_label(sp$Class), "|", sp$Ordr)),
            tags$h2(tools::toTitleCase(as.character(sp$common_name))),
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
          tabsetPanel(
            id = "modal_tabs",
            type = "pills",
            tabPanel("Behaviors", behaviors_ui),
            tabPanel("Map", leafletOutput("modal_map", height = 400)),
            tabPanel("On the tree", tree_ui),
            tabPanel("Sources", sources_ui)
          )
        )
      )
    ))
  }
  
  # Map in the species window, with every population of the species
  output$modal_map <- renderLeaflet({
    sid <- modal_species()
    req(sid)
    d <- groups_table[groups_table$species_id == sid & !is.na(groups_table$lat) & !is.na(groups_table$long), ]
    req(nrow(d) > 0)
    m <- leaflet(data = d) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~long, lat = ~lat, radius = 9, color = "#ffffff", fillColor = "#E76F51", fillOpacity = 0.85, weight = 2,
        label = ~group_name,
        popup = ~paste0("<b>", htmltools::htmlEscape(group_name), "</b><br>", htmltools::htmlEscape(location_evidence))
      )
    if (nrow(d) == 1) setView(m, d$long, d$lat, zoom = 6) else fitBounds(m, min(d$long) - 1, min(d$lat) - 1, max(d$long) + 1, max(d$lat) + 1)
  })
  
  observeEvent(input$modal_tree, {
    removeModal()
    updateTabsetPanel(session = session, inputId = "tabs", selected = "Tree_tab")
  })
  
  # Open the window from the table, the map, the gallery, the tree, and the example
  species_of_group <- function(gid) groups_table$species_id[groups_table$group_id == gid][1]
  table_proxy <- dataTableProxy("groups_table")
  
  observeEvent(input$groups_table_rows_selected, {
    gid <- pop_data()$group_id[input$groups_table_rows_selected]
    showSpeciesModal(species_of_group(gid), gid)
    selectRows(table_proxy, NULL)
  })
  
  observeEvent(input$map_click, { showSpeciesModal(species_of_group(input$map_click), as.integer(input$map_click)) })
  observeEvent(input$species_click, { showSpeciesModal(input$species_click) })
  observeEvent(input$example_open, { showSpeciesModal(species_of_group(example_id()), example_id()) })
  
  # Links such as ?species=Megaptera_novaeangliae or ?tab=Map_tab open that species or tab
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (!is.null(q$tab) && q$tab %in% c("Map_tab", "Populations_tab", "Tree_tab", "Cards_tab", "Stats_tab", "Citations_tab", "About_tab")) {
      updateTabsetPanel(session = session, inputId = "tabs", selected = q$tab)
    }
    if (!is.null(q$species) && q$species %in% species_table$species_id) showSpeciesModal(q$species)
  }, once = TRUE)
  
  # Printable card. The same markup is shown in the Cards tab and sent to the browser to print
  species_card <- function(sid) {
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
    
    tree_text <- if (n_order > 1) {
      paste0(sp$Ordr, " has ", n_order, " species in the ACDB. Relatives: ")
    } else {
      paste0("It is the only ", sp$Ordr, " in the ACDB. Others in ", sp$Class, ": ")
    }
    tree_p <- if (nrow(rel) == 0) {
      tags$p("It is the only species of its class in the ACDB.")
    } else {
      tags$p(tree_text, lapply(seq_len(min(nrow(rel), 8)), function(k) tags$span(if (k > 1) ", ", tags$i(rel$canonicalName[k]))), ".")
    }
    
    tags$div(
      class = "acdb-pdf",
      tags$div(
        class = paste("pdf-band", panel),
        tags$img(class = "pdf-logo", src = "DDLLogo_white_small.png"),
        tags$div(class = "t", tags$small("ANIMAL CULTURE DATABASE"), tags$b("Species card")),
        tags$div(class = "v", tags$b(paste("Version", acdb_version)), sub(" 0", " ", format(Sys.Date(), "%B %d, %Y")))
      ),
      tags$div(
        class = "pdf-hero",
        photo_box(sp, "large"),
        tags$div(
          class = "pdf-id",
          tags$h1(tools::toTitleCase(as.character(sp$common_name))),
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
      tags$div(
        class = "pdf-tree",
        tree_p,
        order_domains(sid)
      ),
      tags$h2(class = "pdf-h", "Sources"),
      tags$ol(class = "pdf-refs", lapply(pk$refs, ref_item)),
      tags$div(
        class = "pdf-foot",
        tags$img(src = "DDLLogo_white_small.png"),
        tags$div(
          paste0("Data from the Animal Culture Database (ACDB), version ", acdb_version, ". Basava K, Alam MNU, Roberts L, Martinet K, Cherry P, García-Verdugo H, Román-Palacios C (2025). Mapping nonhuman cultures with the Animal Culture Database. Scientific Data 12. "),
          tags$a(href = "https://doi.org/10.1038/s41597-025-05315-y", "https://doi.org/10.1038/s41597-025-05315-y"),
          ". Explore the data at ",
          tags$a(href = "https://datadiversitylab.github.io/ACDB/", "datadiversitylab.github.io/ACDB"),
          " | MIT license"
        )
      )
    )
  }
  
  print_card <- function(sid) {
    sp <- species_table[species_table$species_id == sid, ][1, ]
    track("card_download", species = sid)
    session$sendCustomMessage("acdb_print", list(
      title = paste("ACDB card", sp$canonicalName),
      file = paste0("ACDB_card_", gsub("[^A-Za-z0-9]+", "_", sp$canonicalName), ".pdf"),
      html = as.character(species_card(sid))
    ))
  }
  
  output$card_preview <- renderUI({
    req(input$card_species)
    species_card(input$card_species)
  })
  
  observeEvent(input$card_random, { updateSelectizeInput(session, "card_species", selected = sample(sp_sorted$species_id, 1)) })
  observeEvent(input$card_print, { req(input$card_species); print_card(input$card_species) })
  observeEvent(input$modal_pdf, { req(modal_species()); print_card(modal_species()) })
  observeEvent(input$example_card, { print_card(species_of_group(example_id())) })
  
  
  # Citations, with the species coded from each source and what it informs
  output$citations_table <- renderDataTable({
    # Where each source is used: which species, and for what
    info_cols <- c(behavior_source = "Behavior", vertical_source = "Vertical transmission",
                   horizontal_source = "Horizontal transmission", oblique_source = "Oblique transmission",
                   domains_source = "Domains", anth_source = "Anthropogenic effects")
    info_levels <- c(unname(info_cols), "Social unit", "Group size", "Location")
    species_of_behavior <- groups_table$species_id[match(behaviors_table$group_id, groups_table$group_id)]
    uses <- rbind(
      do.call(rbind, lapply(names(info_cols), function(col) {
        data.frame(source_id = behaviors_table[[col]], species_id = species_of_behavior, info = info_cols[[col]])
      })),
      data.frame(source_id = species_table$unit_source, species_id = species_table$species_id, info = "Social unit"),
      data.frame(source_id = groups_table$size_source, species_id = groups_table$species_id, info = "Group size"),
      data.frame(source_id = groups_table$location_source, species_id = groups_table$species_id, info = "Location")
    )
    uses <- uses[uses$source_id %in% sources_table$source_id, ]
    
    species_names <- setNames(species_table$canonicalName, species_table$species_id)
    species_by_source <- tapply(uses$species_id, uses$source_id, function(x) {
      paste0("<i>", esc(sort(unique(species_names[x]))), "</i>", collapse = ", ")
    })
    info_by_source <- tapply(uses$info, uses$source_id, function(x) {
      paste0('<span class="chip chip-', slot(match(unique(x), info_levels)), '">', unique(x), "</span>", collapse = "")
    })
    
    # First author and et al. for long author lists, with the full list on hover
    short_authors <- function(a) paste0('<span title="', esc(a), '">', esc(short_author(a)), "</span>")
    
    ids <- sources_table$source_id
    citations_df <- data.frame(
      Authors = vapply(sources_table$authors, short_authors, "", USE.NAMES = FALSE),
      Year = sources_table$year,
      Title = esc(sources_table$title),
      DOI = ifelse(is.na(sources_table$doi) | sources_table$doi == "", "No DOI",
                   paste0('<a href="https://doi.org/', sources_table$doi, '" target="_blank">', sources_table$doi, '</a>')),
      Species = ifelse(is.na(as.vector(species_by_source[ids])), "", as.vector(species_by_source[ids])),
      Informs = ifelse(is.na(as.vector(info_by_source[ids])), "", as.vector(info_by_source[ids])),
      stringsAsFactors = FALSE
    )
    citations_df <- citations_df[order(-citations_df$Year), ]
    
    datatable(
      citations_df,
      rownames = FALSE,
      escape = FALSE,
      class = "hover",
      options = list(pageLength = 10, scrollX = TRUE, columnDefs = list(list(orderable = FALSE, targets = 5)))
    )
  })
  
  
  # Papers not coded yet. An empty or missing file shows an empty table
  output$papers_table <- renderDataTable({
    papers <- tryCatch(read.csv("db/uncoded_papers.csv", stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(papers) || ncol(papers) < 4) papers <- data.frame(authors = character(), year = character(), title = character(), doi = character())
    papers <- papers[, 1:4]
    names(papers) <- c("authors", "year", "title", "doi")
    papers$doi <- ifelse(is.na(papers$doi) | papers$doi == "", "No DOI",
                         paste0('<a href="https://doi.org/', papers$doi, '" target="_blank">', papers$doi, '</a>'))
    papers$title <- esc(papers$title)
    datatable(
      papers,
      colnames = c("Authors", "Year", "Title", "DOI"),
      rownames = FALSE,
      escape = FALSE,
      class = "hover",
      options = list(pageLength = 10, language = list(emptyTable = "No papers listed yet."))
    )
  })
  
}
