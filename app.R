########################################################
## PCT 2025 Thru-Hike Dashboard
########################################################

library(shiny)
library(leaflet)
library(scales)
library(here)
library(rio)
library(sf)
library(tidyverse)

# ---- Data ---------------------------------------------------------------

hike <- read.csv(here("data", "mileage_clean.csv"), stringsAsFactors = FALSE) %>%
        mutate(date = mdy(date)) %>%
        arrange(night)

map_washington <- readRDS(here("data", "maps", "Washington.shp", "washington.rds"))
map_oregon     <- readRDS(here("data", "maps", "Oregon.shp", "oregon.rds"))
map_norcal     <- readRDS(here("data", "maps", "Northern_California.shp", "norcal.rds"))
map_sierra     <- readRDS(here("data", "maps", "Central_California.shp", "sierra.rds"))
map_socal      <- readRDS(here("data", "maps", "Southern_California.shp", "socal.rds"))
map_full       <- readRDS(here("data", "maps", "full_pct", "full_pct.rds"))

# Look up the trail overlay for a given section. The "whole" section
# (region == NA) gets the full PCT; each named region gets just its own
# state/segment shapefile instead of the entire trail.
map_for_region <- function(region) {
  if (is.na(region)) {
    map_full
  } else {
    switch(region,
      "Washington"           = map_washington,
      "Oregon"               = map_oregon,
      "Northern California"  = map_norcal,
      "Sierra"               = map_sierra,
      "Southern California"  = map_socal
    )
  }
}



# ---- Region assignment ---------------------------------------------------
#
# Each night's region is determined by which state/segment shapefile its
# GPS point falls closest to, rather than by the raw `mile` column. The
# `mile` column turns out not to be a single consistent "distance from the
# Mexican border" measurement across the whole trip -- for most of the hike
# it counts up as the hiker moves from Washington down to Southern
# California, but the first two nights near Mazama use a different
# reference entirely. Thresholding on `mile` directly caused nights to be
# sorted into the wrong region (e.g. Woody Pass, WA landing in "Southern
# California"). Snapping each point to its nearest state shapefile sidesteps
# that inconsistency and keeps region assignment tied to real geography.
#
# Nights with no recorded GPS (e.g. some zero/resupply days) inherit the
# region of the nearest night in the data.

region_levels <- c("Washington", "Oregon", "Northern California", "Sierra", "Southern California")

region_shapes <- list(
  "Washington"          = map_washington,
  "Oregon"              = map_oregon,
  "Northern California" = map_norcal,
  "Sierra"              = map_sierra,
  "Southern California" = map_socal
)

region_union <- purrr::map(region_shapes, st_union)

nearest_region <- function(lon, lat) {
  if (is.na(lon) || is.na(lat)) return(NA_character_)
  pt    <- st_sfc(st_point(c(lon, lat)), crs = st_crs(map_full))
  dists <- purrr::map_dbl(region_union, ~ as.numeric(st_distance(pt, .x)))
  names(region_union)[which.min(dists)]
}

hike <- hike %>%
  mutate(region = purrr::map2_chr(lon, lat, nearest_region)) %>%
  fill(region, .direction = "downup") %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  group_by(region) %>%
  mutate(
    region_cum_miles   = cumsum(replace_na(miles_completed, 0)),
    region_cum_ascent  = cumsum(replace_na(ascent, 0)),
    region_cum_descent = cumsum(replace_na(descent, 0))
  ) %>%
  ungroup()

total_miles   <- max(hike$cum_miles)
total_ascent  <- max(hike$cum_ascent)
total_descent <- max(hike$cum_descent)
max_night     <- max(hike$night)
min_night     <- min(hike$night)

region_summary <- hike %>%
  group_by(region) %>%
  summarise(
    nights  = n(),
    miles   = sum(miles_completed, na.rm = TRUE),
    ascent  = sum(ascent, na.rm = TRUE),
    descent = sum(descent, na.rm = TRUE),
    .groups = "drop"
  )

region_stats <- function(r) region_summary %>% filter(region == r)

# ---- Forest color palette ------------------------------------------------

forest_dark    <- "#1b3a2b"
forest_mid     <- "#2f6b4f"
forest_light   <- "#7fb693"
lake_blue      <- "#2b6777"
sky_blue       <- "#a8dadc"
bark_brown     <- "#4a3728"
panel_bg       <- "#f4f7f3"
card_bg        <- "#ffffff"
text_dark      <- "#1e2b23"

# ---- Reusable section module ---------------------------------------------

day_stat_card <- function(ns, output_id, label, sub_label = NULL, accent) {
  div(class = "stat-card",
      style = sprintf("border-left-color: %s;", accent),
      h4(style = sprintf("color: %s;", accent), label),
      div(class = "stat-value", textOutput(ns(output_id), inline = TRUE)),
      if (!is.null(sub_label)) div(class = "stat-sub", sub_label)
  )
}

sectionUI <- function(id, title, subtitle, cum_label, header_bg, header_text, blurb_p1, blurb_p2) {
  ns <- NS(id)

  div(
    class = "section-panel",
    id = paste0("section-", id),

    div(class = "app-header",
        style = sprintf("background: %s; color: %s;", header_bg, header_text),
        h1(title),
        p(style = sprintf("color: %s;", header_text), subtitle)
    ),

    topNavUI(id),

    fluidRow(
      column(
        width = 7,
        div(style = "background-color: white; border-radius: 10px; padding: 8px; box-shadow: 0 1px 6px rgba(0,0,0,0.1);",
            leafletOutput(ns("map"), height = "560px")
        )
      ),
      column(
        width = 5,
        div(class = "notes-box",
            style = sprintf("border-left-color: %s;", header_bg),
            p(blurb_p1),
            p(blurb_p2)
        ),
        wellPanel(
          uiOutput(ns("slider")),
          fluidRow(
            column(6,
                   actionButton(ns("prevDay"), "\u25c0 Previous Day",
                                width = "100%",
                                style = sprintf("background-color: %s; color: %s; border: none;", header_bg, header_text))
            ),
            column(6,
                   actionButton(ns("nextDay"), "Next Day \u25b6",
                                width = "100%",
                                style = sprintf("background-color: %s; color: %s; border: none;", header_bg, header_text))
            )
          )
        ),
        uiOutput(ns("nightHeader")),
        fluidRow(
          column(4, day_stat_card(ns, "dayMiles", "Miles that day", accent = header_bg)),
          column(4, day_stat_card(ns, "dayAscent", "Ascent", "feet gained", accent = header_bg)),
          column(4, day_stat_card(ns, "dayDescent", "Descent", "feet lost", accent = header_bg))
        ),
        div(class = "cumulative-box",
            h4(cum_label),
            fluidRow(
              column(4,
                     div(style = "font-size: 22px; font-weight: 700;", textOutput(ns("cumMiles"), inline = TRUE)),
                     div(style = "font-size: 12px; opacity: 0.85;", "miles hiked")
              ),
              column(4,
                     div(style = "font-size: 22px; font-weight: 700;", textOutput(ns("cumAscent"), inline = TRUE)),
                     div(style = "font-size: 12px; opacity: 0.85;", "ft ascent")
              ),
              column(4,
                     div(style = "font-size: 22px; font-weight: 700;", textOutput(ns("cumDescent"), inline = TRUE)),
                     div(style = "font-size: 12px; opacity: 0.85;", "ft descent")
              )
            )
        )
      )
    )
  )
}

sectionServer <- function(id, data, full_trail, cum_cols) {
  moduleServer(id, function(input, output, session) {

    data_geo   <- data %>% filter(!is.na(lat), !is.na(lon))
    min_n      <- min(data$night)
    max_n      <- max(data$night)

    output$slider <- renderUI({
      sliderInput(
        session$ns("night"),
        "Select a night on trail:",
        min = min_n,
        max = max_n,
        value = min_n,
        step = 1,
        animate = animationOptions(interval = 600, loop = FALSE),
        width = "100%"
      )
    })

    selectedRow <- reactive({
      req(input$night)
      data %>% filter(night == input$night)
    })

    observeEvent(input$prevDay, {
      req(input$night)
      updateSliderInput(session, "night", value = max(min_n, input$night - 1))
    }, ignoreInit = TRUE)

    observeEvent(input$nextDay, {
      req(input$night)
      updateSliderInput(session, "night", value = min(max_n, input$night + 1))
    }, ignoreInit = TRUE)

    output$nightHeader <- renderUI({
      row <- selectedRow()
      gps_txt <- if (!is.na(row$lat) && !is.na(row$lon)) {
        sprintf("%.5f, %.5f", row$lat, row$lon)
      } else {
        "No GPS recorded for this night"
      }
      tagList(
        div(class = "location-name", sprintf("Night %d \u2014 %s", row$night, row$name)),
        div(class = "location-date", sprintf("%s  \u2022  %s", format(row$date, "%B %d, %Y"), gps_txt))
      )
    })

    output$dayMiles   <- renderText({ sprintf("%.1f mi", selectedRow()$miles_completed) })
    output$dayAscent  <- renderText({ comma(selectedRow()$ascent) })
    output$dayDescent <- renderText({ comma(selectedRow()$descent) })

    output$cumMiles   <- renderText({ comma(selectedRow()[[cum_cols$miles]]) })
    output$cumAscent  <- renderText({ comma(selectedRow()[[cum_cols$ascent]]) })
    output$cumDescent <- renderText({ comma(selectedRow()[[cum_cols$descent]]) })

    output$map <- renderLeaflet({
      leaflet(data_geo) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addPolylines(data = full_trail, color = "red", weight = 2, opacity = 0.85) %>%
        addPolylines(lng = ~lon, lat = ~lat, color = forest_mid, weight = 3, opacity = 0.75) %>%
        addCircleMarkers(
          lng = ~lon, lat = ~lat,
          radius = 4,
          stroke = FALSE,
          fillColor = lake_blue,
          fillOpacity = 0.6,
          layerId = ~as.character(night),
          popup = ~sprintf("<b>Night %d</b><br>%s<br>%s", night, name, format(date, "%b %d, %Y"))
        ) %>%
        addLayersControl(
          baseGroups = c("Map", "Satellite"),
          options = layersControlOptions(collapsed = FALSE),
          position = "topright"
        ) %>%
        fitBounds(
          lng1 = min(data_geo$lon), lat1 = min(data_geo$lat),
          lng2 = max(data_geo$lon), lat2 = max(data_geo$lat)
        )
    })

    observeEvent(input$night, {
      row <- selectedRow()
      proxy <- leafletProxy("map")
      proxy %>% clearGroup("selected")

      if (!is.na(row$lat) && !is.na(row$lon)) {
        proxy %>%
          addCircleMarkers(
            lng = row$lon, lat = row$lat,
            radius = 11,
            stroke = TRUE,
            color = forest_dark,
            weight = 3,
            fillColor = "#e07a3f",
            fillOpacity = 0.95,
            group = "selected",
            popup = sprintf("<b>Night %d</b><br>%s<br>%s", row$night, row$name, format(row$date, "%b %d, %Y"))
          ) %>%
          setView(lng = row$lon, lat = row$lat, zoom = 9)
      }
    })

    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click$id) && !is.na(suppressWarnings(as.numeric(click$id)))) {
        updateSliderInput(session, "night", value = as.numeric(click$id))
      }
    })
  })
}

# ---- Section definitions --------------------------------------------------

lorem_p1 <- paste(
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod",
  "tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim",
  "veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea",
  "commodo consequat."
)

lorem_p2 <- paste(
  "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum",
  "dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non",
  "proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
)

sections <- tibble::tribble(
  ~id,          ~title,                                    ~region,                ~nav_bg,        ~nav_text,     ~nav_icon,   ~blurb_p1,  ~blurb_p2,
  "whole",      "Pacific Crest Trail \u2014 2025 Thru-Hike", NA_character_,          forest_mid,     "#f4f7f3",     "route",     lorem_p1,   lorem_p2,
  "washington", "Washington",                                "Washington",           forest_light,   "#1b3a2b",     "mountain",  lorem_p1,   lorem_p2,
  "oregon",     "Oregon",                                    "Oregon",               "#2a211d",      "#f4f7f3",     "tree",      lorem_p1,   lorem_p2,
  "norcal",     "Northern California",                       "Northern California",  "#c1502e",      "#f4f7f3",     "water",     lorem_p1,   lorem_p2,
  "sierra",     "Sierra",                                    "Sierra",               "#dbeef4",      "#1b3a2b",     "snowflake", lorem_p1,   lorem_p2,
  "socal",      "Southern California",                       "Southern California",  bark_brown,     "#f4f7f3",     "sun",       lorem_p1,   lorem_p2
)

topNavUI <- function(active_id) {
  div(class = "top-nav",
      purrr::pmap(sections, function(id, title, region, nav_bg, nav_text, nav_icon, ...) {
        tags$a(
          href = paste0("#section-", id),
          class = paste("nav-box", if (id == active_id) "active" else ""),
          `data-target` = id,
          style = sprintf("background-color: %s; color: %s;", nav_bg, nav_text),
          icon(nav_icon, lib = "font-awesome", class = "nav-box-icon",
               style = sprintf("color: %s;", nav_text)),
          tags$span(class = "nav-box-label", title)
        )
      })
  )
}

subtitle_for <- function(region) {
  if (is.na(region)) {
    sprintf("%s total nights on trail \u2022 %.1f miles \u2022 %s ft ascent \u2022 %s ft descent",
            max_night, total_miles, comma(total_ascent), comma(total_descent))
  } else {
    s <- region_stats(region)
    sprintf("%d nights on trail \u2022 %.1f miles \u2022 %s ft ascent \u2022 %s ft descent",
            s$nights, s$miles, comma(s$ascent), comma(s$descent))
  }
}

cum_label_for <- function(region) {
  if (is.na(region)) "Cumulative through this night" else sprintf("Cumulative in %s through this night", region)
}

data_for <- function(region) {
  if (is.na(region)) hike else hike %>% filter(region == !!region)
}

cum_cols_for <- function(region) {
  if (is.na(region)) {
    list(miles = "cum_miles", ascent = "cum_ascent", descent = "cum_descent")
  } else {
    list(miles = "region_cum_miles", ascent = "region_cum_ascent", descent = "region_cum_descent")
  }
}

# ---- UI -------------------------------------------------------------------

ui <- fluidPage(
  title = "PCT 2025 Thru-Hike",

  tags$head(
    tags$style(HTML(sprintf("
      html, body {
        height: 100%%;
        margin: 0;
        padding: 0;
        overflow: hidden;
        background-color: %s;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: %s;
      }
      .container-fluid {
        padding: 0 !important;
        margin: 0 !important;
        max-width: 100%%;
      }
      .scroll-container {
        height: 100vh;
        overflow-y: scroll;
        scroll-snap-type: y mandatory;
      }
      .section-panel {
        height: 100vh;
        scroll-snap-align: start;
        overflow-y: auto;
        padding: 0 30px 20px 30px;
        box-sizing: border-box;
      }
      .app-header {
        background: linear-gradient(120deg, %s 0%%, %s 60%%, %s 100%%);
        color: #f4f7f3;
        padding: 12px 30px;
        margin: 0 -30px 10px -30px;
        border-radius: 0 0 12px 12px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.15);
      }
      .app-header h1 {
        margin: 0;
        font-size: 22px;
        font-weight: 700;
        letter-spacing: 0.5px;
      }
      .app-header p {
        margin: 2px 0 0 0;
        font-size: 13px;
        color: %s;
      }
      .stat-card {
        background-color: %s;
        border-left: 5px solid %s;
        border-radius: 8px;
        padding: 14px 16px;
        margin-bottom: 12px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      }
      .stat-card h4 {
        margin: 0 0 6px 0;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: %s;
        font-weight: 700;
      }
      .stat-card .stat-value {
        font-size: 22px;
        font-weight: 700;
        color: %s;
      }
      .stat-card .stat-sub {
        font-size: 12px;
        color: #6b8577;
        margin-top: 2px;
      }
      .cumulative-box {
        background-color: %s;
        color: #f4f7f3;
        border-radius: 8px;
        padding: 16px;
        margin-top: 10px;
      }
      .cumulative-box h4 {
        margin: 0 0 10px 0;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: %s;
      }
      .location-name {
        font-size: 18px;
        font-weight: 700;
        color: %s;
      }
      .location-date {
        font-size: 13px;
        color: #6b8577;
        margin-bottom: 6px;
      }
      .notes-box {
        background-color: %s;
        border-radius: 8px;
        padding: 12px 14px;
        font-size: 13px;
        color: #3c4a41;
        margin-bottom: 14px;
        border-left: 4px solid %s;
      }
      .notes-box p {
        margin: 0 0 8px 0;
      }
      .notes-box p:last-child {
        margin-bottom: 0;
      }
      .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to {
        background: %s !important;
        border-color: %s !important;
      }
      .well {
        background-color: %s;
        border: none;
        margin-bottom: 8px;
        padding-bottom: 8px;
      }
      .top-nav {
        display: flex;
        gap: 10px;
        margin: 0 0 18px 0;
        width: 100%%;
      }
      .nav-box {
        flex: 1 1 0;
        min-width: 0;
        height: 64px;
        box-sizing: border-box;
        border-radius: 10px;
        border: 2px solid rgba(0,0,0,0.15);
        position: relative;
        overflow: hidden;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        font-size: 12px;
        font-weight: 700;
        line-height: 1.25;
        padding: 6px;
        cursor: pointer;
        text-decoration: none;
        transition: transform 0.15s ease, filter 0.15s ease, box-shadow 0.15s ease;
      }
      .nav-box-icon {
        position: absolute;
        top: 50%%;
        left: 50%%;
        transform: translate(-50%%, -50%%);
        font-size: 34px;
        opacity: 0.28;
        pointer-events: none;
        z-index: 0;
      }
      .nav-box-label {
        position: relative;
        z-index: 1;
      }
      .nav-box:hover {
        transform: scale(1.03);
        filter: brightness(1.08);
      }
      .nav-box.active {
        border-color: #f4f7f3;
        box-shadow: 0 0 0 3px rgba(244,247,243,0.55);
      }
    ",
                            panel_bg, text_dark,
                            forest_dark, forest_mid, lake_blue,
                            sky_blue,
                            card_bg, forest_mid,
                            forest_mid,
                            forest_dark,
                            lake_blue,
                            sky_blue,
                            forest_dark,
                            "#e8f0ea",
                            forest_light,
                            forest_mid, forest_mid,
                            card_bg
    )))
  ),

  # ---- Scrolling sections ----
  div(class = "scroll-container",
      purrr::pmap(sections, function(id, title, region, nav_bg, nav_text, blurb_p1, blurb_p2, ...) {
        sectionUI(
          id          = id,
          title       = title,
          subtitle    = subtitle_for(region),
          cum_label   = cum_label_for(region),
          header_bg   = nav_bg,
          header_text = nav_text,
          blurb_p1    = blurb_p1,
          blurb_p2    = blurb_p2
        )
      })
  ),

  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      var sections = document.querySelectorAll('.section-panel');
      var navBoxes = document.querySelectorAll('.nav-box');

      var observer = new IntersectionObserver(function(entries) {
        entries.forEach(function(entry) {
          if (entry.isIntersecting) {
            var id = entry.target.id.replace('section-', '');
            navBoxes.forEach(function(b) { b.classList.remove('active'); });
            document.querySelectorAll('.nav-box[data-target=\"' + id + '\"]').forEach(function(b) {
              b.classList.add('active');
            });
            window.dispatchEvent(new Event('resize'));
          }
        });
      }, { root: document.querySelector('.scroll-container'), threshold: 0.5 });

      sections.forEach(function(s) { observer.observe(s); });

      navBoxes.forEach(function(b) {
        b.addEventListener('click', function(e) {
          e.preventDefault();
          var target = document.getElementById('section-' + b.getAttribute('data-target'));
          if (target) target.scrollIntoView({ behavior: 'smooth' });
        });
      });
    });
  "))
)

# ---- Server -----------------------------------------------------------

server <- function(input, output, session) {
  purrr::pwalk(sections, function(id, title, region, ...) {
    sectionServer(
      id         = id,
      data       = data_for(region),
      full_trail = map_for_region(region),
      cum_cols   = cum_cols_for(region)
    )
  })
}

shinyApp(ui, server)
