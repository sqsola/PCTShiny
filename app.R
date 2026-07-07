########################################################
## PCT 2025 Thru-Hike Dashboard
########################################################

library(shiny)
library(leaflet)
library(scales)
library(here)
library(sf)
library(tidyverse)

# ---- Data ---------------------------------------------------------------

hike <- read_csv(here("data", "mileage_clean.csv"), show_col_types = FALSE) %>%
  mutate(date = mdy(date)) %>%
  arrange(night)

map_washington <- readRDS(here("data", "maps", "Washington.shp", "washington.rds"))
map_oregon     <- readRDS(here("data", "maps", "Oregon.shp", "oregon.rds"))
map_norcal     <- readRDS(here("data", "maps", "Northern_California.shp", "norcal.rds"))
map_sierra     <- readRDS(here("data", "maps", "Central_California.shp", "sierra.rds"))
map_socal      <- readRDS(here("data", "maps", "Southern_California.shp", "socal.rds"))
map_full       <- readRDS(here("data", "maps", "full_pct", "full_pct.rds"))


# ---- Region assignment ---------------------------------------------------
#
# Each night's camp is assigned to whichever regional shapefile its GPS
# coordinates fall closest to, using a spatial nearest-shapefile join
# (st_distance) rather than mileage thresholds. This hike was SOBO, and the
# `mile` column counts distance traveled from the Canadian border -- it is
# not the standard Halfmile mile-marker convention (measured from the
# Mexican border), so mile-threshold logic silently misassigns nights.
# Spatial distance to the actual regional boundary polygons sidesteps that
# mismatch entirely.
#
# Nights with no GPS recorded (e.g. the Ashland zero-day) inherit the
# region of the nearest night on trail, before and after, in trail order.

region_levels <- c("Washington", "Oregon", "Northern California", "Sierra", "Southern California")

region_shapes <- list(
  "Washington"           = map_washington,
  "Oregon"               = map_oregon,
  "Northern California"  = map_norcal,
  "Sierra"               = map_sierra,
  "Southern California"  = map_socal
)

hike_pts <- hike %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(st_crs(map_washington))

region_distances <- region_shapes %>%
  map(~ apply(st_distance(hike_pts, .x), 1, min)) %>%
  as_tibble()

hike_pts <- hike_pts %>%
  mutate(region = region_distances %>% pmap_chr(~ names(region_distances)[which.min(c(...))]))

hike <- hike %>%
  left_join(hike_pts %>% st_drop_geometry() %>% select(night, region), by = "night") %>%
  arrange(night) %>%
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

sectionUI <- function(id, title, subtitle, cum_label) {
  ns <- NS(id)

  div(
    class = "section-panel",
    id = paste0("section-", id),

    div(class = "app-header",
        h1(title),
        p(subtitle)
    ),

    topNavUI(id),

    fluidRow(
      class = "content-row",
      column(
        width = 7,
        class = "map-col",
        div(class = "map-frame",
            leafletOutput(ns("map"), height = "100%")
        )
      ),
      column(
        width = 5,
        class = "stats-col",
        wellPanel(
          uiOutput(ns("slider"))
        ),
        uiOutput(ns("nightHeader")),
        fluidRow(
          class = "stat-row",
          column(4, div(class = "stat-card",
                        h4("Miles that day"),
                        div(class = "stat-value", textOutput(ns("dayMiles"), inline = TRUE)))),
          column(4, div(class = "stat-card",
                        h4("Ascent"),
                        div(class = "stat-value", textOutput(ns("dayAscent"), inline = TRUE)),
                        div(class = "stat-sub", "feet gained"))),
          column(4, div(class = "stat-card",
                        h4("Descent"),
                        div(class = "stat-value", textOutput(ns("dayDescent"), inline = TRUE)),
                        div(class = "stat-sub", "feet lost")))
        ),
        div(class = "cumulative-box",
            h4(cum_label),
            fluidRow(
              column(4,
                     div(style = "font-size: 24px; font-weight: 700;", textOutput(ns("cumMiles"), inline = TRUE)),
                     div(style = "font-size: 12px; opacity: 0.85;", "miles hiked")
              ),
              column(4,
                     div(style = "font-size: 24px; font-weight: 700;", textOutput(ns("cumAscent"), inline = TRUE)),
                     div(style = "font-size: 12px; opacity: 0.85;", "ft ascent")
              ),
              column(4,
                     div(style = "font-size: 24px; font-weight: 700;", textOutput(ns("cumDescent"), inline = TRUE)),
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
    trail_bbox <- full_trail %>% st_transform(4326) %>% st_bbox()

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
          lng1 = trail_bbox[["xmin"]], lat1 = trail_bbox[["ymin"]],
          lng2 = trail_bbox[["xmax"]], lat2 = trail_bbox[["ymax"]]
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

sections <- tibble::tribble(
  ~id,          ~title,                                    ~region,                ~nav_bg,        ~nav_text,      ~nav_icon,
  "whole",      "Pacific Crest Trail \u2014 2025 Thru-Hike", NA_character_,          forest_mid,     "#f4f7f3",      "route",
  "washington", "Washington",                                "Washington",           forest_light,   "#1b3a2b",      "mountain",
  "oregon",     "Oregon",                                    "Oregon",               "#2a211d",      "#f4f7f3",      "tree",
  "norcal",     "Northern California",                       "Northern California",  "#c1502e",      "#f4f7f3",      "water",
  "sierra",     "Sierra",                                    "Sierra",               "#dbeef4",      "#1b3a2b",      "snowflake",
  "socal",      "Southern California",                       "Southern California",  bark_brown,     "#f4f7f3",      "sun"
)

topNavUI <- function(active_id) {
  div(class = "top-nav",
      purrr::pmap(sections, function(id, title, region, nav_bg, nav_text, nav_icon, ...) {
        tags$a(
          href = paste0("#section-", id),
          class = paste("nav-box", if (id == active_id) "active" else ""),
          `data-target` = id,
          style = sprintf("background-color: %s; color: %s;", nav_bg, nav_text),
          div(class = "nav-box-icon", icon(nav_icon)),
          span(class = "nav-box-label", title)
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

region_maps <- list(
  "Washington"           = map_washington,
  "Oregon"               = map_oregon,
  "Northern California"  = map_norcal,
  "Sierra"               = map_sierra,
  "Southern California"  = map_socal
)

map_for_region <- function(region) {
  if (is.na(region)) map_full else pluck(region_maps, region, .default = map_full)
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
      html, body {
        height: 100%%;
      }
      .section-panel {
        height: 100vh;
        scroll-snap-align: start;
        overflow-y: auto;
        padding: 16px 48px 48px 48px;
        box-sizing: border-box;
        display: flex;
        flex-direction: column;
      }
      .app-header {
        flex: 0 0 auto;
        background: linear-gradient(120deg, %s 0%%, %s 60%%, %s 100%%);
        color: #f4f7f3;
        padding: 26px 48px;
        margin: 0 -48px 28px -48px;
        border-radius: 0 0 14px 14px;
        box-shadow: 0 3px 10px rgba(0,0,0,0.15);
      }
      .app-header h1 {
        margin: 0;
        font-size: 30px;
        font-weight: 700;
        letter-spacing: 0.5px;
      }
      .app-header p {
        margin: 6px 0 0 0;
        font-size: 14px;
        color: %s;
      }
      .content-row {
        flex: 1 1 auto;
        min-height: 0;
        display: flex !important;
        margin-left: -14px;
        margin-right: -14px;
      }
      .map-col {
        display: flex;
        flex-direction: column;
        min-height: 0;
        padding-left: 14px;
        padding-right: 22px;
      }
      .map-frame {
        flex: 1 1 auto;
        min-height: 0;
        background-color: white;
        border-radius: 12px;
        padding: 16px;
        box-shadow: 0 1px 8px rgba(0,0,0,0.1);
      }
      .stats-col {
        display: flex;
        flex-direction: column;
        justify-content: space-evenly;
        min-height: 0;
        overflow-y: auto;
        padding-left: 22px;
        padding-right: 14px;
      }
      .stats-col .well {
        margin-bottom: 0;
      }
      .stat-row {
        margin-left: -8px;
        margin-right: -8px;
        margin-bottom: 4px;
      }
      .stat-row > div {
        padding-left: 8px;
        padding-right: 8px;
      }
      .stat-card {
        background-color: %s;
        border-left: 5px solid %s;
        border-radius: 10px;
        padding: 18px 18px;
        margin-bottom: 18px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.08);
      }
      .stat-card h4 {
        margin: 0 0 8px 0;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: %s;
        font-weight: 700;
      }
      .stat-card .stat-value {
        font-size: 24px;
        font-weight: 700;
        color: %s;
      }
      .stat-card .stat-sub {
        font-size: 12px;
        color: #6b8577;
        margin-top: 3px;
      }
      .cumulative-box {
        background-color: %s;
        color: #f4f7f3;
        border-radius: 10px;
        padding: 22px 20px;
      }
      .cumulative-box h4 {
        margin: 0 0 14px 0;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: %s;
      }
      .location-name {
        font-size: 19px;
        font-weight: 700;
        color: %s;
        margin-top: 4px;
      }
      .location-date {
        font-size: 13px;
        color: #6b8577;
        margin-bottom: 14px;
      }
      .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to {
        background: %s !important;
        border-color: %s !important;
      }
      .well {
        background-color: %s;
        border: none;
        padding: 20px;
      }
      .top-nav {
        display: flex;
        gap: 14px;
        margin: 0 0 26px 0;
        width: 100%%;
      }
      .nav-box {
        position: relative;
        overflow: hidden;
        flex: 1 1 0;
        min-width: 0;
        height: 64px;
        box-sizing: border-box;
        border-radius: 10px;
        border: 2px solid rgba(0,0,0,0.15);
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
        inset: 0;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 34px;
        opacity: 0.28;
        pointer-events: none;
      }
      .nav-box-icon svg {
        width: 1em;
        height: 1em;
      }
      .nav-box-label {
        position: relative;
        z-index: 2;
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
                            forest_mid, forest_mid,
                            card_bg
    )))
  ),

  # ---- Scrolling sections ----
  div(class = "scroll-container",
      purrr::pmap(sections, function(id, title, region, ...) {
        sectionUI(
          id        = id,
          title     = title,
          subtitle  = subtitle_for(region),
          cum_label = cum_label_for(region)
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
