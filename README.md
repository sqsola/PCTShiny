# PCT 2025 Thru-Hike Dashboard

An interactive R Shiny dashboard visualizing a 2025 southbound (SOBO) thru-hike of the Pacific Crest Trail — from the Canadian border near Mazama, WA to Idyllwild, CA, over 128–129 nights on trail.

Built with Shiny + Leaflet, the app lets you scroll through six sections (Whole PCT, Washington, Oregon, Northern California, Sierra, Southern California), each with its own regional map, night-by-night slider, and daily/cumulative mileage, ascent, and descent stats.

**Live app:** _add your Posit Connect Cloud URL here_

## Features

- **Six scroll-snapping sections** — a whole-trail overview plus one section per region, each styled with its own accent color
- **Interactive regional maps** (Leaflet) with:
  - The full planned PCT route overlaid in red
  - The actual hiked route as a colored polyline
  - Clickable night markers that jump the slider to that day
  - Toggle between a light basemap (CartoDB Positron) and satellite imagery (Esri World Imagery)
- **Night-by-night navigation** via a slider or Previous/Next Day buttons, clamped to each section's date range
- **Daily stats** — miles hiked, elevation gained, elevation lost
- **Cumulative stats** — running totals of miles/ascent/descent, both for the whole trail and within each region
- **Color-coded top navigation bar** with translucent icon backgrounds for quick jumps between sections

## Data

- `data/mileage_clean.csv` — one row per night on trail: date, camp name, GPS coordinates, miles hiked, ascent, descent, and trail mile marker
- `data/mileage.xlsx` — the raw source spreadsheet
- `prepare_data.py` — regenerates `mileage_clean.csv` from `mileage.xlsx` whenever hike data is updated
- `data/maps/` — regional shapefiles (Washington, Oregon, Northern California, Sierra, Southern California) plus a full-trail shapefile, used both for map rendering and for spatially assigning each night to its region

### Region assignment

Because this was a SOBO hike, the raw `mile` column counts distance from the Canadian border (not the standard Halfmile convention measured from the Mexican border), and the first two nights use a different reference point. Rather than relying on mileage thresholds, each night's GPS coordinates are spatially joined to the nearest regional shapefile boundary (`sf::st_distance()`) to determine its region. Nights with no GPS recorded (e.g. a zero day) inherit the region of the nearest recorded night in trail order.

## Tech stack

- [Shiny](https://shiny.posit.co/) — app framework
- [leaflet](https://rstudio.github.io/leaflet/) — interactive maps
- [sf](https://r-spatial.github.io/sf/) — spatial joins for region assignment
- Tidyverse (`dplyr`, `readr`, `purrr`, `tidyr`, `stringr`, `scales`) — data wrangling and formatting throughout
- [here](https://here.r-lib.org/) — project-relative file paths
- Hosted on [Posit Connect Cloud](https://posit.cloud/)

This work is licensed under CC BY-NC-SA 4.0
