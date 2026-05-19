# ============================================
# CEN314 - Final Project
# Natural Disasters & Economic Damage
# Shiny Dashboard - global.R
# 
# PURPOSE:
# This file is executed once when the application starts. 
# It handles loading libraries, importing datasets, cleaning data, 
# making API calls to the World Bank (CPI and GDP), 
# and defining global variables like color palettes and map overlays.
# ============================================

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(DT)
library(wbstats)
library(maps)
library(countrycode)
library(scales)
library(sf)          # For reading fault line GeoJSON

# ============================================
# DATA LOADING & CLEANING
# ============================================

df <- read_csv("../data/emdat_data.csv", show_col_types = FALSE)

income <- wb_countries() %>%
  select(iso3c, income_level) %>%
  distinct(iso3c, .keep_all = TRUE)   # Prevent region duplicates

df_clean <- df %>%
  select(Year, `Disaster Type`, Country, ISO, Continent,
         `Total Deaths`, `Total Affected`,
         `Total Damages ('000 US$)`) %>%
  filter(!is.na(`Total Damages ('000 US$)`)) %>%
  rename(
    year          = Year,
    disaster_type = `Disaster Type`,
    country       = Country,
    continent     = Continent,
    total_deaths  = `Total Deaths`,
    total_affected = `Total Affected`,
    total_damages = `Total Damages ('000 US$)`
  ) %>%
  left_join(income, by = c("ISO" = "iso3c")) %>%
  filter(!is.na(income_level), income_level != "Not classified") %>%
  filter(disaster_type %in% c("Storm", "Flood", "Earthquake",
                               "Drought", "Wildfire", "Landslide"))

# ============================================
# CPI INFLATION ADJUSTMENT (World Bank, base year = 2021)
# Nominal USD → Constant 2021 USD
# Covers 1960–2021; pre-1960 records keep nominal value
# ============================================

cpi_raw <- wb_data("FP.CPI.TOTL", country = "US",
                   start_date = 1960, end_date = 2021) %>%
  select(year = date, cpi = FP.CPI.TOTL) %>%
  filter(!is.na(cpi))

cpi_2021   <- cpi_raw %>% filter(year == 2021) %>% pull(cpi)
cpi_index  <- cpi_raw %>%
  mutate(cpi_factor = cpi_2021 / cpi) %>%
  select(year, cpi_factor)

df_clean <- df_clean %>%
  left_join(cpi_index, by = "year") %>%
  mutate(
    real_damages = if_else(!is.na(cpi_factor),
                           total_damages * cpi_factor,
                           total_damages)
  )

# ============================================
# GDP NORMALIZATION (World Bank, NY.GDP.MKTP.CD)
# damage_pct_gdp = damage as % of GDP — strips infrastructure cost bias
# total_damages is in 000' USD; GDP is in USD → multiply damages * 1000 first
# ============================================

gdp_raw <- wb_data("NY.GDP.MKTP.CD", country = "countries_only",
                   start_date = 1960, end_date = 2021) %>%
  select(ISO = iso3c, year = date, gdp = NY.GDP.MKTP.CD) %>%
  filter(!is.na(gdp)) %>%
  distinct(ISO, year, .keep_all = TRUE)  # Prevent possible API duplicates

df_clean <- df_clean %>%
  left_join(gdp_raw, by = c("ISO", "year")) %>%
  mutate(
    damage_pct_gdp = if_else(
      !is.na(gdp) & gdp > 0,
      (total_damages * 1000) / gdp * 100,
      NA_real_
    )
  ) %>%
  # Remove possible duplicate rows after all joins
  distinct(year, disaster_type, country, ISO, total_damages,
           total_deaths, total_affected, .keep_all = TRUE)

# ============================================
# FAULT LINE DATA (Tectonic plate boundaries)
# Source: fraxen/tectonicplates (GitHub)
# Loaded once at app startup
# ============================================

fault_lines <- tryCatch(
  sf::st_read(
    "https://raw.githubusercontent.com/fraxen/tectonicplates/master/GeoJSON/PB2002_boundaries.json",
    quiet = TRUE
  ),
  error = function(e) {
    message("Failed to load fault line data: ", e$message)
    NULL
  }
)

# ============================================
# COUNTRY COORDINATES (for world map pins)
# Using capital city coordinates via maps package
# ============================================

country_coords <- maps::world.cities %>%
  filter(capital == 1) %>%
  select(country.etc, lat, long) %>%
  mutate(ISO = countrycode(country.etc, "country.name", "iso3c",
                           warn = FALSE)) %>%
  filter(!is.na(ISO)) %>%
  group_by(ISO) %>%
  slice(1) %>%
  ungroup() %>%
  select(ISO, lat, long)

# ============================================
# COLOR PALETTE — disaster type → hex color
# Both marker and legend use the same hex code → color mismatch impossible
# ============================================

disaster_legend_colors <- c(
  "Storm"      = "#1a3a5c",
  "Flood"      = "#5bc0de",
  "Earthquake" = "#d9534f",
  "Drought"    = "#f0ad4e",
  "Wildfire"   = "#8B0000",
  "Landslide"  = "#5cb85c"
)

# Disaster type → hex color (same source as legend)
disaster_to_hexcolor <- function(dtype) {
  colors <- disaster_legend_colors
  ifelse(dtype %in% names(colors), colors[dtype], "#888888")
}

# ============================================
# TROPICAL CYCLONE BASINS (Storm Zone Overlay)
# Source: WMO / NOAA — 6 main tropical cyclone formation regions
# Shown as an informative layer on the map
# ============================================

storm_basins <- list(
  list(
    name   = "North Atlantic — Hurricane Belt",
    lat1   = 8,  lat2 = 36, lng1 = -100, lng2 = -15,
    color  = "#1a3a5c",
    season = "June – November",
    info   = "Warm Atlantic, Gulf of Mexico & Caribbean waters fuel Category 1–5 hurricanes."
  ),
  list(
    name   = "Western North Pacific — Typhoon Alley",
    lat1   = 5,  lat2 = 40, lng1 = 110,  lng2 = 180,
    color  = "#2171b5",
    season = "Year-round (peak: Jul – Oct)",
    info   = "World's most active cyclone basin. Typhoons regularly strike the Philippines, Japan & China."
  ),
  list(
    name   = "Eastern North Pacific",
    lat1   = 5,  lat2 = 22, lng1 = -180, lng2 = -90,
    color  = "#4292c6",
    season = "May – November",
    info   = "High storm frequency but most dissipate over open ocean before landfall."
  ),
  list(
    name   = "North Indian Ocean — Bay of Bengal & Arabian Sea",
    lat1   = 5,  lat2 = 25, lng1 = 45,   lng2 = 100,
    color  = "#08519c",
    season = "April – June  &  October – December",
    info   = "Bay of Bengal cyclones are among the deadliest — striking Bangladesh, India & Myanmar."
  ),
  list(
    name   = "South Indian Ocean",
    lat1   = -40, lat2 = -5, lng1 = 20,  lng2 = 135,
    color  = "#6baed6",
    season = "November – April (Southern Hemisphere)",
    info   = "Affects Madagascar, Mozambique & western Australia. Season mirrors Northern Hemisphere."
  ),
  list(
    name   = "Southwest Pacific — Australian Region",
    lat1   = -30, lat2 = -5, lng1 = 135, lng2 = 175,
    color  = "#9ecae1",
    season = "November – April (Southern Hemisphere)",
    info   = "Cyclones frequently affect Queensland (Australia), Fiji, Vanuatu & New Caledonia."
  ),
  list(
    name   = "European Windstorm Zone (Extratropical)",
    lat1   = 40,  lat2 = 70, lng1 = -20, lng2 = 30,
    color  = "#1a3a5c",
    season = "October – March (Winter)",
    info   = "Not tropical cyclones, but powerful winter storms driven by the North Atlantic jet stream."
  )
)

# ============================================
# PLOTLY THEME — consistent, professional styling
# ============================================

plotly_theme <- function(p) {
  p %>%
    layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font = list(family = "Inter, Segoe UI, sans-serif", size = 12, color = "#334155"),
      margin = list(l = 10, r = 10, t = 40, b = 10, pad = 4),
      legend = list(bgcolor = "rgba(255,255,255,0.9)",
                    bordercolor = "#e2e8f0", borderwidth = 1,
                    font = list(size = 11)),
      xaxis = list(gridcolor = "#f1f5f9", zerolinecolor = "#e2e8f0",
                   tickfont = list(size = 11),
                   titlefont = list(size = 12, color = "#475569")),
      yaxis = list(gridcolor = "#f1f5f9", zerolinecolor = "#e2e8f0",
                   tickfont = list(size = 11),
                   titlefont = list(size = 12, color = "#475569"))
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                                 "hoverCompareCartesian", "toggleSpikelines"),
      toImageButtonOptions = list(format = "png", width = 1200, height = 700)
    )
}
