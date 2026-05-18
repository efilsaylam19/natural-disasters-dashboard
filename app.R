# ============================================
# CEN314 - Final Project
# Natural Disasters & Economic Damage
# Shiny Dashboard - app.R
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
library(sf)          # fay hattı GeoJSON okuma için

# ============================================
# DATA LOADING & CLEANING
# ============================================



df <- read_csv("1900_2021_DISASTERS.xlsx - emdat data.csv")

income <- wb_countries() %>%
  select(iso3c, income_level)

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

gdp_raw <- wb_data("NY.GDP.MKTP.CD", country = "all",
                   start_date = 1960, end_date = 2021) %>%
  select(ISO = iso3c, year = date, gdp = NY.GDP.MKTP.CD) %>%
  filter(!is.na(gdp))

df_clean <- df_clean %>%
  left_join(gdp_raw, by = c("ISO", "year")) %>%
  mutate(
    damage_pct_gdp = if_else(
      !is.na(gdp) & gdp > 0,
      (total_damages * 1000) / gdp * 100,
      NA_real_
    )
  )

# ============================================
# FAY HATTI VERiSi (tektonik plaka sınırları)
# Kaynak: fraxen/tectonicplates (GitHub)
# Uygulama başlangıcında bir kez yüklenir
# ============================================

fault_lines <- tryCatch(
  sf::st_read(
    "https://raw.githubusercontent.com/fraxen/tectonicplates/master/GeoJSON/PB2002_boundaries.json",
    quiet = TRUE
  ),
  error = function(e) {
    message("Fay hattı verisi yüklenemedi: ", e$message)
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
# COLOR PALETTE — felaket türü → hex renk
# Hem marker hem lejant aynı hex kodunu kullanır → renk uyuşmazlığı imkânsız
# ============================================

disaster_legend_colors <- c(
  "Storm"      = "#1a3a5c",
  "Flood"      = "#5bc0de",
  "Earthquake" = "#d9534f",
  "Drought"    = "#f0ad4e",
  "Wildfire"   = "#8B0000",
  "Landslide"  = "#5cb85c"
)

# Felaket türü → hex renk (legend ile aynı kaynak)
disaster_to_hexcolor <- function(dtype) {
  colors <- disaster_legend_colors
  ifelse(dtype %in% names(colors), colors[dtype], "#888888")
}

# ============================================
# UI
# ============================================

ui <- fluidPage(
  title = "Natural Disasters Dashboard",

  tags$head(tags$style(HTML("

    /* ══════════════════════════════════════════
       GLOBAL
    ══════════════════════════════════════════ */
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');

    *, *::before, *::after { box-sizing: border-box; }

    body {
      font-family: 'Inter', 'Segoe UI', Helvetica, sans-serif;
      background-color: #f1f5f9;
      margin: 0; padding: 0;
      color: #1e293b;
    }

    /* ══════════════════════════════════════════
       HEADER
    ══════════════════════════════════════════ */
    .top-header {
      background: linear-gradient(135deg, #0f1923 0%, #1d3557 60%, #22405f 100%);
      color: white;
      padding: 16px 32px 14px 32px;
      border-bottom: 3px solid #2563eb;
    }
    .top-header h2 {
      margin: 0; font-size: 21px; font-weight: 700; letter-spacing: -0.2px;
    }
    .top-header p {
      margin: 5px 0 0 0; font-size: 11.5px; opacity: 0.60;
      letter-spacing: 0.4px; font-weight: 400;
    }

    /* ══════════════════════════════════════════
       STAT CARDS
    ══════════════════════════════════════════ */
    .stat-bar {
      background: #ffffff;
      border-bottom: 1px solid #e2e8f0;
      padding: 14px 32px;
      display: flex; gap: 14px;
    }
    .stat-card {
      flex: 1; background: #ffffff;
      border-radius: 10px;
      padding: 14px 20px 12px 20px;
      border: 1px solid #e2e8f0;
      border-top: 3px solid #94a3b8;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      transition: box-shadow 0.2s;
    }
    .stat-card:hover { box-shadow: 0 4px 12px rgba(0,0,0,0.10); }
    .stat-card.blue   { border-top-color: #2563eb; }
    .stat-card.red    { border-top-color: #dc2626; }
    .stat-card.orange { border-top-color: #ea580c; }
    .stat-card.green  { border-top-color: #16a34a; }
    .stat-card .stat-value {
      font-size: 24px; font-weight: 700; color: #0f172a;
      line-height: 1.15; letter-spacing: -0.5px;
    }
    .stat-card .stat-label {
      font-size: 10.5px; color: #64748b;
      text-transform: uppercase; letter-spacing: 0.8px;
      margin-top: 4px; font-weight: 600;
    }

    /* ══════════════════════════════════════════
       SIDEBAR
    ══════════════════════════════════════════ */
    .sidebar-wrap {
      background: #ffffff;
      border-radius: 12px;
      border: 1px solid #e2e8f0;
      box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      padding: 20px 18px;
      margin-top: 16px;
      margin-left: 12px;
    }
    .sidebar-wrap h4 {
      color: #0f172a; font-weight: 700; font-size: 12px;
      text-transform: uppercase; letter-spacing: 1px;
      margin: 0 0 16px 0;
      padding-bottom: 10px;
      border-bottom: 1px solid #f1f5f9;
    }
    .sidebar-wrap .section-label {
      font-size: 10.5px; font-weight: 700; color: #94a3b8;
      text-transform: uppercase; letter-spacing: 0.9px;
      margin: 16px 0 6px 0; display: block;
    }
    .well {
      background: transparent !important; border: none !important;
      box-shadow: none !important; padding: 0 !important;
    }

    /* Checkbox styling */
    .checkbox label { font-size: 13px; color: #334155; font-weight: 500; }

    /* Select & form controls */
    .form-control {
      border-radius: 7px; border: 1px solid #e2e8f0;
      font-size: 13px; color: #334155; font-weight: 500;
      box-shadow: none !important;
    }
    .form-control:focus { border-color: #2563eb !important; }

    /* Slider accent */
    .irs-bar        { background: #2563eb !important; border-color: #2563eb !important; }
    .irs-handle     { border-color: #2563eb !important; background: #fff !important; }
    .irs-from, .irs-to, .irs-single { background: #2563eb !important; border-radius: 4px; }
    .irs-line       { background: #e2e8f0 !important; border-color: #e2e8f0 !important; }

    /* ══════════════════════════════════════════
       TABS
    ══════════════════════════════════════════ */
    .main-wrap { margin: 16px 12px 16px 0; }

    .nav-tabs {
      border-bottom: 1px solid #e2e8f0 !important;
      background: #ffffff;
      border-radius: 12px 12px 0 0;
      padding: 0 20px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.05);
    }
    .nav-tabs > li > a {
      color: #64748b; font-weight: 600; font-size: 12.5px;
      border: none !important;
      border-bottom: 2px solid transparent !important;
      padding: 13px 15px; margin-bottom: -1px;
      border-radius: 0 !important;
      transition: color 0.15s, border-color 0.15s;
      letter-spacing: 0.1px;
    }
    .nav-tabs > li > a:hover {
      color: #1e293b; background: transparent !important;
      border-bottom-color: #cbd5e1 !important;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus {
      color: #2563eb !important; background: transparent !important;
      border-bottom: 2px solid #2563eb !important;
    }
    .tab-content {
      background: #ffffff;
      border-radius: 0 0 12px 12px;
      padding: 22px 26px 26px 26px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.05);
      border: 1px solid #e2e8f0;
      border-top: none;
    }

    /* ══════════════════════════════════════════
       RQ BANNERS
    ══════════════════════════════════════════ */
    .rq-banner {
      background: #f8fafc;
      border-left: 3px solid #2563eb;
      border-radius: 0 8px 8px 0;
      padding: 9px 16px;
      margin-bottom: 14px;
      font-size: 12.5px;
      color: #334155;
      font-weight: 500;
      letter-spacing: 0.1px;
    }

    /* ══════════════════════════════════════════
       MAP & PLOTLY
    ══════════════════════════════════════════ */
    .leaflet-container { border-radius: 10px; border: 1px solid #e2e8f0; }
    .leaflet-control-zoom a {
      border-radius: 6px !important;
      font-size: 16px !important;
    }

    /* Clean up plotly toolbar */
    .modebar { opacity: 0.4; transition: opacity 0.2s; }
    .modebar:hover { opacity: 1; }

    /* ══════════════════════════════════════════
       DATA TABLE
    ══════════════════════════════════════════ */
    .dataTables_wrapper { font-size: 13px; color: #334155; }
    table.dataTable thead th {
      background: #f8fafc; color: #475569;
      font-weight: 700; font-size: 11px;
      text-transform: uppercase; letter-spacing: 0.6px;
      border-bottom: 2px solid #e2e8f0 !important;
    }
    table.dataTable tbody tr:hover { background: #f0f9ff !important; }

  "))),

  # ── TOP HEADER ──────────────────────────────────
  div(class = "top-header",
    div(style = "display:flex; align-items:center; gap:14px;",
      span("🌍", style = "font-size:30px; opacity:0.9;"),
      div(
        h2("Natural Disasters & Economic Impact Dashboard"),
        p("EM-DAT Global Dataset  ·  1900–2021  ·  Storm · Flood · Earthquake · Drought · Wildfire · Landslide")
      )
    )
  ),

  # ── STAT CARDS ──────────────────────────────────
  div(class = "stat-bar",
    uiOutput("stat_countries"),
    uiOutput("stat_events"),
    uiOutput("stat_damage"),
    uiOutput("stat_deaths")
  ),

  # ── SIDEBAR + MAIN ──────────────────────────────
  sidebarLayout(

    sidebarPanel(
      width = 3,
      div(class = "sidebar-wrap",

        h4("Filters"),

        div(class = "section-label", "Time Period"),
        sliderInput("year_range", label = NULL,
                    min = 1900, max = 2021,
                    value = c(1970, 2021), sep = ""),

        div(class = "section-label", "Disaster Type"),
        checkboxGroupInput("disaster_type", label = NULL,
                           choices  = c("Storm", "Flood", "Earthquake",
                                        "Drought", "Wildfire", "Landslide"),
                           selected = c("Storm", "Flood", "Earthquake",
                                        "Drought", "Wildfire", "Landslide")),

        div(class = "section-label", "Continent"),
        selectInput("continent", label = NULL,
                    choices  = c("All", sort(unique(df_clean$continent))),
                    selected = "All"),

        div(class = "section-label", "Income Level"),
        selectInput("income_filter", label = NULL,
                    choices  = c("All", "Low income", "Lower middle income",
                                 "Upper middle income", "High income"),
                    selected = "All"),

        hr(style = "margin:18px 0 8px 0; border:none; border-top:1px solid #f1f5f9;"),
        div(class = "section-label", "Map Overlay"),
        checkboxInput("show_faults", 
                      HTML("&#127755; Fault Lines <span style='font-size:10px;color:#95a5a6;'>(Earthquake)</span>"),
                      value = FALSE),

        hr(style = "margin:18px 0 10px 0; border:none; border-top:1px solid #f1f5f9;"),
        p("Source: EM-DAT · World Bank",
          style = "font-size:10px; color:#94a3b8; margin:0; text-align:center; letter-spacing:0.3px;")
      )
    ),

    mainPanel(
      width = 9,
      div(class = "main-wrap",
        tabsetPanel(
          id = "main_tabs",

          # ── TAB 1: WORLD MAP ───────────────
          tabPanel("World Map",
            br(),
            div(class = "rq-banner",
              "Hover over a marker to preview — click for full details. Circle size reflects total economic damage."
            ),
            leafletOutput("world_map", height = "520px")
          ),

          # ── TAB 2: INCOME & DAMAGE ─────────
          tabPanel("Income & Damage",
            br(),
            div(class = "rq-banner",
              "RQ1: How does economic damage relate to a country's income level?"
            ),
            plotlyOutput("income_plot", height = "350px"),
            br(),
            div(class = "rq-banner",
              "RQ1 (normalized): Damage as % of GDP — removes infrastructure cost bias and reveals that lower-income countries bear a heavier relative burden."
            ),
            plotlyOutput("income_plot_normalized", height = "350px")
          ),

          # ── TAB 3: DAMAGE TRENDS ───────────
          tabPanel("Damage Trends",
            br(),
            div(class = "rq-banner",
              "RQ2: Which disaster types cause the most economic damage,
               and has this changed over time?"
            ),
            plotlyOutput("bar_plot",  height = "270px"),
            br(),
            plotlyOutput("line_plot", height = "270px")
          ),

          # ── TAB 4: BY CONTINENT ────────────
          tabPanel("By Continent",
            br(),
            div(class = "rq-banner",
              "RQ3: How does disaster frequency and total economic loss
               vary across continents?"
            ),
            plotlyOutput("continent_freq_plot",    height = "270px"),
            br(),
            plotlyOutput("continent_scatter_plot", height = "270px")
          ),

          # ── TAB 5: HEATMAP ─────────────────
          tabPanel("Heatmap",
            br(),
            div(class = "rq-banner",
              "RQ3: Which disaster type causes the most economic damage in each continent?"
            ),
            plotlyOutput("heatmap_plot", height = "420px")
          ),

          # ── TAB 6: DATA TABLE ──────────────
          tabPanel("Data Table",
            br(),
            div(class = "rq-banner",
              "Browse, search and sort all filtered records. Use column filters to drill down."
            ),
            DTOutput("data_table")
          )
        )
      )
    )
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

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {

  # ── REACTIVE: filtered data ───────────────
  filtered_data <- reactive({
    data <- df_clean %>%
      filter(
        year          >= input$year_range[1],
        year          <= input$year_range[2],
        disaster_type %in% input$disaster_type
      )

    if (input$continent != "All")
      data <- data %>% filter(continent == input$continent)

    if (input$income_filter != "All")
      data <- data %>% filter(income_level == input$income_filter)

    data
  })

  # ── REACTIVE: country-level summary for map
  map_summary <- reactive({
    filtered_data() %>%
      group_by(country, ISO, continent) %>%
      summarise(
        total_damages  = sum(total_damages,  na.rm = TRUE),
        total_events   = n(),
        total_deaths   = sum(total_deaths,   na.rm = TRUE),
        total_affected = sum(total_affected, na.rm = TRUE),
        top_disaster   = names(sort(table(disaster_type),
                                    decreasing = TRUE))[1],
        .groups = "drop"
      ) %>%
      left_join(country_coords, by = "ISO") %>%
      filter(!is.na(lat), !is.na(long))
  })

  # ============================================
  # STAT CARDS (reactive to filters)
  # ============================================

  stat_card <- function(value, label, cls) {
    div(class = paste("stat-card", cls),
      div(class = "stat-value", value),
      div(class = "stat-label", label)
    )
  }

  output$stat_countries <- renderUI({
    n <- filtered_data() %>% pull(country) %>% n_distinct()
    stat_card(format(n, big.mark = ","), "Countries", "blue")
  })
  output$stat_events <- renderUI({
    n <- nrow(filtered_data())
    stat_card(format(n, big.mark = ","), "Disaster Events", "red")
  })
  output$stat_damage <- renderUI({
    v <- sum(filtered_data()$total_damages, na.rm = TRUE)
    stat_card(paste0("$", format(round(v / 1e6, 1), big.mark = ","), "B"),
              "Total Damage (000' USD)", "orange")
  })
  output$stat_deaths <- renderUI({
    v <- sum(filtered_data()$total_deaths, na.rm = TRUE)
    stat_card(format(v, big.mark = ","), "Total Deaths", "green")
  })

  # ============================================
  # TAB 1 — WORLD MAP
  # ============================================

  # Base map — fit to world bounds to eliminate gray polar area
  output$world_map <- renderLeaflet({
    leaflet(options = leafletOptions(
        minZoom        = 2,
        maxZoom        = 10,
        worldCopyJump  = TRUE,   # smooth wrap-around when panning horizontally
        zoomControl    = TRUE
      )) %>%
      # Clean, high-contrast basemap — no clutter, labels still visible
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
        options = tileOptions(opacity = 1)
      ) %>%
      # Constrain vertical pan so polar gray areas never appear
      setMaxBounds(lng1 = -180, lat1 = -60, lng2 = 180, lat2 = 80) %>%
      fitBounds(lng1 = -150, lat1 = -55, lng2 = 160, lat2 = 72)
  })

  # ============================================
  # FAY HATTI OVERLAY (checkbox ile aç/kapat)
  # ============================================

  observe({
    proxy <- leafletProxy("world_map")

    if (!is.null(fault_lines) && isTRUE(input$show_faults)) {
      proxy %>%
        clearGroup("fault_lines") %>%
        addPolylines(
          data    = fault_lines,
          color   = "#c0392b",   # koyu kırmızı — deprem rengiyle uyumlu
          weight  = 1.2,
          opacity = 0.6,
          group   = "fault_lines",
          label   = "Tectonic Plate Boundary"
        )
    } else {
      proxy %>% clearGroup("fault_lines")
    }
  })

  # Update markers whenever filters change
  observe({
    data <- map_summary()

    # Filtreler boş sonuç döndürdüğünde mevcut markerleri temizle ve çık
    if (nrow(data) == 0) {
      leafletProxy("world_map") %>%
        clearMarkers() %>%
        clearControls()
      return()
    }

    # Marker color from dominant disaster type
    data <- data %>%
      mutate(
        marker_color = disaster_to_hexcolor(top_disaster),

        # Radius scaled by sqrt(damage)
        radius = scales::rescale(sqrt(total_damages), to = c(5, 22)),

        # Hover label: clean, concise
        hover_label = paste0(
          country, " · ", top_disaster,
          "  $", format(round(total_damages / 1e3), big.mark = ","), "M"
        ),

        # Rich HTML popup — card style with colored header strip
        popup_html = paste0(
          "<div style='font-family:Segoe UI,Helvetica,sans-serif;",
                     "min-width:230px;max-width:280px;border-radius:8px;",
                     "overflow:hidden;box-shadow:0 2px 10px rgba(0,0,0,0.15);'>",

          "<div style='background:", marker_color, ";padding:10px 14px;'>",
            "<span style='color:#fff;font-size:14px;font-weight:700;'>",
              "📍 ", country, "</span><br>",
            "<span style='color:rgba(255,255,255,0.85);font-size:11px;'>",
              continent, " · ", top_disaster, "</span>",
          "</div>",

          "<div style='padding:10px 14px;background:#fff;'>",
          "<table style='width:100%;font-size:12.5px;border-collapse:collapse;color:#2c3e50;'>",

          "<tr style='border-bottom:1px solid #f0f0f0;'>",
            "<td style='padding:4px 0;color:#7f8c8d;'>Events</td>",
            "<td style='padding:4px 0;font-weight:600;text-align:right;'>",
              format(total_events, big.mark = ","), "</td></tr>",

          "<tr style='border-bottom:1px solid #f0f0f0;'>",
            "<td style='padding:4px 0;color:#7f8c8d;'>Total Damage</td>",
            "<td style='padding:4px 0;font-weight:700;color:#c0392b;text-align:right;'>",
              "$", format(round(total_damages), big.mark = ","), " k</td></tr>",

          "<tr style='border-bottom:1px solid #f0f0f0;'>",
            "<td style='padding:4px 0;color:#7f8c8d;'>Deaths</td>",
            "<td style='padding:4px 0;font-weight:600;text-align:right;'>",
              format(total_deaths, big.mark = ","), "</td></tr>",

          "<tr>",
            "<td style='padding:4px 0;color:#7f8c8d;'>People Affected</td>",
            "<td style='padding:4px 0;font-weight:600;text-align:right;'>",
              format(total_affected, big.mark = ","), "</td></tr>",

          "</table></div></div>"
        )
      )

    # CircleMarkers — white stroke for crisp separation on Positron basemap
    leafletProxy("world_map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data         = data,
        lng          = ~long,
        lat          = ~lat,
        color        = "#ffffff",
        fillColor    = ~marker_color,
        fillOpacity  = 0.80,
        radius       = ~radius,
        weight       = 1.8,
        opacity      = 1,
        popup        = ~popup_html,
        popupOptions = popupOptions(maxWidth = 290, closeButton = TRUE),
        label        = ~hover_label,
        labelOptions = labelOptions(
          style = list(
            "font-family"   = "Segoe UI, Helvetica, sans-serif",
            "font-size"     = "12px",
            "font-weight"   = "600",
            "background"    = "rgba(255,255,255,0.95)",
            "border"        = "none",
            "border-radius" = "6px",
            "padding"       = "4px 10px",
            "box-shadow"    = "0 1px 6px rgba(0,0,0,0.18)"
          ),
          noHide = FALSE
        )
      ) %>%
      addLegend(
        position  = "bottomright",
        colors    = unname(disaster_legend_colors),
        labels    = names(disaster_legend_colors),
        title     = "<span style='font-size:12px;font-weight:700;'>Top Disaster Type</span>",
        opacity   = 0.95
      )
  })

  # ============================================
  # TAB 2 — INCOME & DAMAGE  (RQ1)
  # ============================================

  output$income_plot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(income_level = factor(income_level,
                                   levels = c("Low income",
                                              "Lower middle income",
                                              "Upper middle income",
                                              "High income")))
    p <- ggplot(data, aes(x = income_level, y = total_damages,
                           fill = income_level,
                           text = paste0("Country: ", country,
                                         "<br>Damages: $",
                                         format(round(total_damages),
                                                big.mark = ","),
                                         " (000' USD)"))) +
      geom_boxplot(outlier.alpha = 0.3) +
      scale_y_log10(labels = comma) +
      scale_fill_brewer(palette = "RdYlGn") +
      labs(title = "Economic Damage by Income Level (log scale)",
           x = "Income Level",
           y = "Total Damages (000' USD)") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") %>% plotly_theme()
  })

  output$income_plot_normalized <- renderPlotly({
    data <- filtered_data() %>%
      filter(!is.na(damage_pct_gdp)) %>%
      mutate(income_level = factor(income_level,
                                   levels = c("Low income",
                                              "Lower middle income",
                                              "Upper middle income",
                                              "High income")))

    p <- ggplot(data, aes(x = income_level, y = damage_pct_gdp,
                           fill = income_level,
                           text = paste0("Country: ", country,
                                         "<br>Damage as % of GDP: ",
                                         round(damage_pct_gdp, 3), "%"))) +
      geom_boxplot(outlier.alpha = 0.3) +
      scale_y_log10(labels = scales::label_number(suffix = "%")) +
      scale_fill_brewer(palette = "RdYlGn") +
      labs(title = "Economic Damage as % of GDP by Income Level (log scale)",
           subtitle = "1960–2021 | Normalized: removes infrastructure cost bias",
           x = "Income Level",
           y = "Damage as % of GDP") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") %>% plotly_theme()
  })

  # ============================================
  # TAB 3 — DAMAGE TRENDS  (RQ2)
  # ============================================

  output$bar_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(disaster_type) %>%
      summarise(total = sum(total_damages, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total))

    p <- ggplot(data, aes(x = reorder(disaster_type, total),
                           y = total, fill = disaster_type,
                           text = paste0(disaster_type, ": $",
                                         format(round(total), big.mark = ","),
                                         " (000' USD)"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Total Economic Damage by Disaster Type",
           x = NULL, y = "Total Damages (000' USD)") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text") %>% plotly_theme()
  })

  output$line_plot <- renderPlotly({
    # Uses real_damages (constant 2021 USD) to remove nominal inflation bias
    data <- filtered_data() %>%
      group_by(year, disaster_type) %>%
      summarise(total = sum(real_damages, na.rm = TRUE), .groups = "drop")

    # geom_line → çizgiyi çizer (text aesthetic olmadan, aksi halde ggplotly render etmez)
    # geom_point → aynı veriyle hover tooltip taşır (görünmez, size = 0)
    p <- ggplot(data, aes(x = year, y = total, color = disaster_type)) +
      geom_line(linewidth = 0.8) +
      geom_point(aes(text = paste0("Year: ", year,
                                   "<br>Type: ", disaster_type,
                                   "<br>Damages (2021 USD): $",
                                   format(round(total), big.mark = ","),
                                   " (000')")),
                 size = 0.8, alpha = 0.6) +
      scale_y_continuous(labels = comma) +
      scale_color_brewer(palette = "Set2") +
      labs(title = "Damage Trends Over Time by Disaster Type",
           subtitle = "Constant 2021 USD (CPI-adjusted)",
           x = "Year", y = "Total Damages (Constant 2021 USD, 000')",
           color = "Disaster Type") +
      theme_minimal()

    ggplotly(p, tooltip = "text") %>% plotly_theme()
  })

  # ============================================
  # TAB 4 — BY CONTINENT  (RQ3)
  # ============================================

  output$continent_freq_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(continent, disaster_type) %>%
      summarise(count = n(), .groups = "drop")

    p <- ggplot(data, aes(x = continent, y = count, fill = disaster_type,
                           text = paste0(continent, " – ", disaster_type,
                                         ": ", count, " events"))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Disaster Frequency by Continent & Type",
           x = "Continent", y = "Number of Events",
           fill = "Disaster Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 15, hjust = 1))

    ggplotly(p, tooltip = "text") %>% plotly_theme()
  })

  output$continent_scatter_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(continent) %>%
      summarise(
        total_damages = sum(total_damages, na.rm = TRUE),
        total_events  = n(),
        .groups = "drop"
      )

    p <- ggplot(data, aes(x = total_events, y = total_damages,
                           color = continent, size = total_damages,
                           text = paste0("Continent: ", continent,
                                         "<br>Events: ",
                                         format(total_events, big.mark = ","),
                                         "<br>Damages: $",
                                         format(round(total_damages),
                                                big.mark = ","),
                                         " (000' USD)"))) +
      geom_point(alpha = 0.75) +
      scale_y_continuous(labels = comma) +
      scale_size_continuous(guide = "none") +
      scale_color_brewer(palette = "Set1") +
      labs(title = "Frequency vs Total Economic Damage by Continent",
           x = "Number of Disaster Events",
           y = "Total Damages (000' USD)",
           color = "Continent") +
      theme_minimal()

    ggplotly(p, tooltip = "text") %>% plotly_theme()
  })

  # ============================================
  # TAB 5 — DATA TABLE
  # ============================================

  # ============================================
  # TAB 5 — HEATMAP  (RQ3)
  # Kıta × Felaket Türü — ortalama hasar (milyar USD)
  # ============================================

  output$heatmap_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(continent, disaster_type) %>%
      summarise(
        avg_damage   = mean(total_damages, na.rm = TRUE) / 1e6,  # milyar USD
        total_damage = sum(total_damages,  na.rm = TRUE) / 1e6,
        n_events     = n(),
        .groups = "drop"
      )

    p <- ggplot(data, aes(
        x    = continent,
        y    = disaster_type,
        fill = avg_damage,
        text = paste0(
          continent, " — ", disaster_type, "<br>",
          "Avg Damage: $", format(round(avg_damage, 1), big.mark = ","), "B<br>",
          "Total Damage: $", format(round(total_damage, 1), big.mark = ","), "B<br>",
          "Events: ", n_events
        )
      )) +
      geom_tile(color = "white", linewidth = 0.6) +
      scale_fill_gradientn(
        colors   = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
        name     = "Avg Damage\n($B)",
        na.value = "#f0f2f5"
      ) +
      labs(
        title = "Average Economic Damage: Continent \u00d7 Disaster Type",
        subtitle = "Renk: k\u0131tadaki ortalama olay ba\u015f\u0131na hasar (milyar USD)",
        x = NULL,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x     = element_text(face = "bold", size = 11),
        axis.text.y     = element_text(size = 11),
        panel.grid      = element_blank(),
        legend.position = "right",
        plot.subtitle   = element_text(size = 10, color = "#7f8c8d")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "")) %>%
      plotly_theme()
  })

  # ============================================
  # TAB 6 — DATA TABLE
  # ============================================

  output$data_table <- renderDT({
    filtered_data() %>%
      select(year, country, continent, disaster_type,
             income_level, total_damages, total_deaths, total_affected) %>%
      arrange(desc(total_damages)) %>%
      datatable(
        filter   = "top",
        rownames = FALSE,
        options  = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "Bfrtip"
        ),
        colnames = c("Year", "Country", "Continent", "Disaster Type",
                     "Income Level", "Damages (000' USD)",
                     "Deaths", "Affected")
      ) %>%
      formatCurrency(c("total_damages", "total_deaths", "total_affected"),
                     currency = "", interval = 3, mark = ",", digits = 0)
  })
}

# ============================================
# RUN APP
# ============================================

shinyApp(ui = ui, server = server)
