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

    /* ── Global ── */
    body {
      font-family: 'Segoe UI', Helvetica, sans-serif;
      background-color: #f0f2f5;
      margin: 0; padding: 0;
    }

    /* ── Top header bar ── */
    .top-header {
      background: linear-gradient(135deg, #1a2332 0%, #2c3e50 100%);
      color: white;
      padding: 18px 30px 14px 30px;
      margin-bottom: 0;
    }
    .top-header h2 {
      margin: 0; font-size: 24px; font-weight: 700; letter-spacing: 0.3px;
    }
    .top-header p {
      margin: 4px 0 0 0; font-size: 12px; opacity: 0.7; letter-spacing: 0.5px;
    }

    /* ── Stat cards ── */
    .stat-bar {
      background: #ffffff;
      border-bottom: 1px solid #dee2e6;
      padding: 12px 30px;
      display: flex; gap: 16px;
    }
    .stat-card {
      flex: 1; background: #fff; border-radius: 8px;
      padding: 12px 18px; text-align: center;
      border-left: 4px solid #2c3e50;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
    }
    .stat-card.blue  { border-left-color: #3498db; }
    .stat-card.red   { border-left-color: #e74c3c; }
    .stat-card.green { border-left-color: #27ae60; }
    .stat-card.orange{ border-left-color: #e67e22; }
    .stat-card .stat-value {
      font-size: 22px; font-weight: 700; color: #2c3e50; line-height: 1.2;
    }
    .stat-card .stat-label {
      font-size: 11px; color: #7f8c8d; text-transform: uppercase;
      letter-spacing: 0.6px; margin-top: 2px;
    }

    /* ── Sidebar ── */
    .sidebar-wrap {
      background: #ffffff; border-radius: 10px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      padding: 18px 16px; margin-top: 16px; margin-left: 12px;
    }
    .sidebar-wrap h4 {
      color: #1a2332; font-weight: 700; font-size: 14px;
      text-transform: uppercase; letter-spacing: 0.8px; margin: 0 0 12px 0;
    }
    .sidebar-wrap .section-label {
      font-size: 11px; font-weight: 600; color: #95a5a6;
      text-transform: uppercase; letter-spacing: 0.7px;
      margin: 14px 0 4px 0;
    }
    .well { background: transparent !important; border: none !important;
            box-shadow: none !important; padding: 0 !important; }

    /* ── Tabs ── */
    .main-wrap { margin: 16px 12px 16px 0; }
    .nav-tabs {
      border-bottom: 2px solid #dee2e6;
      background: #ffffff;
      border-radius: 10px 10px 0 0;
      padding: 0 16px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    }
    .nav-tabs > li > a {
      color: #5d6d7e; font-weight: 600; font-size: 13px;
      border: none !important; border-bottom: 3px solid transparent !important;
      padding: 12px 16px; margin-bottom: -2px; border-radius: 0 !important;
    }
    .nav-tabs > li > a:hover {
      color: #2c3e50; background: transparent !important;
      border-bottom-color: #bdc3c7 !important;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus {
      color: #2980b9 !important; background: transparent !important;
      border-bottom: 3px solid #2980b9 !important;
    }
    .tab-content {
      background: #ffffff; border-radius: 0 0 10px 10px;
      padding: 20px 24px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.06);
    }

    /* ── Tab research question banners ── */
    .rq-banner {
      background: linear-gradient(90deg, #eaf4fb, #f8f9fa);
      border-left: 4px solid #2980b9;
      border-radius: 0 6px 6px 0;
      padding: 10px 16px; margin-bottom: 16px;
      font-size: 13px; color: #2c3e50; font-weight: 600;
    }

    /* ── Leaflet map ── */
    .leaflet-container { border-radius: 8px; }

    /* ── Slider & inputs ── */
    .irs-bar { background: #2980b9 !important; border-color: #2980b9 !important; }
    .irs-handle { border-color: #2980b9 !important; }
    .irs-from, .irs-to, .irs-single { background: #2980b9 !important; }
    .form-control { border-radius: 6px; border-color: #dee2e6; font-size: 13px; }
    .checkbox label { font-size: 13px; }

    /* ── DT table ── */
    .dataTables_wrapper { font-size: 13px; }
  "))),

  # ── TOP HEADER ──────────────────────────────────
  div(class = "top-header",
    div(style = "display:flex; align-items:center; gap:12px;",
      span("🌍", style = "font-size:32px;"),
      div(
        h2("Natural Disasters & Economic Impact Dashboard"),
        p("EM-DAT Global Dataset  |  1900–2021  |  Storm · Flood · Earthquake · Drought · Wildfire · Landslide")
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

        h4("⚙  Filters"),

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

        hr(style = "margin:14px 0 6px 0; border-color:#eee;"),
        div(class = "section-label", "Map Overlay"),
        checkboxInput("show_faults", 
                      HTML("&#127755; Fault Lines <span style='font-size:10px;color:#95a5a6;'>(Earthquake)</span>"),
                      value = FALSE),

        hr(style = "margin:16px 0 8px 0; border-color:#eee;"),
        p("Source: EM-DAT \u0026 World Bank",
          style = "font-size:10px; color:#bdc3c7; margin:0; text-align:center;")
      )
    ),

    mainPanel(
      width = 9,
      div(class = "main-wrap",
        tabsetPanel(
          id = "main_tabs",

          # ── TAB 1: WORLD MAP ───────────────
          tabPanel("🗺  World Map",
            br(),
            div(class = "rq-banner",
              "📍 Hover over a pin to preview — click to see full country details.
               Each pin is a country with recorded disaster damage in the selected filters."
            ),
            leafletOutput("world_map", height = "520px")
          ),

          # ── TAB 2: INCOME & DAMAGE ─────────
          tabPanel("💰 Income & Damage",
            br(),
            div(class = "rq-banner",
              "RQ1: How does economic damage relate to a country's income level?"
            ),
            plotlyOutput("income_plot", height = "450px")
          ),

          # ── TAB 3: DAMAGE TRENDS ───────────
          tabPanel("📈 Damage Trends",
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
          tabPanel("🌐 By Continent",
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
          tabPanel("🔥 Heatmap",
            br(),
            div(class = "rq-banner",
              "RQ3: Which disaster type causes the most economic damage in each continent?"
            ),
            plotlyOutput("heatmap_plot", height = "420px")
          ),

          # ── TAB 6: DATA TABLE ──────────────
          tabPanel("📋 Data Table",
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
    leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
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

    # Her ülke dairesi o ülkedeki baskın felaket türünün hex rengiyle boyanır
    data <- data %>%
      mutate(
        marker_color = disaster_to_hexcolor(top_disaster),

        # Daire boyutu hasara göre ölçeklenir (sqrt ile aşırı fark yumuşatılır)
        radius = scales::rescale(sqrt(total_damages), to = c(6, 24)),

        # Hover etiketi: ülke + baskın felaket + hasar
        hover_label = paste0("f30d ", country, "  ⚡ ", top_disaster,
                             "  $", format(round(total_damages / 1e3),
                                          big.mark = ","), "M"),

        # Rich HTML popup shown on click
        popup_html = paste0(
          "<div style='font-family:Segoe UI,sans-serif;min-width:210px;",
                           "padding:4px;'>",
          "<h4 style='margin:0 0 8px 0;color:#2c3e50;border-bottom:",
                     "2px solid #eee;padding-bottom:4px;'>",
            "📍 ", country, "</h4>",
          "<table style='width:100%;font-size:13px;border-collapse:collapse;'>",
          "<tr style='background:#f9f9f9;'>",
            "<td style='padding:3px 6px;'><b>Continent</b></td>",
            "<td style='padding:3px 6px;'>",      continent,    "</td></tr>",
          "<tr>",
            "<td style='padding:3px 6px;'><b>Top Disaster</b></td>",
            "<td style='padding:3px 6px;'>",   top_disaster, "</td></tr>",
          "<tr style='background:#f9f9f9;'>",
            "<td style='padding:3px 6px;'><b>Total Events</b></td>",
            "<td style='padding:3px 6px;'>",
              format(total_events, big.mark = ","),               "</td></tr>",
          "<tr>",
            "<td style='padding:3px 6px;'><b>Damages (000' USD)</b></td>",
            "<td style='padding:3px 6px;color:#c0392b;font-weight:bold;'>$",
              format(round(total_damages), big.mark = ","),       "</td></tr>",
          "<tr style='background:#f9f9f9;'>",
            "<td style='padding:3px 6px;'><b>Deaths</b></td>",
            "<td style='padding:3px 6px;'>",
              format(total_deaths, big.mark = ","),               "</td></tr>",
          "<tr>",
            "<td style='padding:3px 6px;'><b>People Affected</b></td>",
            "<td style='padding:3px 6px;'>",
              format(total_affected, big.mark = ","),             "</td></tr>",
          "</table></div>"
        )
      )

    # CircleMarkers — hex renk doğrudan atanır, lejantla bire bir örtüşür
    leafletProxy("world_map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data         = data,
        lng          = ~long,
        lat          = ~lat,
        color        = ~marker_color,   # hex → border
        fillColor    = ~marker_color,   # hex → iç alan
        fillOpacity  = 0.85,
        radius       = ~radius,         # hasara göre ölçekli boyut
        weight       = 1.5,
        opacity      = 1,
        popup        = ~popup_html,
        label        = ~hover_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "font-size" = "12px",
                       "background" = "white", "border" = "1px solid #ccc",
                       "border-radius" = "4px", "padding" = "4px 8px")
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors   = unname(disaster_legend_colors),
        labels   = names(disaster_legend_colors),
        title    = "Top Disaster Type",
        opacity  = 0.9
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

    ggplotly(p, tooltip = "text")
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

    ggplotly(p, tooltip = "text")
  })

  output$line_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(year, disaster_type) %>%
      summarise(total = sum(total_damages, na.rm = TRUE), .groups = "drop")

    # geom_line → çizgiyi çizer (text aesthetic olmadan, aksi halde ggplotly render etmez)
    # geom_point → aynı veriyle hover tooltip taşır (görünmez, size = 0)
    p <- ggplot(data, aes(x = year, y = total, color = disaster_type)) +
      geom_line(linewidth = 0.8) +
      geom_point(aes(text = paste0("Year: ", year,
                                   "<br>Type: ", disaster_type,
                                   "<br>Damages: $",
                                   format(round(total), big.mark = ","),
                                   " (000' USD)")),
                 size = 0.8, alpha = 0.6) +
      scale_y_continuous(labels = comma) +
      scale_color_brewer(palette = "Set2") +
      labs(title = "Damage Trends Over Time by Disaster Type",
           x = "Year", y = "Total Damages (000' USD)",
           color = "Disaster Type") +
      theme_minimal()

    ggplotly(p, tooltip = "text")
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

    ggplotly(p, tooltip = "text")
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

    ggplotly(p, tooltip = "text")
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
      layout(xaxis = list(title = ""), yaxis = list(title = ""))
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
