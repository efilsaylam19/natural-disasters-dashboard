# ============================================
# CEN314 - Final Project
# Natural Disasters & Economic Damage
# Shiny Dashboard - ui.R
# 
# PURPOSE:
# This file defines the User Interface (frontend) of the dashboard.
# It contains the custom CSS styling, the sidebar layout with filters,
# the tab structures, and placeholders for the maps and plots.
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
        checkboxInput("show_storm_zones",
                      HTML("🌪️ Storm Basins <span style='font-size:10px;color:#95a5a6;'>(Cyclone Zones)</span>"),
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
