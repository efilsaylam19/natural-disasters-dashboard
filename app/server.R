# ============================================
# CEN314 - Final Project
# Natural Disasters & Economic Damage
# Shiny Dashboard - server.R
# 
# PURPOSE:
# This file defines the backend logic of the application.
# It processes user inputs from the UI, filters the dataset reactively,
# dynamically generates the Leaflet map, computes statistics for stat cards,
# and renders all interactive Plotly charts and data tables.
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
  # FAULT LINE OVERLAY (toggle with checkbox)
  # ============================================

  observe({
    proxy <- leafletProxy("world_map")

    if (!is.null(fault_lines) && isTRUE(input$show_faults)) {
      proxy %>%
        clearGroup("fault_lines") %>%
        addPolylines(
          data    = fault_lines,
          color   = "#c0392b",   # dark red — matches earthquake color
          weight  = 1.2,
          opacity = 0.6,
          group   = "fault_lines",
          label   = "Tectonic Plate Boundary"
        )
    } else {
      proxy %>% clearGroup("fault_lines")
    }
  })

  # ============================================
  # STORM BASIN OVERLAY (toggle with checkbox)
  # ============================================

  observe({
    proxy <- leafletProxy("world_map")

    if (isTRUE(input$show_storm_zones)) {
      proxy %>% clearGroup("storm_basins")
      for (b in storm_basins) {
        label_html <- paste0(
          "<div style='font-family:Segoe UI,sans-serif;padding:6px 10px;",
                         "background:rgba(255,255,255,0.96);border-radius:8px;",
                         "box-shadow:0 2px 8px rgba(0,0,0,0.18);min-width:200px;'>",
          "<b style='color:", b$color, ";font-size:13px;'>", b$name, "</b><br>",
          "<span style='font-size:11px;color:#555;'>",
            "&#128197; Season: ", b$season, "</span><br>",
          "<span style='font-size:11px;color:#333;margin-top:3px;display:block;'>",
            b$info,
          "</span></div>"
        )
        proxy %>%
          addRectangles(
            lng1 = b$lng1, lat1 = b$lat1,
            lng2 = b$lng2, lat2 = b$lat2,
            color      = b$color,
            weight     = 1.5,
            opacity    = 0.7,
            fillColor  = b$color,
            fillOpacity = 0.08,
            group      = "storm_basins",
            label      = lapply(list(label_html), htmltools::HTML),
            labelOptions = labelOptions(
              style = list(
                "background" = "transparent",
                "border"     = "none",
                "box-shadow" = "none",
                "padding"    = "0"
              ),
              direction = "auto"
            )
          )
      }
    } else {
      proxy %>% clearGroup("storm_basins")
    }
  })

  # Update markers whenever filters change
  observe({
    data <- map_summary()

    # Clear existing markers and exit if filters return empty results
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
          "  $", ifelse(total_damages >= 1e6,
                       paste0(format(round(total_damages / 1e6, 1), nsmall = 1), "B"),
                       paste0(format(round(total_damages / 1e3), big.mark = ","), "M"))
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

    # geom_line draws the line (without text aesthetic so ggplotly works correctly)
    # geom_point adds the hover tooltip with size=0 so it's invisible
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
  # TAB 5 — HEATMAP  (RQ3)
  # Continent × Disaster Type — average damage (Billion USD)
  # ============================================

  output$heatmap_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(continent, disaster_type) %>%
      summarise(
        avg_damage   = mean(total_damages, na.rm = TRUE) / 1e6,  # Billion USD
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
        subtitle = "Color: Average damage per event in the continent (Billion USD)",
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
      mutate(year = as.integer(year)) %>%
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
