# ============================================
# CEN314 - Final Project
# Natural Disasters & Economic Damage
# ============================================

library(tidyverse)
library(wbstats)
library(plotly)
library(shiny)

# ============================================
# 1. LOAD & CLEAN DATA
# ============================================

setwd("C:/Users/eemre/Desktop/natural-disasters-dashboard")

df <- read_csv("1900_2021_DISASTERS.xlsx - emdat data.csv")

income <- wb_countries() %>%
  select(iso3c, income_level)

df_clean <- df %>%
  select(Year, `Disaster Type`, Country, ISO, Continent,
         `Total Deaths`, `Total Affected`,
         `Total Damages ('000 US$)`) %>%
  filter(!is.na(`Total Damages ('000 US$)`)) %>%
  rename(
    year = Year,
    disaster_type = `Disaster Type`,
    country = Country,
    continent = Continent,
    total_deaths = `Total Deaths`,
    total_affected = `Total Affected`,
    total_damages = `Total Damages ('000 US$)`
  ) %>%
  left_join(income, by = c("ISO" = "iso3c")) %>%
  filter(!is.na(income_level), income_level != "Not classified") %>%
  filter(disaster_type %in% c("Storm", "Flood", "Earthquake",
                              "Drought", "Wildfire", "Landslide"))

# ============================================
# 2. STATIC PLOTS
# ============================================

# Plot 1 - Boxplot: Economic damage by income level (RQ1)
df_clean %>%
  mutate(income_level = factor(income_level,
                               levels = c("Low income",
                                          "Lower middle income",
                                          "Upper middle income",
                                          "High income"))) %>%
  ggplot(aes(x = income_level, y = total_damages, fill = income_level)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(
    title = "Economic Damage from Natural Disasters by Income Level",
    subtitle = "1900–2021 | Log scale",
    x = "Income Level",
    y = "Total Damages (000' USD, log scale)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2 - Bar chart: Total damages by disaster type (RQ2)
df_clean %>%
  group_by(disaster_type) %>%
  summarise(total = sum(total_damages, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  ggplot(aes(x = reorder(disaster_type, total), y = total, fill = disaster_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Total Economic Damage by Disaster Type",
    subtitle = "1900–2021",
    x = "Disaster Type",
    y = "Total Damages (000' USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 3 - Line chart: Damage trend over time (RQ2)
df_clean %>%
  filter(year >= 1970) %>%
  group_by(year, disaster_type) %>%
  summarise(total = sum(total_damages, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total, color = disaster_type)) +
  geom_line(linewidth = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Economic Damage Trends by Disaster Type Over Time",
    subtitle = "1970–2021",
    x = "Year",
    y = "Total Damages (000' USD)",
    color = "Disaster Type"
  ) +
  theme_minimal()

# Plot 4 - Bar chart: Disaster frequency by continent (RQ3)
df_clean %>%
  group_by(continent, disaster_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = continent, y = count, fill = disaster_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Disaster Frequency by Continent and Type",
    subtitle = "1900–2021",
    x = "Continent",
    y = "Number of Disasters",
    fill = "Disaster Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

# Plot 5 - Point chart: Frequency vs damage by continent (RQ3)
df_clean %>%
  group_by(continent) %>%
  summarise(
    total_damages = sum(total_damages, na.rm = TRUE),
    total_events = n()
  ) %>%
  ggplot(aes(x = total_events, y = total_damages,
             color = continent, size = total_damages)) +
  geom_point(alpha = 0.7) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(guide = "none") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Disaster Frequency vs Economic Damage by Continent",
    subtitle = "1900–2021",
    x = "Number of Disaster Events",
    y = "Total Damages (000' USD)",
    color = "Continent"
  ) +
  theme_minimal()

# Plot 6 - Faceted line chart: damage trends by continent, per disaster type (RQ2 & RQ3)
# facet_wrap ile her felaket türü ayrı panelde gösterilir;
# bu grafik hem RQ2 (hangi tür daha fazla hasar) hem RQ3 (kıtalar arası fark) cevaplar.
df_clean %>%
  filter(year >= 1970) %>%
  group_by(year, disaster_type, continent) %>%
  summarise(total = sum(total_damages, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total, color = continent)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  facet_wrap(~disaster_type, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Economic Damage Trends by Disaster Type and Continent",
    subtitle = "1970\u20132021 | Faceted by Disaster Type | Y-axis free per panel",
    x        = "Year",
    y        = "Total Damages (000\u2019 USD)",
    color    = "Continent"
  ) +
  theme_minimal() +
  theme(
    strip.text       = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "#f0f2f5", color = NA),
    panel.spacing    = unit(1, "lines")
  )

# ============================================
# 3. INTERACTIVE PLOTS (PLOTLY)
# ============================================

# Interactive Plot 1
p1 <- df_clean %>%
  mutate(income_level = factor(income_level,
                               levels = c("Low income",
                                          "Lower middle income",
                                          "Upper middle income",
                                          "High income"))) %>%
  ggplot(aes(x = income_level, y = total_damages, fill = income_level)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(title = "Economic Damage by Income Level",
       x = "Income Level", y = "Total Damages (log scale)") +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p1)

# Interactive Plot 2
p2 <- df_clean %>%
  group_by(disaster_type) %>%
  summarise(total = sum(total_damages, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  ggplot(aes(x = reorder(disaster_type, total), y = total,
             fill = disaster_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Total Economic Damage by Disaster Type",
    x = "Disaster Type",
    y = "Total Damages (000' USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggplotly(p2)