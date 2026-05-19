# Natural Disasters & Economic Impact Dashboard

This repository contains the final project for **CEN314**, focusing on the economic and human impact of global natural disasters between 1900 and 2021. The project uses data from **EM-DAT** and the **World Bank** to provide an interactive dashboard built with R and Shiny.

## 📁 Directory Structure & File Descriptions

The project follows a standard, modular data science repository structure. Here is what each file does:

```text
natural-disasters-dashboard/
├── app/                  # Main Shiny application files
│   ├── global.R          # PURPOSE: Runs once at startup. Loads datasets, cleans data, fetches API data (CPI/GDP), and defines global palettes.
│   ├── ui.R              # PURPOSE: Defines the frontend. Contains custom CSS, sidebar filters, and tab layouts.
│   └── server.R          # PURPOSE: Defines the backend. Handles reactive filtering, draws interactive Plotly charts, and updates the Leaflet map.
├── analysis/             # Exploratory Data Analysis (EDA)
│   └── disasters_project.R # PURPOSE: A prototype script used for initial EDA and static data visualization before building the dashboard.
├── data/                 # Raw datasets
│   └── emdat_data.csv    # The core EM-DAT dataset used across the project.
├── docs/                 # Documentation & Reports
│   ├── CEN314_Report.docx
│   └── CEN314_Final_Project.pdf
└── README.md             # This file
```

## 🚀 How to Run the Application

The dashboard is built entirely in R using the `shiny` framework.

1. **Open the Project:**
   Open the R project or set your working directory to the `natural-disasters-dashboard` folder.

2. **Install Required Packages:**
   Ensure you have the following packages installed:
   ```r
   install.packages(c("shiny", "tidyverse", "plotly", "leaflet", "DT", "wbstats", "maps", "countrycode", "scales", "sf"))
   ```

3. **Launch the App:**
   You can start the dashboard in two ways:
   - **Method A (RStudio):** Open `app/ui.R` or `app/server.R` in RStudio and click the **"Run App"** button at the top right of the script editor.
   - **Method B (Console):** Run the following command in your R console:
     ```r
     shiny::runApp("app")
     ```

## ✨ Key Features & Methodologies

* **Interactive Mapping (Leaflet):** Real-time mapping of disaster events with tectonic fault line data and tropical cyclone basin overlays.
* **Inflation Adjustment (CPI):** Uses World Bank API data to normalize nominal historical damages to Constant 2021 USD.
* **GDP Normalization:** Evaluates economic damage as a percentage of a country's GDP to remove infrastructure cost bias and analyze relative economic burden.
* **Dynamic Plotly Visualizations:** Includes faceted charts, boxplots with log-scales, and a complex heatmap for continent-level disaster profiling.
