---
title: SPP
sdk: docker
app_port: 7860
---

# Subnational Politics Project (SPP) - Hugging Face Deployment

This repository contains the **R Shiny** application for the Subnational Politics Project (SPP). It is specifically configured for deployment on **Hugging Face Spaces** using Docker and serves as the core interactive engine embedded via iframe on [subnationalpolitics.com](https://subnationalpolitics.com).

## 🚀 Overview

The SPP application provides interactive visualizations (maps, charts, and data tables) for subnational political data across various countries (e.g., Mexico, Argentina, Brazil).

### Key Technical Features:
- **Modular Architecture**: Uses Shiny modules for maintainability and scalability.
- **Dynamic Data Source**: Fetches and joins datasets directly from the `fsotoj/spp-data` repository.
- **Privacy-First Analytics**: Implements a `postMessage` bridge to handle Google Analytics tracking safely within an iframe context.
- **Reproducible Environment**: Managed via `renv` and containerized with Docker.

---

## 📂 Folder Structure

The project follows a modularized R Shiny structure:

- **Root Files**
  - `app.R`: Entry point for the Shiny application.
  - `ui.R`: defines the frontend layout and UI components.
  - `server.R`: Contains the backend logic, reactive expressions, and module coordination.
  - `global.R`: Handles data loading, environment setup, and global variable definitions.
- `analysis/`: Contains technical documentation, including:
  - Repository analysis and architecture overview.
  - Relational Schema / Entity-Relationship Diagram (ERD).
  - Google Analytics implementation details.
- `modules/`: Modularized components for specific app sections:
  - `map_module.R`: Interactive map visualization.
  - `camera_module_v2.R`: Tools for data capture and sharing.
  - `hc_line_module.R`: Highcharts-based time-series visualizations.
  - `about_spp_module.R`: Project information and credits.
- `fx/`: Helper functions (`get_jstree_data.R`, `update_data.R`).
- `www/`: Static web assets (CSS, custom JS for analytics and UI enhancements).
- `data_cache/`: (Generated) Local cache for datasets fetched from GitHub.
- `public_assets/`: (Generated) Local storage for downloaded media/images.
- `renv.lock`: R environment dependency definitions.
- `Dockerfile`: Deployment configuration for Hugging Face (non-root execution).

---

## 🛠 Tech Stack

- **Language**: R (Shiny)
- **Charts**: Highcharts (`highcharter`)
- **Maps**: Leaflet
- **Containerization**: Docker (based on `rocker/shiny`)
- **Dependency Management**: `renv`
- **Analytics**: Google Analytics (Custom iframe bridge)

---

## 📡 Data Flow

1. The app starts via `app.R` / `global.R`.
2. It checks for local cached data in `data_cache/`.
3. If missing or outdated, it fetches raw Excel/CSV data from the [spp-data](https://github.com/fsotoj/spp-data) repository (managed by `fsotoj`).
4. Data is processed, joined, and served to the reactive modules.

---

## 📊 Analytics Architecture

Tracking within an iframe is challenging due to browser privacy restrictions. This project solves this by:
1. Intercepting `gtag` calls within the Shiny app.
2. Sending a `postMessage` to the parent window (`subnationalpolitics.com`).
3. The parent site processes the event, ensuring consistent session tracking across the site and the embedded app.