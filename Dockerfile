# 1. Base Image
FROM rocker/shiny:4.5.1

# 2. Install System Dependencies
# Optimized to be one single layer
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsodium-dev \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Project Directory
WORKDIR /home/shinyapp

# --- NEW: Fix 1: Environment Variables ---
# We disable the cache and tell renv exactly where to put the library
ENV RENV_CONFIG_CACHE_ENABLED=FALSE
ENV RENV_PATHS_LIBRARY=/home/shinyapp/renv/library

# 4. Restore R Packages (The "Slow" Step)
# We copy only the renv files first. This layer only rebuilds 
# if your renv.lock file changes.
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# --- NEW: Fix 2: Clean Restore ---
# We use 'clean = TRUE' to remove any broken links and start fresh.
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); renv::restore(clean = TRUE)"

# 5. Data Update (The "Cache Breaker")
# To force a fresh data download, change the date below.
# LAST DATA UPDATE: 2026-02-15
COPY fx/update_data.R /home/shinyapp/fx/update_data.R
RUN Rscript /home/shinyapp/fx/update_data.R

# 6. Copy App Code (The "Fast" Step)
# We copy everything else now. Any change to app.R, ui.R, or modules 
# will trigger a rebuild FROM THIS POINT ONLY.
COPY . /home/shinyapp/

# --- NEW: Fix 3: Recursive Permission Check ---
# Very important: ensure the 'shiny' user owns the library we just built
USER root
RUN chown -R shiny:shiny /home/shinyapp
USER shiny

# 7. Permissions Fix for Hugging Face
# HF Spaces run as a non-root user (UID 1000). 
# We ensure that user owns the app files.
RUN chown -R shiny:shiny /home/shinyapp

# 8. Hugging Face Configuration
ENV PORT=7860
EXPOSE 7860

# 9. Start the App
# We run as the 'shiny' user for security and HF compatibility
USER shiny

CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=7860)"]