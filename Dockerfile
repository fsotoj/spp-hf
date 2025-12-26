# 1. Base Image
FROM rocker/shiny:4.5.1

# 2. Install ALL system dependencies (The "Final Boss" list)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libglpk-dev \
    # Geospatial stack (for terra, sf, etc.)
    gdal-bin \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    # Units and formatting (for units package)
    libudunits2-dev \
    # Cryptography (for sodium)
    libsodium-dev \
    # General data dependencies
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Setup Project Directory
# We use /home/shinyapp to stay away from the default server folders
WORKDIR /home/shinyapp

# 4. Copy renv files first (Crucial for caching)
COPY renv.lock renv.lock
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

# 5. Restore the library
# We use the Posit Package Manager for Debian Bookworm (R 4.5.1 base)
# to download pre-compiled binaries, making the build MUCH faster.
RUN R -e "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/bookworm/latest')); renv::restore()"


# --- NEW STEP 5.5: Pre-populate Data Cache ---
# We copy ONLY the update script first to create a cached data layer.
# Change the date below to force a fresh data download from GitHub.
# LAST DATA UPDATE: 2025-12-26
COPY fx/update_data.R /home/shinyapp/fx/update_data.R
RUN Rscript /home/shinyapp/fx/update_data.R


# --- STEP 6: Optimized Copying ---

# 6a. Copy Data & Modules (Stuff you said changes less often)
# If these folders don't change, Docker skips this layer entirely.
COPY fx/ /home/shinyapp/fx/
COPY modules/ /home/shinyapp/modules/
COPY app.R server.R ui.R global.R /home/shinyapp/
COPY app.R server.R ui.R global.R /home/shinyapp/

# 6c. Copy everything else (README, etc.)
COPY . /home/shinyapp/

# 7. Hugging Face Configuration
# HF expects port 7860. We ignore the default Shiny Server (3838) 
# and launch the app directly.
ENV PORT=7860
EXPOSE 7860

# 8. Start the App
# This ensures R runs YOUR code in the current directory
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=7860)"]