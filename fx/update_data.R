library(httr)

# Settings
owner <- "fsotoj"; repo <- "spp-data"
folders <- c("data", "public") # Folders to download

for (f in folders) {
  local_dir <- if(f == "data") "data_cache" else "public_assets"
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  
  # GitHub API call for each folder
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", f)
  res <- content(GET(url))
  
  for (item in res) {
    if (item$type == "file") {
      download.file(item$download_url, 
                    destfile = file.path(local_dir, item$name), 
                    mode = "wb", quiet = TRUE)
    }
  }
}