ifpack <- function(packages) {
  if (is.character(packages)) {
    packages <- setNames(as.list(rep("", length(packages))), packages)
  }
  
  for (package_name in names(packages)) {
    repo <- packages[[package_name]]
    
    if (!requireNamespace(package_name, quietly = TRUE) || !is.null(repo) && repo != "") {
      
      if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
      
      if (!is.null(repo) && repo != "") {
        message(paste("Installing", package_name, "from GitHub:", repo, "..."))
        remotes::install_github(repo, dependencies = TRUE, force = TRUE)
      } else {
        message(paste("Installing", package_name, "from CRAN..."))
        install.packages(package_name, dependencies = TRUE)
      }
      
    } else {
      message(paste("Package", package_name, "is already installed."))
    }
    
    suppressPackageStartupMessages(library(package_name, character.only = TRUE))
    message(paste("Loaded package:", package_name))
  }
}