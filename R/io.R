#' Read EEZ geography
#' 
#' @export
#' @param version char, by default "v11"
#' @param res char, one of "low" (default) or "high"
#' @param boundaries logical, by default FALSE to read all of the data. This is 
#' only meaningful for high resolution.
#' @return sf table
read_eez <- function(version = "v11", 
                     res = c("high", "low")[2],
                     boundaries = FALSE){
  
  
  if (tolower(res[1]) == "high"){
    path = switch(tolower(version[1]),
                  "v11" = mareg_path("World_EEZ_v11_20191118_gpkg"),
                  stop("version not known:", version[1]))
    files <- list.files(path, pattern = "^.*\\.gpkg$")
    filename <- files[grep("boundaries", files, fixed = TRUE, invert = !boundaries)]
  } else {
    path = switch(tolower(version[1]),
                  "v11" = mareg_path("World_EEZ_v11_20191118_LR"),
                  stop("version not known:", version[1]))
    filename <- list.files(path, pattern = "^.*\\.gpkg$")
  }
  
  if (length(filename) == 0) stop("file not found")
  sf::read_sf(file.path(path, filename[1]))
}