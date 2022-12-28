#' Create or retrieve the root data directory
#' 
#' First search for ~/.marineregions which may contain the desired path
#' If absent search for rappdirs::user_data_dir("marineregions")
#' If absent search for rappdirs::site_data_dir("marineregions")
#' If absent create the path specified by new_dir argument
#' 
#' If you want to specify a non-rappdirs standard path, then create a
#' ~/mareg file with one line specifying the desired data path. You must
#' create this path yourself.
#' 
#' @export
#' @param new_dir char, used to create a new directory the first time this is
#'   run
#' @return the root data path
mareg_root <- function(new_dir = rappdirs::user_data_dir("marineregions")){
  if (file.exists("~/.marineregions")){
    root <- readLines("~/.marineregions")
  } else if(dir.exists(rappdirs::user_data_dir("marineregions"))){
    root <- rappdirs::user_data_dir("marineregions")
  } else if (dir.exists(rappdirs::site_data_dir("marineregions"))){
    root <- rappdirs::site_data_dir("marineregions")
  } else {
    ok <- dir.create(new_dir[1], recursive = TRUE)
    root <- new_dir[1]
  }
  root
}

#' Retrieve the mareg data path
#' 
#' @export
#' @param ... char, path segments
#' @param root char, the root directory specification
#' @param create logical, if TRUE create the path if it doesn't exist
#' @return path specification, possibly untested for existence
mareg_path <- function(..., 
                          root = mareg_root(), 
                          create = FALSE){
  path <- file.path(root[1], ...)
  if (create[1]){
    if (!dir.exists(path[1])) ok <- dir.create(path[1], recursive = TRUE)
  }
  path
}


#' List files/directories in the mareg root directory
#' 
#' This is a wrapper around \code{\link[base]{list.files}}
#' @export
#' @param path char, the path to list
#' @param ... other arguments for \code{\link[base]{list.files}}
#' @return character vector
list_mareg <- function(path = mareg_path(), ...){
  list.files(path, ...)
}
