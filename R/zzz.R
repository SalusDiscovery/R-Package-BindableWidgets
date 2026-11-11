#' Package Initialization and Resource Registration
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Register the inst/www directory as an asset resource path for Shiny
  shiny::addResourcePath(
    prefix = "bindableWidgets",
    directoryPath = system.file("www", package = "BindableWidgets")
  )
}

