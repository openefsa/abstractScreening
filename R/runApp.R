#' Run the shiny application
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @importFrom shiny runApp
#' @importFrom stats update
#' @importFrom devtools dev_package_deps
#' @export
runShiny <- function(installDependencies = FALSE, ...) {

  # (1) Install all suggested R packages (see DESCRIPTION)
  if (installDependencies) {

    ## (a) CRAN packages
    update(dev_package_deps(pkg = system.file("ui", package = "AbstractScreening"),
            dependencies = "Suggests"))


#    ## (b) non-CRAN packages - by hand
#    if (!requireNamespace("shiny.collections")) {
#
#      install_github("Appsilon/shiny.collections")
#
#    }

  }

  # (2) Run the application
  ldots <- list(...)

  if (!is.null(ldots$appDir))
    shiny::runApp(...) else
    shiny::runApp(appDir = system.file("ShinyApp", package = "AbstractScreening"), ...)

}
