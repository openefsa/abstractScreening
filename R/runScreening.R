
#' @title Shiny App for Abstract Screening
#'
#' @description Shiny App for Abstract Screening using multiple MLT methods.
#'
#' @author Stijn Jaspers
#' @docType package
#' @name AbstractScreening
NULL


# NOTE, the second row appeared when I wanted to install fresh...
#' @import shiny caretEnsemble plyr xlsx tm DT ranger SnowballC wordcloud e1071 kernlab rpart rpart.plot nnet caret RWeka topicmodels pROC DMwR randomForest hmeasure pdftools tabulizer tabulizerjars
#' @import recipes rlang
NULL

#' @title Launch the Shiny App for Abstract Screening
#' @description Shiny App for Abstract Screening using multiple MLT methods.
#' @author Stijn Jaspers
#' @export
#' @examples
#' \dontrun{#'
#' ## FIRST-TIME INSTALLATION ##
#' ## Note: If updating Shiny App, skip to step 4
#' # 1. Install R (>= 3.4.0)
#' # 2. Install Appropriate version of R Tools (https://cran.r-project.org/bin/windows/Rtools/)
#' # 3. Install devtools R Package
#' install.packages("devtools")
#' devtools::install_github("hadley/devtools")
#' # 4. Install Abstract Screening Package (from the directory with source files)
#' devtools::install(".../AbstractScreening")
#' # 5. If you do not have the development version of the caretEnsemble R package, run
#' devtools::install_github('zachmayer/caretEnsemble')
#'
#' ## LAUNCHING THE APP ##
#' library(AbstractScreening)
#' runAbstractScreening() # Note: Only necessary if you need to launch the app again.
#'
#'
#' # REMARK EWOUD:
#' # THE ABOVE INSTRUCTIONS ARE IF WE WANT TO DISTRIBUTE THE PACKAGE THROUGH THE SOURCE FILES
#' # IF THEY WANT NOT INSTALL, BUT JUST A ZIP OF THE BINARIES, WE NEED TO EMPLOY A DIFFERENT
#' # STRATEGY (.onAttach to install dependencies) However caretEnsemble additional install
#' # (due to needing developer version), still applies (because same version!).
#'
#' }
runAbstractScreening <- function() {
  appDir <- system.file("ShinyApp", package = "AbstractScreening")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `AbstractScreening`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
