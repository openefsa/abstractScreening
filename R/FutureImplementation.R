## NOTE:
# - Atm probably too many packages in Description and runScreening.R
# Delete packages which you do NOT use directlty. (Since the other packages depend on it anyway)
# Also remove these from library call in script.R!
# - Remove :: calls




## At the moment the shiny files are in inst/ShinyApp.
## While this works, people could adjust the R code in the package installation and change the app.
## The way it is done here, includes the R code in the package installation itself.
## Possibility: Implement FINAL version like this


# my_server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     my_plot(input$obs)
#   })
# }
#
# # Needs to be a function so nested dependencies are lazily loaded.
# my_ui <- function(){fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
#     ),
#     mainPanel(plotOutput("distPlot"))
#   )
# )}


# my_app = function(){
#   shinyApp(ui = my_ui(), server = my_server)
# }
