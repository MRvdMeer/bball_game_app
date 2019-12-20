#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Set up User Interface
ui <- fluidPage(
    dateInput("date", label = "Game Date"),
    actionButton("click", "Update Predictions"),
    textOutput("game_list"),
    textOutput("predictions")
)

# Define server logic
server <- function(input, output, session) {
    game_date <- reactive( input$date )
    
    x1 <- eventReactive(input$click, {
        "No predictions to show"
    })
    
    output$game_list <- renderText({
        paste( "No games on date", game_date() )
    })
    
    output$predictions <- renderText({
        x1()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
