#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(rvest)
library(shiny)
source("extract_game_table.R")


nbb_link <- "https://www.basketball.nl/basketball/starten-met-basketball/vereniging-zoeken/club/463/team/381/bc-schrobbelaar-mse-3/competition/409/"

schrob <- read_html(nbb_link)

away_xml <- schrob %>%
    html_nodes(".c-season-statistics-match-result__away")

home_xml <- schrob %>%
    html_nodes(".c-season-statistics-match-result__home")

away <- away_xml %>% html_text(trim = TRUE)
home <- home_xml %>% html_text(trim = TRUE)

game_table <- extract_game_table(home = home, away = away)


# Set up User Interface
ui <- fluidPage(
    dateInput("date", label = "Select Date"),
    selectInput("team", "Select a team", sort(unique(game_table$home_team))),
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
