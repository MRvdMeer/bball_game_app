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
library(rstan)
source("background_functions.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# First load and process the data from NBB.
nbb_link <- "https://www.basketball.nl/basketball/starten-met-basketball/vereniging-zoeken/club/463/team/381/bc-schrobbelaar-mse-3/competition/409/"

schrob <- read_html(nbb_link)

away_xml <- schrob %>%
    html_nodes(".c-season-statistics-match-result__away")

home_xml <- schrob %>%
    html_nodes(".c-season-statistics-match-result__home")

away <- away_xml %>% html_text(trim = TRUE)
home <- home_xml %>% html_text(trim = TRUE)

game_table <- extract_game_table(home = home, away = away)

unique_teams <- sort(unique(c(game_table$away_team, game_table$home_team)))

N_games <- nrow(game_table)
team_mapping_table <- tibble(team_name = unique_teams, team_id = 1:length(unique_teams))
team_id <- map_int(c(game_table$away_team, game_table$home_team), lookup_team_id, mapping_table = team_mapping_table)
game_table <- game_table %>% mutate(away_team_id = team_id[1:N_games], home_team_id = team_id[(N_games + 1):(2 * N_games)]) %>%
                             select(home_team_id, home_team, home_score, away_team_id, away_team, away_score, home_win, played)


rm(list = c("nbb_link", "schrob", "away_xml", "home_xml", "away", "home"))

# Now we prepare the data for inference with Stan
stan_data <- list(N_games <- sum(game_table$played),
                  N_teams <- length(team_mapping_table$team_id),
                  home_win <- game_table$home_score > game_table$away_score,
                  away_team_id <- game_table$away_team_id,
                  home_team_id <- game_table$home_team_id
                  )

logit_model <- stan_model("NBA_logit_homecourt.stan")

fit_logit <- sampling(logit_model, data = stan_data, )


# Set up User Interface
ui <- fluidPage(
    dateInput("date", label = "Select Date"),
    selectInput("team", "Select a team", sort(unique(game_table$home_team)), selected = "BC Schrobbelaar MSE 3"),
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
