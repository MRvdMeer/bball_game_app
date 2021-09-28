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
library(cmdstanr)
source("background_functions.R")
options(mc.cores = parallel::detectCores())

# First load and process the data from NBB.
game_table <- readRDS("game_table.rds")

unique_teams <- sort(unique(c(game_table$away_team, game_table$home_team)))

N_games <- nrow(game_table)
team_mapping_table <- tibble(team_name = unique_teams, team_id = 1:length(unique_teams))
N_teams <- length(unique_teams)

# Now we prepare the data for inference with Stan
games_played <- game_table %>% filter(played)
games_not_played <- game_table %>% filter(!played)

stan_data <- list(N_games = nrow(games_played),
                  N_teams = N_teams,
                  home_win = games_played$home_win,
                  away_team_id = games_played$away_team_id,
                  home_team_id = games_played$home_team_id
                  )

# Fit Stan model
logit_model <- cmdstan_model("NBA_logit_homecourt.stan")

fit_logit <- logit_model$sample(data = stan_data,
                                chains = 4,
                                iter_warmup = 5000,
                                iter_sampling = 5000)

# Extract and use output
tmptst <- fit_logit$summary()
tempsummary <- fit_logit$summary(c("team_skill", "home_court_advantage"))
x <- tempsummary$mean
skill_est <- x[1:N_teams]
hc_est <- x[N_teams + 1]
skill_table <- tibble(id = 1L:N_teams, team_name = unique_teams, estimated_skill = skill_est)
N_games_unplayed <- nrow(games_not_played)

post <- fit_logit$draws(variables = c("team_skill", "home_court_advantage"), format = "draws_matrix")

home_win_prob <- numeric(N_games_unplayed)
for (n in 1:N_games_unplayed) {
    home_win_prob[n] <- mean(
        inv_logit(
            post[, games_not_played$home_team_id[n]] + post[, N_teams + 1] -
                post[, games_not_played$away_team_id[n]]
            )
    )
}

games_not_played <- games_not_played %>% select(home_team, away_team) %>% mutate(home_win_prob = home_win_prob)

############# End of pre-computed objects #############



# Set up User Interface
ui <- fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    titlePanel("Basketball game predictions"),
    fluidRow(
        column(5,
            tableOutput("skill_est"),
            textOutput("home_court_est")
        ),
        column(7,
            selectInput("team", "Select a team", sort(unique(game_table$home_team)), selected = "BC Schrobbelaar MSE 3"),
            tableOutput("predictions"),
            textOutput("pred_wins")
        )
    ),
    fluidRow(
        column(5,
               plotOutput("plot")
               ),
        column(7,
               textOutput("previous_games_header"),
               tableOutput("previous_games")
               )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    x1 <- eventReactive(input$team, {
        temp <- games_not_played %>% filter(home_team == input$team | away_team == input$team)
        game_win_prob <- temp$home_win_prob
        for (n in 1:nrow(temp)) {
            if (temp$away_team[n] == input$team) {
                game_win_prob[n] = 1 - game_win_prob[n]
            }
        }
        temp %>% mutate(game_win_prob = game_win_prob) %>% select(-home_win_prob)
    })

    output$predictions <- renderTable({
        x1()
    })
    
    output$pred_wins <- renderText({
        paste(input$team, "is expected to win", round( sum(x1()$game_win_prob), digits = 1 ), "out of the remaining", nrow(x1()), "games.")
    })
    
    output$skill_est <- renderTable({
        print_cmdstan_custom_name(fit_logit,
                                  regpattern = "team_skill",
                                  replace_by = unique_teams,
                                  variables = c("team_skill", "home_court_advantage"))
        skill_table %>% arrange(desc(estimated_skill))
    })
    
    output$plot <- renderPlot({
        #stan_plot(fit_logit, pars = c("team_skill", "home_court_advantage"))
        tmp <- team_mapping_table %>% filter(team_name == input$team)
        id <- tmp$team_id
        plotdata <- data.frame(post[, id])
        colnames(plotdata) <- "skill"
        
        ggplot(data = plotdata, aes(x = skill) ) +
            geom_histogram(fill = "lightblue", col = "black", bins = 30) + theme_bw() +
            geom_vline(xintercept = mean(plotdata$skill), color = "red") +
            ggtitle("Estimated skill") + xlab(paste("Skill distribution for team", input$team) )
    })
    
    output$home_court_est <- renderText( paste("Estimated home court effect:", round(hc_est, digits = 2) ))
    
    output$previous_games_header <- renderText( paste("Previous games for", input$team) )
    
    output$previous_games <- renderTable({
        games_played %>%
            filter(home_team == input$team | away_team == input$team) %>%
            select(home_team, home_score, away_team, away_score) %>%
            mutate(home_score = as.integer(home_score), away_score = as.integer(away_score))
    })

}

# Run the application
shinyApp(ui = ui, server = server)
