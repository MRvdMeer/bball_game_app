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
nbb_link <- "https://www.basketball.nl/basketball/starten-met-basketball/vereniging-zoeken/club/463/team/381/bc-schrobbelaar-mse-3/competition/4988/"

# old link: "https://www.basketball.nl/basketball/starten-met-basketball/vereniging-zoeken/club/463/team/381/bc-schrobbelaar-mse-3/competition/409/"

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
N_teams <- length(unique_teams)

team_id <- map_int(c(game_table$away_team, game_table$home_team), lookup_team_id, mapping_table = team_mapping_table)
game_table <- game_table %>% mutate(away_team_id = team_id[1:N_games], home_team_id = team_id[(N_games + 1):(2 * N_games)]) %>%
                             select(home_team_id, home_team, home_score, away_team_id, away_team, away_score, home_win, played)


rm(list = c("nbb_link", "schrob", "away_xml", "home_xml", "away", "home", "team_id"))

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
logit_model <- stan_model("NBA_logit_homecourt.stan")

fit_logit <- sampling(logit_model, data = stan_data, chains = 4, iter = 10000)

# Extract and use output
x <- summary(fit_logit, pars = c("team_skill", "home_court_advantage"))$summary[,1]
skill_est <- x[1:N_teams]
hc_est <- x[N_teams + 1]
skill_table <- tibble(id = 1L:11L, team_name = unique_teams, estimated_skill = skill_est)
N_games_unplayed <- nrow(games_not_played)

post <- extract(fit_logit, pars = c("team_skill", "home_court_advantage"), permuted = TRUE)

home_win_prob <- numeric(N_games_unplayed)
for (n in 1:N_games_unplayed) {
    home_win_prob[n] <- mean(
        inv_logit(
            post$team_skill[, games_not_played$home_team_id[n]] + post$home_court_advantage -
                post$team_skill[, games_not_played$away_team_id[n]]
            )
    )
}

games_not_played <- games_not_played %>% select(home_team, away_team) %>% mutate(home_win_prob = home_win_prob)



# home_win_prob <- numeric(N_games_unplayed)
# for (n in 1:N_games_unplayed) {
#     home_win_prob[n] <- inv_logit( skill_est[games_not_played$home_team_id[n]] + hc_est - skill_est[games_not_played$away_team_id[n]])
# }
# 
# games_not_played <- games_not_played %>% select(home_team, away_team) %>% mutate(home_win_prob = home_win_prob)


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
        print_stanfit_custom_name(fit_logit,
                                  regpattern = "team_skill", replace_by = unique_teams,
                                  pars = c("team_skill", "home_court_advantage"))
        skill_table %>% arrange(desc(estimated_skill))
    })
    
    output$plot <- renderPlot({
        #stan_plot(fit_logit, pars = c("team_skill", "home_court_advantage"))
        tmp <- team_mapping_table %>% filter(team_name == input$team)
        id <- tmp$team_id
        plotdata <- data.frame(skill = post$team_skill[, id])
        
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
