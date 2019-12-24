library(tidyverse)
library(rvest)

nbb_link <- "https://www.basketball.nl/basketball/starten-met-basketball/vereniging-zoeken/club/463/team/381/bc-schrobbelaar-mse-3/competition/409/"

schrob <- read_html(nbb_link)

# test <- schrob %>%
#   html_nodes(".c-season-statistics-match-result__away , .c-season-statistics-match-result__home ")
# 
# test[1:10] %>% html_text(trim = TRUE)
# 
# n <- length(test)
# 
# test[(n-5):n] %>% html_text(trim = TRUE)
# 
# test2 <- schrob %>%
#   html_nodes(".m-0")
# 
# test2 %>% html_text(trim = TRUE)


away_xml <- schrob %>%
  html_nodes(".c-season-statistics-match-result__away")

home_xml <- schrob %>%
  html_nodes(".c-season-statistics-match-result__home")

away <- away_xml %>% html_text(trim = TRUE)
home <- home_xml %>% html_text(trim = TRUE)


extract_game_table <- function(home, away) {
  if (length(home) != length(away)) {
    stop("Home and away must be of equal length")
  }
  
  temp_home <- strsplit(home, split = "\n") %>%
    map(grep, pattern = "[^ ]", value = TRUE) %>%
    map(sub, pattern = "[ ]{2, }", replacement = "") %>%
    map(sub, pattern = " \\*", replacement = "")
  
  home_team <- map_chr(temp_home, function(x) x[[2]])
  home_score <- map_chr(temp_home, function(x) x[[3]]) %>% map_dbl(.f = function(x) {if (x == "None") return(NA_real_) else return(as.numeric(x))})
  
  temp_away <- strsplit(away, split = "\n") %>%
    map(grep, pattern = "[^ ]", value = TRUE) %>%  
    map(sub, pattern = "[ ]{2, }", replacement = "") %>%
    map(sub, pattern = " \\*", replacement = "")
  
  away_team <- map_chr(temp_away, function(x) x[[2]])
  away_score <- map_chr(temp_away, function(x) x[[1]]) %>% map_dbl(.f = function(x) {if (x == "None") return(NA_real_) else return(as.numeric(x))})
  
  played <- !is.na(home_score)
  test <- !is.na(away_score)
  
  if (!all(played == test)) stop("mismatch in home and away played")
  
  df <- tibble(home_team = home_team,
               home_score = home_score,
               away_team = away_team,
               away_score = away_score,
               played = played)
  df
}

game_table <- extract_game_table(home = home, away = away)
