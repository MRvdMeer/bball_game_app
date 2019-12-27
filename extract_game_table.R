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