## Importing libraries and data
library(tidyverse)
library(dplyr)
library(vroom)
library(lubridate)

library(ggthemes)
library(plotly)
library(scales)

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)

options(spinner.type = 8)

balls <- vroom('data.csv', delim = ',') %>% mutate(date = dmy(date))

## Defining themes and colors

theme_ipl <- function() {
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = 'none',
          axis.title = element_text(face = 'bold'),
          axis.text = element_text(face = 'bold'))
}

team_colors <- c('#F76F0B', '#F12D35', '#20B8DF', '#C64539', '#39215C', 
                 '#B62424', '#1C5FAA', '#F5E01E', '#FE83A4', '#B5B5B3', 
                 '#7F3F98', '#6FA657', '#CC7834', '#1C5FAA')

names(team_colors) <- c('Sunrisers Hyderabad', 'Royal Challengers Bangalore','Mumbai Indians',
                        'Gujarat Lions', 'Kolkata Knight Riders', 'Kings XI Punjab',
                        'Delhi Daredevils', 'Chennai Super Kings', 'Rajasthan Royals', 
                        'Deccan Chargers', 'Kochi Tuskers Kerala', 'Pune Warriors', 
                        'Rising Pune Supergiants', 'Delhi Capitals')

batsman_list <- balls %>% select(batsman) %>% unique()
bowler_list <- balls %>% select(bowler) %>% unique()

teams <- balls %>% select(team1) %>% unique() %>% rename('team' = team1) %>% arrange(team)


## Plotting functions start here

### 1. Batsman
#### 1.1. Runs per season 
get_batsman_runs <- function(batsman_name) {
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        group_by(season, batting_team) %>%
        summarise(batsman_runs = sum(batsman_runs)) %>%
        ggplot(aes(x = season, y = batsman_runs, fill = batting_team,
                   text = paste0('Season: ', season, '\nRuns: ', batsman_runs, '\nTeam: ', batting_team))) +
        geom_col() +
        scale_fill_manual(values = team_colors) +
        scale_x_continuous(breaks = breaks_width(1)) +
        labs(x = 'Season', y = 'Runs scored', title = paste('Runs scored by', batsman_name)) +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl()
    
    
    plot <- ggplotly(batsman_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

#### 1.2. Distribution of runs
get_batsman_distribution_runs <- function(batsman_name) {
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        select(batsman_runs) %>%
        group_by(batsman_runs) %>%
        summarise(count = n()) %>%
        mutate(batsman = batsman_name, percent = round(count / sum(count), 4)) %>%
        mutate(batsman_runs = recode(batsman_runs, 
                                     `0` = 'Dot', `1` = '1 Run', `2` = '2 Runs', `3` = '3 Runs',
                                     `4` = 'Four', `5` = '5 Runs', `6` = 'Six')) %>%
        mutate(batsman_runs = fct_rev(fct_relevel(batsman_runs, 
                                                  c('Dot', '1 Run', '2 Runs', '3 Runs', 'Four', '5 Runs', 'Six')))) %>%
        ggplot(aes(x = batsman, y = percent, fill = batsman_runs, text = paste0(percent*100, '%'))) +
        geom_col() +
        scale_fill_tableau() +
        scale_y_continuous(labels = label_percent()) +
        labs(x = "", y = "% of Runs", title = paste('Distribution of runs for', batsman_name), fill = '') +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl() +
        theme(legend.position = 'right')
    
    plot <- ggplotly(batsman_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

#### 1.3. Strike rate per season
get_batsman_strike_rate <- function(batsman_name) {
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        group_by(season, batting_team) %>%
        filter(extra_runs == 0) %>%
        summarise(runs = sum(batsman_runs), balls = length(ball)) %>%
        mutate(strike_rate = round((runs/balls) * 100, 2)) %>%
        ggplot(aes(x = season, y = strike_rate, color = batting_team, group = 1,
                   text = paste0('Season: ', season, 
                                 '\nStrike Rate: ', strike_rate,
                                 '\nTeam: ', batting_team))) +
        geom_point(size = 3) +
        geom_step(direction = 'vh') +
        scale_x_continuous(breaks = breaks_width(1)) +
        scale_y_continuous(breaks = breaks_pretty(10)) +
        scale_color_manual(values = team_colors) +
        labs(x = 'Season', y = 'Strike Rate', colour = 'Team',
             title = paste("Strike Rate of", batsman_name)) +
        theme_ipl() +
        guides(size = F)
    
    plot <- ggplotly(batsman_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

#### 1.4. Favorite venues
get_favorite_venues <- function(batsman_name) {
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        select(venue, batsman_runs) %>%
        group_by(venue) %>%
        summarise(batsman_runs = sum(batsman_runs)) %>%
        ungroup() %>%
        arrange(desc(batsman_runs))
    
    return(batsman_data)
}

#### 1.5. Favorite bowlers
get_favorite_bowlers <- function(batsman_name) {
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        select(bowler, batsman_runs) %>%
        group_by(bowler) %>%
        summarise(batsman_runs = sum(batsman_runs)) %>%
        ungroup() %>%
        arrange(desc(batsman_runs))
    
    return(batsman_data)
}

#### 1.6. Runs against teams
get_runs_against_teams <- function(batsman_name) {
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        select(bowling_team, batsman_runs) %>%
        group_by(bowling_team) %>%
        summarise(batsman_runs = sum(batsman_runs)) %>%
        ungroup() %>%
        arrange(desc(batsman_runs)) %>%
        mutate(bowling_team = fct_reorder(bowling_team, batsman_runs, max)) %>%
        ggplot(aes(x = bowling_team, y = batsman_runs, fill = bowling_team, 
                   text = paste0(bowling_team, ', ', batsman_runs))) +
        geom_col() +
        scale_fill_manual(values = team_colors) +
        labs(x = 'Team', y = 'Runs scored', title = paste('Runs scored against teams by', batsman_name)) +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl() +
        coord_flip()
    
    plot <- ggplotly(batsman_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

#### 1.7. Runs per over
get_runs_per_over <- function(batsman_name){
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        select(over, batsman_runs)
    
    individual_overs <- batsman_data %>%
        group_by(over) %>%
        summarise(batsman_runs = sum(batsman_runs)) %>%
        ggplot(aes(x = over, y = batsman_runs, text = paste0("Over: ", over, "\nRuns: ", batsman_runs))) +
        geom_col(fill = "#247da2") +
        labs(x = "Over", y = "Runs scored", title = paste("Runs per over of", batsman_name)) +
        scale_x_continuous(breaks = seq(0, 20, 1), limits = c(-1, 21)) +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl()
    
    plot <- ggplotly(individual_overs, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}
    
get_runs_per_over_grouped <- function(batsman_name){
    batsman_data <- balls %>%
        filter(batsman == batsman_name) %>%
        select(over, batsman_runs)
    
    grouped_overs <- batsman_data %>%
        mutate(over_group = case_when(over >=0 & over <= 6 ~ "0-6",
                                      over >= 7 & over <= 11 ~ "7-11",
                                      over >= 12 & over <= 15 ~ "12-15",
                                      over >= 16 & over <= 20 ~ "16-20"
        )) %>%
        select(-over) %>%
        rename("over" = over_group) %>%
        group_by(over) %>%
        summarise(batsman_runs = sum(batsman_runs)) %>%
        mutate(over = fct_relevel(over, c('0-6', '7-11', '12-15', '16-20'))) %>%
        ggplot(aes(x = over, y = batsman_runs, text = paste0("Over: ", over, "\nRuns: ", batsman_runs))) +
        geom_col(fill = "#ff7300") +
        labs(x = "Over", y = "Runs scored") +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl()
    
    plot <- ggplotly(grouped_overs, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

### 2. Bowler
#### 2.1. Runs conceded per season
get_bowler_runs <- function(bowler_name) {
    bowler_data <- balls %>%
        filter(bowler == bowler_name) %>%
        group_by(season, bowling_team) %>%
        summarise(total_runs = sum(total_runs)) %>%
        ggplot(aes(x = season, y = total_runs, fill = bowling_team, 
                   text = paste0('Season: ', season, '\nRuns: ', total_runs, '\nTeam: ', bowling_team))) +
        geom_col() +
        scale_fill_manual(values = team_colors) +
        scale_x_continuous(breaks = breaks_width(1)) +
        labs(x = 'Season', y = 'Runs conceded', title = paste('Runs concded by', bowler_name)) +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl()
    
    
    plot <- ggplotly(bowler_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

#### 2.2. Economy rate per season
get_economy_rate <- function(bowler_name) {
    bowler_data <- balls %>%
        filter(bowler == bowler_name) %>%
        select(season, over, ball, total_runs, bowling_team) %>%
        group_by(season, bowling_team) %>%
        summarise(total_runs = sum(total_runs), overs_bowled = length(ball)/6) %>%
        mutate(economy_rate = total_runs / overs_bowled) %>%
        ggplot(aes(x = season, y = economy_rate, colour = bowling_team, group = 1,
                   text = paste0('Season: ', season, 
                                 '\nEconomy Rate: ', round(economy_rate, 2), 
                                 '\nTeam: ', bowling_team))) +
        geom_point(aes(size = 0.8)) +
        geom_line(linetype = 'dashed') +
        scale_color_manual(values = team_colors) +
        scale_x_continuous(breaks = breaks_width(1)) +
        labs(x = 'Season', y = 'Economy Rate', title = paste("Economy Rate of", bowler_name)) +
        theme_ipl()
    
    plot <- ggplotly(bowler_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
    
}

#### 2.3. Wickets per season
get_wickets_by_season <- function(bowler_name) {
    bowler_data <- balls %>%
        filter(bowler == bowler_name) %>%
        select(season, dismissal_kind) %>%
        filter(!is.na(dismissal_kind)) %>%
        group_by(season, dismissal_kind) %>%
        summarise(count = n()) %>%
        mutate(dismissal_kind = toupper(dismissal_kind)) %>%
        ggplot(aes(x = season, y = count, fill = dismissal_kind,
                   text = paste0(dismissal_kind, ': ', count))) +
        geom_col() +
        scale_x_continuous(breaks = breaks_width(1)) +
        scale_fill_tableau() +
        labs(x = 'Season', y = 'Number of wickets', title = paste('Distribution of wickets by', bowler_name),
             fill = 'Type of wicket') +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl() +
        scale_fill_tableau()
    
    
    plot <- ggplotly(bowler_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
    
}

#### 2.4. Wickets per over
get_wickets_by_over <- function(bowler_name) {
    bowler_data <- balls %>%
        filter(bowler == bowler_name) %>%
        select(over, dismissal_kind) %>%
        filter(!is.na(dismissal_kind)) %>%
        group_by(over, dismissal_kind) %>%
        summarise(count = n()) %>%
        mutate(dismissal_kind = toupper(dismissal_kind)) %>%
        ggplot(aes(x = over, y = count, fill = dismissal_kind,
                   text = paste0(dismissal_kind, ': ', count))) +
        geom_col() +
        scale_x_continuous(breaks = seq(0, 20, 1), limits = c(-1, 21)) +
        scale_fill_tableau() +
        labs(x = 'Over', y = 'Number of wickets', title = paste('Wickets per over by', bowler_name),
             fill = 'Type of wicket') +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl() +
        scale_fill_tableau()
    
    
    plot <- ggplotly(bowler_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
    
}

#### 2.5. Number of wickets per player
get_wickets_by_batsman <- function(bowler_name) {
    bowler_data <- balls %>%
        filter(bowler == bowler_name) %>%
        select(batsman, dismissal_kind) %>%
        filter(!is.na(dismissal_kind)) %>%
        group_by(batsman) %>%
        summarise(wickets = n()) %>%
        arrange(desc(wickets))
    
    return(bowler_data)
    
}

### 3. Batsman vs. Bowler
#### 3.1. Strike Rate
get_strike_rate_batsman_bowler <- function(batsman_name, bowler_name) {
    players_data <- balls %>%
        filter(batsman == batsman_name & bowler == bowler_name) %>%
        select(season, over, ball, batsman_runs) %>%
        group_by(season) %>%
        summarise(batsman_runs = sum(batsman_runs), balls = length(ball)) %>%
        mutate(strike_rate = round((batsman_runs / balls) * 100, 2)) %>%
        ggplot(aes(x = season, y = strike_rate, group = 1,
                   text = paste0('Season: ', season, 
                                 '\nStrike Rate: ', strike_rate))) +
        geom_point(aes(size = 0.8, colour = "#ff7300")) +
        geom_line(linetype = 'dashed', colour = "#ff7300") +
        scale_x_continuous(breaks = breaks_width(1)) +
        labs(x = 'Season', y = 'Strike Rate', 
             title = paste("Strike Rate of", batsman_name, "against", bowler_name, "by season")) +
        theme_ipl()
    
    plot <- ggplotly(players_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

#### 3.2. Distribution of wickets
get_wickets_batsman_bowler <- function(batsman_name, bowler_name) {
    players_data <- balls %>%
        filter(batsman == batsman_name & bowler == bowler_name) %>%
        select(season, dismissal_kind) %>%
        filter(!is.na(dismissal_kind)) %>%
        group_by(season, dismissal_kind) %>%
        summarise(wickets = n()) %>% 
        mutate(dismissal_kind = toupper(dismissal_kind)) %>%
        ggplot(aes(x = season, y = wickets, fill = dismissal_kind,
                   text = paste0("Season: ", season, "\n", dismissal_kind, ": ", wickets))) +
        geom_col() +
        scale_x_continuous(breaks = breaks_width(1)) +
        labs(x = 'Season', y = 'Number of wickets', fill = 'Type of wicket',
             title = paste("How", bowler_name, "takes wickets of", batsman_name)) +
        geom_hline(yintercept = 0, colour = "#000000") +
        theme_ipl() +
        theme(legend.position = 'right') +
        scale_fill_tableau() +
        coord_flip() 
    
    plot <- ggplotly(players_data, tooltip = c('text')) %>% 
        layout(dragmode=FALSE, autosize = TRUE) %>% 
        config(displayModeBar = F)
    
    return(plot)
}

### 4. Season statistics
get_season_stats <- function(season_selected) {
    finals <- balls %>%
        group_by(season) %>%
        filter(date == max(date)) %>%
        select(season, team1, team2, winner) %>%
        distinct() %>% 
        mutate(runnerup = if_else(winner == team1, team2, team1)) %>%
        select(-team1, -team2) %>%
        arrange(season)
    
    orange_cap <- balls %>%
        group_by(season, batsman, batting_team) %>%
        summarise(runs = sum(batsman_runs)) %>%
        arrange(desc(runs)) %>%
        group_by(season) %>%
        slice(1) %>%
        rename('orange_cap' = batsman, 'orange_cap_team' = batting_team, 'orange_cap_runs' = runs)
    
    purple_cap <- balls %>%
        group_by(season, bowler, bowling_team) %>%
        filter(!dismissal_kind == 'run out') %>%
        summarise(wickets = sum(!is.na(dismissal_kind))) %>%
        arrange(desc(wickets)) %>%
        group_by(season) %>%
        slice(1) %>%
        rename('purple_cap' = bowler, 'purple_cap_team' = bowling_team, 'purple_cap_wickets' = wickets)
    
    most_sixes <- balls %>%
        group_by(season, batsman, batting_team) %>%
        summarise(sixes = sum(batsman_runs == 6)) %>%
        arrange(desc(sixes)) %>%
        group_by(season) %>%
        slice(1) %>%
        rename('most_sixes' = batsman, 'most_sixes_team' = batting_team)
    
    most_fours <- balls %>%
        group_by(season, batsman, batting_team) %>%
        summarise(fours = sum(batsman_runs == 4)) %>%
        arrange(desc(fours)) %>%
        group_by(season) %>%
        slice(1) %>%
        rename('most_fours' = batsman, 'most_fours_team' = batting_team)
    
    best_economy <- balls %>%
        group_by(season, bowler, bowling_team) %>%
        summarise(runs = sum(total_runs), overs = floor(length(ball) / 6)) %>%
        mutate(economy = round(runs / overs, 2)) %>%
        arrange(economy) %>%
        group_by(season) %>%
        slice(1) %>%
        select(-runs, -overs) %>%
        rename('best_economy' = bowler, 'best_economy_team' = bowling_team)
    
    most_dots <- balls %>%
        group_by(season, bowler, bowling_team) %>%
        summarise(dots = sum(total_runs == 0)) %>%
        arrange(desc(dots)) %>%
        group_by(season) %>%
        slice(1) %>%
        rename('most_dots' = bowler, 'most_dots_team' = bowling_team)
    
    season_stats <- finals %>%
        left_join(orange_cap, by = 'season') %>%
        left_join(purple_cap, by = 'season') %>%
        left_join(most_sixes, by = 'season') %>%
        left_join(most_fours, by = 'season') %>%
        left_join(best_economy, by = 'season') %>%
        left_join(most_dots, by = 'season')
    
    return(season_stats %>% filter(season == season_selected))
}

### 5. Head-to-head
team_head_to_head <- function(team_1, team_2) {
    h2h_data <- balls %>%
        filter(team1 %in% c(team_1, team_2) & team2 %in% c(team_1, team_2)) %>%
        select(season:venue) %>%
        distinct()
}

#### 5.1. Head-to-head winners

h2h_stats <- function(team_data) {
    
    winner_count <- team_data %>%
        group_by(winner) %>%
        summarise(count = n())
    
    winner_count <- winner_count %>%
        add_row(winner = 'Total', count = sum(winner_count$count))
    
    return(winner_count)
    
}

get_head_to_head_history <- function(team_data, team_1, team_2) {
    h2h_data <- team_data %>%
        mutate(position = case_when(winner == team_1 ~ 1,
                                    winner == team_2 ~ -1,
                                    TRUE ~ 0)) %>%
        mutate(win_by = case_when(win_by_runs > 0 ~ win_by_runs,
                                  win_by_wickets > 0 ~ win_by_wickets,
                                  TRUE ~ 0)) %>%
        mutate(position_win = position * win_by, how_win = case_when(win_by_runs > 0 ~ paste0('Won by ', win_by_runs, ' runs'),
                                                                     win_by_wickets > 0 ~ paste0('Won by ', win_by_wickets, ' wickets'),
                                                                     result == 'tie' ~ paste0('Match tied'),
                                                                     TRUE ~ 'No result')) %>%
        mutate(plot_text = paste0("<b>Date:</b> ", day(date), " ", month(date, label = T), " ", year(date),
                                  "\n<b>Winner</b>: ", winner,
                                  "\n<b>Margin</b>: ", how_win,
                                  "\n<b>POTM</b>: ", player_of_match,
                                  "\n<b>Venue</b>: ", venue))
    
    h2h_plot <- ggplot(h2h_data, aes(x = date, y = position_win, fill = winner, text = plot_text)) +
        geom_segment(data = h2h_data, aes(y = position_win, yend = 0, xend = date, color = winner)) +
        geom_point(aes(size = win_by), alpha = 0.75) +
        scale_fill_manual(values = team_colors) +
        scale_color_manual(values = team_colors) +
        scale_x_date(breaks = date_breaks('1 year'), labels = label_date('%Y')) +
        theme_ipl() +
        geom_segment(x = 0, xend = (h2h_data %>% slice(n()) %>% select(date) %>% as.integer()), y = 0, yend = 0, 
                     size = 0.05, linetype = 'dotted') +
        coord_flip() +
        labs(x = 'Season', y = "Margin", title = 'Match history') +
        theme(axis.text.x = element_blank())
    
    
    plot <- ggplotly(h2h_plot, tooltip = 'text') %>% 
        layout(autosize = T, dragmode = 'zoom') %>% 
        config(displayModeBar = F)
    
    return(plot)
    
}

#### 5.2. Toss statistics

get_toss_stats <- function(team_data, team_1, team_2) {
    toss_stats <- team_data %>%
        group_by(toss_winner, toss_decision, winner) %>%
        summarise(count = n()) %>%
        mutate(label = paste('Toss won by:', toss_winner)) %>%
        ggplot(aes(x = str_to_sentence(toss_decision), y = count, fill = winner,
                   text = paste0('Toss winner: ', toss_winner,
                                 '\nToss Decision: ', str_to_sentence(toss_decision),
                                 '\nMatch winner: ', winner))) +
        geom_col(position = position_dodge()) +
        scale_fill_manual(values = team_colors) +
        labs(x = 'Toss Decision', y = 'Number of match wins', 
             title = paste(team_1, 'vs.', team_2, '\nToss statistics')) +
        facet_wrap(~ label) +
        theme_ipl()
    
    plot_toss_stats <- ggplotly(toss_stats, tooltip = 'text') %>%
        layout(dragmode = F, autosize = T, margin = list(t = 125)) %>%
        config(displayModeBar = F)
    
    return(plot_toss_stats)
}

#### 5.3. Venue statistics

get_venue_wins <- function(team_data, team_1, team_2) {
    plot_venues <- team_data %>%
        ggplot(aes(x = fct_rev(venue), fill = winner,
                   text = paste0('Venue: ', venue, '\nWinner: ', winner))) +
        geom_bar(position = position_dodge()) +
        scale_fill_manual(values = team_colors) +
        scale_y_continuous(breaks = breaks_width(1)) +
        coord_flip() +
        labs(x = "Venue", y = "Number of wins", title = paste(team_1, 'vs.', team_2, 'by venue')) +
        theme_ipl()
    
    
    plot_venues <- ggplotly(plot_venues, tooltip = 'text') %>% 
        layout(autosize = T, dragmode = F) %>%
        config(displayModeBar = F)
    
    
    return(plot_venues)
}

### 6. Top batsmen/bowlers

#### 6.1. Top batsmen
get_top_batsmen <- function(){
    batsman_data <- balls %>%
        group_by(batsman) %>%
        filter(extra_runs == 0) %>%
        summarise(runs = sum(batsman_runs), balls = length(ball), strike_rate = round(runs / balls * 100, 2),
                  sixes = sum(batsman_runs == 6), fours = sum(batsman_runs == 4))
    
    highest_score <- balls %>%
        group_by(batsman ,match_id) %>%
        summarise(runs = sum(batsman_runs)) %>%
        mutate(high_score = max(runs)) %>%
        select(batsman, high_score) %>%
        distinct()
    
    batsman_data <- batsman_data %>%
        left_join(highest_score, by = 'batsman') %>%
        arrange(desc(runs))
    
    return(batsman_data)
}


#### 6.2. Top bowlers
get_top_bowlers <- function(){
    bowler_data <- balls %>%
        group_by(bowler) %>%
        summarise(runs = sum(total_runs), balls = length(ball), 
                  wickets = sum(!is.na(dismissal_kind) & !dismissal_kind == 'run out'),
                  strike_rate = round(balls / wickets, 2),
                  economy_rate = round(runs / floor(balls / 6), 2),
                  average = round(runs / wickets, 2))
    
    top_bowlers <- bowler_data %>%
        arrange(desc(wickets))
    
    return(top_bowlers)
    
}


## ui.R

header <- dashboardHeader(
    title = 'IPL Dashboard'
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("About the dashboard", tabName = 'about', icon = icon('info')),
        menuItem("Batsman", tabName = 'batsman', icon = icon('pen')), #1
        menuItem("Bowlers", tabName = 'bowler', icon = icon('baseball-ball')), #2
        menuItem("Batsman vs. Bowler", tabName = 'batsmanvsbowler', icon = icon('user-friends')), #3
        menuItem("Top players", tabName = 'topplayers', icon = icon('medal')), #6
        menuItem("Season statistics", tabName = 'season', icon = icon('calendar')), #4
        menuItem("Head-to-head", tabName = 'headtohead', icon = icon('drum-steelpan')), #5
        menuItem(""),
        menuItem('Created by Lakshya Agarwal', href = 'https://github.com/lakshyaag/', newtab = T, 
                 icon = icon('magic'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem('about', 
                fluidRow(
                    box(title = 'Indian Premier League', width = 6, status = 'primary', solidHeader = T,
                        p('The Indian Premier League is a professional Twenty20 cricket league in India contested during March-May of every year by eight teams representing eight different cities in India.'),
                        p('The IPL is the most-attended cricket league in the world and in 2014 ranked sixth by average attendance among all sports leagues.')
                    ),
                    box(title = 'Analytics Dashboard', width = 6, status = 'success', solidHeader = T,
                        p('An interactive analytics dashboard for IPL which contains data from all seasons till 2019.'),
                        p('You can analyze runs scored by season, dismissals, strike rates by player, toss statistics, season statistics and other insights. If you have any ideas, drop me a message.')
                    )
                ),
                fluidRow(
                    box(htmlOutput('about_image'), width = 12)
                )
        ),
        tabItem('batsman',
                fluidRow(
                    box(width = 12, status = 'success',
                        selectInput('batsman_selected', 'Choose batsman',
                                       choices = batsman_list, selected = 'V Kohli')
                        )
                ),
                
                fluidRow(
                    tabBox(width = 12,
                           tabPanel("Runs per season",
                                    plotlyOutput('runs_per_season') %>% withSpinner()),
                           tabPanel("Distribution of runs",
                                    plotlyOutput('distribution_of_runs') %>% withSpinner()),
                           tabPanel("Strike rate per season",
                                    plotlyOutput('strike_rate_per_season') %>% withSpinner()),
                           tabPanel("Favorite venues (by runs)",
                                    dataTableOutput('fav_venues') %>% withSpinner()),
                           tabPanel("Favorite bowlers",
                                    dataTableOutput('fav_bowlers') %>% withSpinner()),
                           tabPanel("Runs against teams",
                                    plotlyOutput('runs_against_teams') %>% withSpinner()),
                           tabPanel("Runs per over",
                                    plotlyOutput('runs_per_over') %>% withSpinner(),
                                    plotlyOutput('runs_per_over_grouped') %>% withSpinner())
                           )
                )
        ),
        tabItem('bowler',
                fluidRow(
                    box(width = 12, status = 'success',
                        selectInput('bowler_selected', 'Choose bowler',
                                    choices = bowler_list, selected = 'JJ Bumrah')
                    )
                ),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel("Runs conceded per season",
                                    plotlyOutput('runs_conceded_per_season') %>% withSpinner()),
                           tabPanel("Economy rate per season",
                                    plotlyOutput('economy_rate_per_season') %>% withSpinner()),
                           tabPanel('Wickets per season',
                                    plotlyOutput('wickets_per_season') %>% withSpinner()),
                           tabPanel("Wickets per over",
                                    plotlyOutput('wickets_per_over') %>% withSpinner()),
                           tabPanel("Number of wickets by player",
                                    dataTableOutput('wickets_per_player') %>% withSpinner())
                           )
                )
        ),
        tabItem('batsmanvsbowler',
                fluidRow(
                    box(width = 12, status = 'success',
                        column(6,
                               selectInput('batsman_selected_compare', 'Choose batsman',
                                    choices = batsman_list, selected = 'V Kohli')
                        ),
                        column(6,
                               selectInput('bowler_selected_compare', 'Choose bowler',
                                    choices = bowler_list, selected = 'JJ Bumrah')
                               )
                    )
                ),
                fluidRow(
                    tabBox(width = 12,
                           tabPanel('Strike Rate',
                                    plotlyOutput('strike_rate_compare') %>% withSpinner()),
                           tabPanel('Distribution of wickets',
                                    plotlyOutput('dist_wickets_compare') %>% withSpinner())
                           )
                )
        ),
        tabItem('season',
                fluidRow(
                    box(width = 2, status = 'success',
                        selectInput('season_selected', 'Select season',
                                    choices = seq(2008, 2019, 1), selected = 2019)
                    ),
                    
                    box(width = 10, status = 'primary', title = 'Season statistics',
                        valueBoxOutput('winner_box', width = 6),
                        valueBoxOutput('runnerup_box', width = 6),
                        valueBoxOutput('orangecap_box', width = 6),
                        valueBoxOutput('purplecap_box', width = 6),
                        valueBoxOutput('most_sixes_box', width = 6),
                        valueBoxOutput('most_fours_box', width = 6),
                        valueBoxOutput('best_economy_box', width = 6),
                        valueBoxOutput('most_dots_box', width = 6)
                    )
                )
        ),
        tabItem('headtohead',
                fluidRow(
                    box(width = 12, status = 'success',
                        column(6,
                               selectInput('team_1', 'Choose a team',
                                           choices = teams, selected = 'Chennai Super Kings')
                        ),
                        column(6,
                               selectInput('team_2', 'Choose a team',
                                           choices = teams, selected = 'Mumbai Indians')
                        )
                    )
                ),
                tabBox(width = 12,
                       tabPanel('Head-to-head wins',
                                fluidRow(
                                    valueBoxOutput('team1_wins', width = 4),
                                    valueBoxOutput('total_matches', width = 4),
                                    valueBoxOutput('team2_wins', width = 4),
                                    
                                    box(width = 12, solidHeader = T, status = 'success', collapsible = T,
                                        plotlyOutput('h2h_history') %>% withSpinner()
                                    )
                                )
                        ),
                       tabPanel('Toss statistics',
                                plotlyOutput('toss_teams') %>% withSpinner()),
                       tabPanel('Venue statistics',
                                plotlyOutput('venue_teams') %>% withSpinner())
                       )
        ),
        tabItem('topplayers',
                fluidRow(
                    tabBox(width = 12,
                           tabPanel('Top batsmen',
                                    dataTableOutput('top_batsmen') %>% withSpinner()),
                           tabPanel('Top bowlers',
                                    dataTableOutput('top_bowlers') %>% withSpinner())
                           )
                )
        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin = 'black')

## server.R

server <- function(input, output) {
    
    output$about_image <- renderUI({
        img(src = 'https://iplt20scores.com/wp-content/uploads/2020/01/IPL-logo-4.jpg',
             width = '100%',
             height = 'auto'
             )
    })
    
#### 1. Batsman
    
    output$runs_per_season <- renderPlotly({
        get_batsman_runs(input$batsman_selected)
    })
    
    output$distribution_of_runs <- renderPlotly({
        get_batsman_distribution_runs(input$batsman_selected)
    })
    
    output$strike_rate_per_season <- renderPlotly({
        get_batsman_strike_rate(input$batsman_selected)
    })
    
    output$fav_venues <- renderDataTable({get_favorite_venues(input$batsman_selected)},
                                             selection = 'none',
                                             colnames = c('Venue', "Runs scored"),
                                             rownames = F,
                                             options = list(dom = 'tp'),
                                             style = 'bootstrap')
    
    output$fav_bowlers <- renderDataTable({get_favorite_bowlers(input$batsman_selected)},
                                              selection = 'none',
                                              colnames = c('Venue', "Runs scored"),
                                              rownames = F,
                                              options = list(dom = 'tp'),
                                              style = 'bootstrap')
    
    output$runs_against_teams <- renderPlotly({
        get_runs_against_teams(input$batsman_selected)
    })
    
    output$runs_per_over <- renderPlotly({
        get_runs_per_over(input$batsman_selected)
    })
    
    output$runs_per_over_grouped <- renderPlotly({
        get_runs_per_over_grouped(input$batsman_selected)
    })
    
#### 2. Bowler
    output$runs_conceded_per_season <- renderPlotly({
        get_bowler_runs(input$bowler_selected)
    })
    
    output$economy_rate_per_season <- renderPlotly({
        get_economy_rate(input$bowler_selected)
    })
    
    output$wickets_per_season <- renderPlotly({
        get_wickets_by_season(input$bowler_selected)
    })
    
    output$wickets_per_over <- renderPlotly({
        get_wickets_by_over(input$bowler_selected)
    })
    
    output$wickets_per_player <- renderDataTable({get_wickets_by_batsman(input$bowler_selected)},
                                                     selection = 'none',
                                                     colnames = c('Venue', "Wickets taken"),
                                                     rownames = F,
                                                     options = list(dom = 'tp'),
                                                     style = 'bootstrap')
    
#### 3. Batsman vs. Bowler
    
    output$strike_rate_compare <- renderPlotly({
        get_strike_rate_batsman_bowler(input$batsman_selected_compare, input$bowler_selected_compare)
    })
    
    output$dist_wickets_compare <- renderPlotly({
        get_wickets_batsman_bowler(input$batsman_selected_compare, input$bowler_selected_compare)
    })
    
#### 4. Season statistics
    
    season_data <- reactive({get_season_stats(input$season_selected)})
    
    output$winner_box <- renderValueBox({
        valueBox(
            season_data()$winner,
            "Winner",
            color = "green",
            icon = icon('trophy')
        )
    })
    
    output$runnerup_box <- renderValueBox({
        valueBox(
            season_data()$runnerup,
            "Runner Up",
            color = "red",
            icon = icon('shield-alt')
        )
    })
    
    output$orangecap_box <- renderValueBox({
        valueBox(
            paste0(season_data()$orange_cap, ' (', season_data()$orange_cap_runs, ')'),
            "Orange Cap",
            color = "orange",
            icon = icon('pen')
        )
    })
    
    output$purplecap_box <- renderValueBox({
        valueBox(
            paste0(season_data()$purple_cap, ' (', season_data()$purple_cap_wickets, ')'),
            "Purple Cap",
            color = "purple",
            icon = icon('baseball-ball')
        )
    })
    
    output$most_sixes_box <- renderValueBox({
        valueBox(
            paste0(season_data()$most_sixes, ' (', season_data()$sixes, ')'),
            "Most sixes",
            color = "aqua"
        )
    })
    
    output$most_fours_box <- renderValueBox({
        valueBox(
            paste0(season_data()$most_fours, '(', season_data()$fours, ')'),
            "Most fours",
            color = "navy"
        )
    })
    
    output$best_economy_box <- renderValueBox({
        valueBox(
            paste0(season_data()$best_economy, ' (', season_data()$economy, ')'),
            "Best economy",
            color = "teal"
        )
    })
    
    output$most_dots_box <- renderValueBox({
        valueBox(
            paste0(season_data()$most_dots, ' (', season_data()$dots, ')'),
            "Most dots",
            color = "olive"
        )
    })
    
#### 5. Head-to-head
    
    team_head_to_head_data <- reactive({team_head_to_head(input$team_1, input$team_2)})
    
    h2h_stats_data <- reactive({h2h_stats(team_head_to_head_data())})
    
    output$team1_wins <- renderValueBox({
        valueBox(
            paste0(h2h_stats_data() %>% filter(winner == input$team_1) %>% select(count)),
            paste0(input$team_1, ' Wins'),
            color = 'olive'
        )
    })
    
    output$total_matches <- renderValueBox({
        valueBox(
            paste0(h2h_stats_data() %>% filter(winner == 'Total') %>% select(count)),
            paste0('Total matches'),
            color = 'aqua',
            icon = icon('drum-steelpan')
        )
    })
    
    output$team2_wins <- renderValueBox({
        valueBox(
            paste0(h2h_stats_data() %>% filter(winner == input$team_2) %>% select(count)),
            paste0(input$team_2, ' Wins'),
            color = 'fuchsia'
        )
    })
    
    output$h2h_history <- renderPlotly({
        get_head_to_head_history(team_head_to_head_data(), input$team_1, input$team_2)
    })
    
    output$toss_teams <- renderPlotly({
        get_toss_stats(team_head_to_head_data(), input$team_1, input$team_2)
    })
    
    output$venue_teams <- renderPlotly({
        get_venue_wins(team_head_to_head_data(), input$team_1, input$team_2)
    })
    
#### 6. Top batsmen/bowlers
    
    output$top_batsmen <- renderDataTable({get_top_batsmen()},
                                          selection = 'none',
                                          colnames = c('Batsman', "Runs", "Balls", 
                                                       "Strike Rate", "6s", "4s", "Highest score"),
                                          rownames = F,
                                          options = list(dom = 'tp'),
                                          style = 'bootstrap')
    
    output$top_bowlers <- renderDataTable({get_top_bowlers()},
                                          selection = 'none',
                                          colnames = c('Bowler', "Runs", "Balls", 
                                                       "Wickets", "Strike Rate", "Economy Rate", "Average"),
                                          rownames = F,
                                          options = list(dom = 'tp'),
                                          style = 'bootstrap')
    
}

shinyApp(ui = ui, server = server)