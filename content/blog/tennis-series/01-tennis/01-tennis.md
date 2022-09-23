---
title: "Wimbledon analysis part 1: get the data"
weight: 1
author: Brendan Graham
date: '2022-02-02'
slug: tennis-part1
categories: 
  - tennis
  - R
tags:
  - tennis
  - R
subtitle: 
summary: 'This post is the first in a series on analyzing Wimbledon tournament data.'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
draft: false
editor_options: 
  chunk_output_type: console
---



## Introduction

This is the first in a series of posts analyzing data from recent Wimbledon tournaments. In this post I will talk through how I retrieve and format the data. the point by point and match data come from this [amazing repo](https://github.com/JeffSackmann/tennis_slam_pointbypoint/) that has point by point data for Wimbledon and many more tournaments.

## Get the data

I want to use several years worth of data. Rather than load the match data and the point by point data manually, the `get_url()` and `get_data()` functions below streamline the process.


```r
# no tournament in 2020
years <-
  c(seq(2015, 2019, 1), 2021)

get_url <- 
  function(year, type){
    
    if(!type %in% c('points', 'matches')) stop("Type must be one of 'points' or 'maches'")
    
    glue('https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/{year}-wimbledon-{type}.csv')
    
  }

get_data <- 
  function(url){
    
    #points datasets needs an extra step in order to use map_dfr
    data <- 
      if (str_detect(url, "points")){
        read_csv({{url}}) %>%
          as_tibble() %>%
          # this step needed for map_dfr(); in some datasets point number is numeric, others its a character
          mutate(PointNumber = as.numeric(PointNumber)) %>%
          clean_names()
        
      } else {
        
        readr::read_csv({{url}}) %>%
          as_tibble() %>%
          janitor::clean_names()
      }
    
    data
    
  }
```

Here I map my 2 functions over the years list and retrieve the points and matches data from 2015-2021

```r
points <-
  map(years, ~get_url(.x, type = 'points')) %>%
  map_dfr(., ~get_data(.x)) 

points
```

```
## # A tibble: 265,127 × 65
##    match_id       elapsed_time set_no p1games_won p2games_won set_winner game_no
##    <chr>          <time>        <dbl>       <dbl>       <dbl>      <dbl>   <dbl>
##  1 2015-wimbledo… 00'00"            1           0           0          0       1
##  2 2015-wimbledo… 00'00"            1           0           0          0       1
##  3 2015-wimbledo… 00'18"            1           0           0          0       1
##  4 2015-wimbledo… 00'40"            1           0           0          0       1
##  5 2015-wimbledo… 01'28"            1           0           0          0       1
##  6 2015-wimbledo… 02'22"            1           0           0          0       1
##  7 2015-wimbledo… 02'46"            1           1           0          0       1
##  8 2015-wimbledo… 03'40"            1           1           0          0       2
##  9 2015-wimbledo… 04'11"            1           1           0          0       2
## 10 2015-wimbledo… 04'36"            1           1           0          0       2
## # … with 265,117 more rows, and 58 more variables: game_winner <dbl>,
## #   point_number <dbl>, point_winner <dbl>, point_server <dbl>,
## #   speed_kmh <dbl>, rally <lgl>, p1score <chr>, p2score <chr>,
## #   p1momentum <dbl>, p2momentum <dbl>, p1points_won <dbl>, p2points_won <dbl>,
## #   p1ace <dbl>, p2ace <dbl>, p1winner <dbl>, p2winner <dbl>,
## #   p1double_fault <dbl>, p2double_fault <dbl>, p1unf_err <dbl>,
## #   p2unf_err <dbl>, p1net_point <dbl>, p2net_point <dbl>, …
```


```r
matches <-
  map(years, ~get_url(.x, type = 'matches')) %>%
  map_dfr(., ~get_data(.x)) 

matches
```

```
## # A tibble: 1,393 × 16
##    match_id  year slam  match_num player1 player2 status winner event_name round
##    <chr>    <dbl> <chr>     <dbl> <chr>   <chr>   <lgl>  <lgl>  <lgl>      <lgl>
##  1 2015-wi…  2015 wimb…      1101 Novak … Philip… NA     NA     NA         NA   
##  2 2015-wi…  2015 wimb…      1102 Jarkko… Lleyto… NA     NA     NA         NA   
##  3 2015-wi…  2015 wimb…      1103 Pierre… Hyeon … NA     NA     NA         NA   
##  4 2015-wi…  2015 wimb…      1104 Jan Le… Bernar… NA     NA     NA         NA   
##  5 2015-wi…  2015 wimb…      1105 Leonar… Thanas… NA     NA     NA         NA   
##  6 2015-wi…  2015 wimb…      1106 Janko … Marcel… NA     NA     NA         NA   
##  7 2015-wi…  2015 wimb…      1107 Marsel… Jerzy … NA     NA     NA         NA   
##  8 2015-wi…  2015 wimb…      1108 Lucas … Kevin … NA     NA     NA         NA   
##  9 2015-wi…  2015 wimb…      1109 Marin … Hiroki… NA     NA     NA         NA   
## 10 2015-wi…  2015 wimb…      1110 Andrea… Ricard… NA     NA     NA         NA   
## # … with 1,383 more rows, and 6 more variables: court_name <lgl>,
## #   court_id <lgl>, player1id <lgl>, player2id <lgl>, nation1 <lgl>,
## #   nation2 <lgl>
```

At this point I would normally save the data to a `data/raw` directory to preserve its initial state. Then I would create a separate script to read in the raw data to clean and format it. I skip that step here but below is the the code I would use to do that.


```r
write_feather(points, here("wimbledon", "data", "raw", "points.feather"))
write_feather(matches, here("wimbledon", "data", "raw" "matches.feather"))
```

## Clean the data

The next step is to clean and prep the data for analysis.

### Match data

The matches dataset has a ton of missing data, so we have to ignore most columns. Also, we can deconstruct `match_num` to extract meaningful match metadata. For example, the first number seems to indicate a mens match (1) vs a womens match (2), the 2nd number is the round in which the match took place and the last 2 numbers are the match number. The `format_matches` function applies these steps  


```r
matches
```

```
## # A tibble: 1,393 × 16
##    match_id  year slam  match_num player1 player2 status winner event_name round
##    <chr>    <dbl> <chr>     <dbl> <chr>   <chr>   <lgl>  <lgl>  <lgl>      <lgl>
##  1 2015-wi…  2015 wimb…      1101 Novak … Philip… NA     NA     NA         NA   
##  2 2015-wi…  2015 wimb…      1102 Jarkko… Lleyto… NA     NA     NA         NA   
##  3 2015-wi…  2015 wimb…      1103 Pierre… Hyeon … NA     NA     NA         NA   
##  4 2015-wi…  2015 wimb…      1104 Jan Le… Bernar… NA     NA     NA         NA   
##  5 2015-wi…  2015 wimb…      1105 Leonar… Thanas… NA     NA     NA         NA   
##  6 2015-wi…  2015 wimb…      1106 Janko … Marcel… NA     NA     NA         NA   
##  7 2015-wi…  2015 wimb…      1107 Marsel… Jerzy … NA     NA     NA         NA   
##  8 2015-wi…  2015 wimb…      1108 Lucas … Kevin … NA     NA     NA         NA   
##  9 2015-wi…  2015 wimb…      1109 Marin … Hiroki… NA     NA     NA         NA   
## 10 2015-wi…  2015 wimb…      1110 Andrea… Ricard… NA     NA     NA         NA   
## # … with 1,383 more rows, and 6 more variables: court_name <lgl>,
## #   court_id <lgl>, player1id <lgl>, player2id <lgl>, nation1 <lgl>,
## #   nation2 <lgl>
```


```r
format_matches <- 
  function(){
    matches %>%
      select(match_id:player2) %>%
      mutate(category = ifelse(str_sub(match_num, 1, 1) == 1, "men", "women"),
             round = str_sub(match_num, 2, 2),
             match_no = as.numeric(str_sub(match_num, 3, 4)))
  }

format_matches()
```

```
## # A tibble: 1,393 × 9
##    match_id    year slam   match_num player1   player2   category round match_no
##    <chr>      <dbl> <chr>      <dbl> <chr>     <chr>     <chr>    <chr>    <dbl>
##  1 2015-wimb…  2015 wimbl…      1101 Novak Dj… Philipp … men      1            1
##  2 2015-wimb…  2015 wimbl…      1102 Jarkko N… Lleyton … men      1            2
##  3 2015-wimb…  2015 wimbl…      1103 Pierre H… Hyeon Ch… men      1            3
##  4 2015-wimb…  2015 wimbl…      1104 Jan Lenn… Bernard … men      1            4
##  5 2015-wimb…  2015 wimbl…      1105 Leonardo… Thanasi … men      1            5
##  6 2015-wimb…  2015 wimbl…      1106 Janko Ti… Marcel G… men      1            6
##  7 2015-wimb…  2015 wimbl…      1107 Marsel I… Jerzy Ja… men      1            7
##  8 2015-wimb…  2015 wimbl…      1108 Lucas Po… Kevin An… men      1            8
##  9 2015-wimb…  2015 wimbl…      1109 Marin Ci… Hiroki M… men      1            9
## 10 2015-wimb…  2015 wimbl…      1110 Andreas … Ricardas… men      1           10
## # … with 1,383 more rows
```

It'll probably be useful to have a column for match winner. To get the winner for each match we can turn to the points data and use `p1games_won` and `p2games_won` to determine the match winner.


```r
get_match_winner <- 
  function(){
    
    points %>%
      filter(set_winner > 0) %>%
      select(match_id, p1games_won, p2games_won) %>%
      group_by(match_id) %>%
      summarise(tot_p1games_won = sum(p1games_won),
                tot_p2games_won = sum(p2games_won)) %>%
      mutate(match_winner = ifelse(tot_p1games_won > tot_p2games_won, "player1", "player2")) %>%
      select(match_id, match_winner)
  }

get_match_winner()
```

```
## # A tibble: 1,387 × 2
##    match_id            match_winner
##    <chr>               <chr>       
##  1 2015-wimbledon-1101 player1     
##  2 2015-wimbledon-1102 player1     
##  3 2015-wimbledon-1103 player1     
##  4 2015-wimbledon-1104 player2     
##  5 2015-wimbledon-1105 player1     
##  6 2015-wimbledon-1106 player2     
##  7 2015-wimbledon-1107 player1     
##  8 2015-wimbledon-1108 player2     
##  9 2015-wimbledon-1109 player1     
## 10 2015-wimbledon-1110 player2     
## # … with 1,377 more rows
```

Finally we can apply and join the the 2 functions to get our cleaned matches dataset. (As above, for the purposes of this post we'll skip the step of saving the cleaned data, but the code to do so is included below).


```r
matches <- 
  format_matches() %>%
  inner_join(., get_match_winner()) %>%
  mutate(match_winner = ifelse(match_winner == "player1", player1, player2))

matches
```

```
## # A tibble: 1,387 × 10
##    match_id    year slam   match_num player1   player2   category round match_no
##    <chr>      <dbl> <chr>      <dbl> <chr>     <chr>     <chr>    <chr>    <dbl>
##  1 2015-wimb…  2015 wimbl…      1101 Novak Dj… Philipp … men      1            1
##  2 2015-wimb…  2015 wimbl…      1102 Jarkko N… Lleyton … men      1            2
##  3 2015-wimb…  2015 wimbl…      1103 Pierre H… Hyeon Ch… men      1            3
##  4 2015-wimb…  2015 wimbl…      1104 Jan Lenn… Bernard … men      1            4
##  5 2015-wimb…  2015 wimbl…      1105 Leonardo… Thanasi … men      1            5
##  6 2015-wimb…  2015 wimbl…      1106 Janko Ti… Marcel G… men      1            6
##  7 2015-wimb…  2015 wimbl…      1107 Marsel I… Jerzy Ja… men      1            7
##  8 2015-wimb…  2015 wimbl…      1108 Lucas Po… Kevin An… men      1            8
##  9 2015-wimb…  2015 wimbl…      1109 Marin Ci… Hiroki M… men      1            9
## 10 2015-wimb…  2015 wimbl…      1110 Andreas … Ricardas… men      1           10
## # … with 1,377 more rows, and 1 more variable: match_winner <chr>
```

```r
# write_feather(matches, here("wimbledon", "data", "formatted", "matches.feather"))
```

### Points data


```r
points
```

```
## # A tibble: 265,127 × 65
##    match_id       elapsed_time set_no p1games_won p2games_won set_winner game_no
##    <chr>          <time>        <dbl>       <dbl>       <dbl>      <dbl>   <dbl>
##  1 2015-wimbledo… 00'00"            1           0           0          0       1
##  2 2015-wimbledo… 00'00"            1           0           0          0       1
##  3 2015-wimbledo… 00'18"            1           0           0          0       1
##  4 2015-wimbledo… 00'40"            1           0           0          0       1
##  5 2015-wimbledo… 01'28"            1           0           0          0       1
##  6 2015-wimbledo… 02'22"            1           0           0          0       1
##  7 2015-wimbledo… 02'46"            1           1           0          0       1
##  8 2015-wimbledo… 03'40"            1           1           0          0       2
##  9 2015-wimbledo… 04'11"            1           1           0          0       2
## 10 2015-wimbledo… 04'36"            1           1           0          0       2
## # … with 265,117 more rows, and 58 more variables: game_winner <dbl>,
## #   point_number <dbl>, point_winner <dbl>, point_server <dbl>,
## #   speed_kmh <dbl>, rally <lgl>, p1score <chr>, p2score <chr>,
## #   p1momentum <dbl>, p2momentum <dbl>, p1points_won <dbl>, p2points_won <dbl>,
## #   p1ace <dbl>, p2ace <dbl>, p1winner <dbl>, p2winner <dbl>,
## #   p1double_fault <dbl>, p2double_fault <dbl>, p1unf_err <dbl>,
## #   p2unf_err <dbl>, p1net_point <dbl>, p2net_point <dbl>, …
```

The first part of `format_points()` creates some time columns: match minutes, set minutes and game minutes. then I add some sequence numbers that assigns a row number for each game, set and match. This comes in handy when trying to determine how many points happen in each game, set or match, and also helps to partition sets and games within a match, `game_seq` will reset after every game, `set_seq` rests after each set and `match_seq` is a sequence of row number from 1 to the end of the match.


```r
format_points <- 
  function(){

    time <- 
      points %>%
      filter(point_number > 0) %>%
      select(match_id, set_no, elapsed_time) %>%
      group_by(match_id, set_no) %>%
      filter(elapsed_time == max(elapsed_time)) %>%
      ungroup() %>%
      group_by(match_id) %>%
      mutate(
        set_length_mins = case_when(
          set_no == 1 ~ as.numeric(elapsed_time)/60,
          TRUE ~ (as.numeric(elapsed_time) - lag(as.numeric(elapsed_time)))/60),
        game_length_mins = sum(set_length_mins)
      )
    
    points_formatted <- 
      points %>%
      filter(point_number > 0) %>%
      group_by(match_id) %>%
      mutate(match_seq = dplyr::row_number()) %>%
      ungroup() %>%
      group_by(match_id, set_no) %>%
      mutate(set_seq = dplyr::row_number()) %>%
      ungroup() %>%
      group_by(match_id, set_no, game_no) %>%
      mutate(game_seq = dplyr::row_number()) %>%
      ungroup() %>% 
      inner_join(., time %>% select(-elapsed_time), by = c("match_id", "set_no")) %>%
      select(match_id, set_no, match_seq:game_length_mins, everything()) %>%
      mutate(p1score = as.numeric(p1score),
             p2score = as.numeric(p2score))
    
    points_formatted
    
  }

# write_feather(points, here("wimbledon", "data", "formatted", "points.feather"))

points
```

```
## # A tibble: 265,127 × 65
##    match_id       elapsed_time set_no p1games_won p2games_won set_winner game_no
##    <chr>          <time>        <dbl>       <dbl>       <dbl>      <dbl>   <dbl>
##  1 2015-wimbledo… 00'00"            1           0           0          0       1
##  2 2015-wimbledo… 00'00"            1           0           0          0       1
##  3 2015-wimbledo… 00'18"            1           0           0          0       1
##  4 2015-wimbledo… 00'40"            1           0           0          0       1
##  5 2015-wimbledo… 01'28"            1           0           0          0       1
##  6 2015-wimbledo… 02'22"            1           0           0          0       1
##  7 2015-wimbledo… 02'46"            1           1           0          0       1
##  8 2015-wimbledo… 03'40"            1           1           0          0       2
##  9 2015-wimbledo… 04'11"            1           1           0          0       2
## 10 2015-wimbledo… 04'36"            1           1           0          0       2
## # … with 265,117 more rows, and 58 more variables: game_winner <dbl>,
## #   point_number <dbl>, point_winner <dbl>, point_server <dbl>,
## #   speed_kmh <dbl>, rally <lgl>, p1score <chr>, p2score <chr>,
## #   p1momentum <dbl>, p2momentum <dbl>, p1points_won <dbl>, p2points_won <dbl>,
## #   p1ace <dbl>, p2ace <dbl>, p1winner <dbl>, p2winner <dbl>,
## #   p1double_fault <dbl>, p2double_fault <dbl>, p1unf_err <dbl>,
## #   p2unf_err <dbl>, p1net_point <dbl>, p2net_point <dbl>, …
```

Now we're set to actually start analyzing the data in part 2 of this series.
