---
title: MyRide
author: Brendan Graham
date: '2021-03-27'
slug: use-changepoint-detection-to-analyze-your-intervals
categories: [R, algorithm, cycling]
tags: []
subtitle: 'Use Changepoint Detection to Analyze Your Intervals'
summary: 'Learn how to apply a changepoint detection algorithm to automatically identify changes in a process'
authors: []
lastmod: '2021-03-27'
image:
  caption:
  focal_point: Smart
projects: [my_ride]
output:
  blogdown::html_page:
    toc: false
    number_sections: false
    toc_depth: 1
draft: TRUE
---

![](myride_example.gif)

## Motivation

I created this app to help monitor my training rides, specifically  interval workouts done outdoors (or at least those not done on a smart trainer). Smart trainers lock you in to a certain wattage, so there's no uncertainty about whether or not you held the correct power for the prescribed time. But interval workouts done outdoors are a different story - you're either manually reviewing them afterwards or getting a feel for if you hit the interval during the ride as you glance down. This app uses changepoint detection to automatically detect sustained changes in your effort, and can tell you the average power within each detected change region.

This app was fun to create - I got to learn how to parse `.fit` files and learned about how to implement changepoint detection to automatically detect intervals. However, shortly after I created it I came across an amazing app called https://intervals.icu/, which is the same kind of idea, but 1000x better. Regardless, I still use the app and am happy with how it came out and what I was able to learn by developing it.

## Changepoint Detection

The data from a cycling workout is essentially timeseries data at the second-level, meaning 1 observation per second. A typical interval workout might look something like 10 minute warm up, 5 minutes @ 250 watts followed by 5 minutes @ 130 watts, repeated 4 times then followed by a 10 minute cool down. If performed correctly, we'd expected clear changes in power levels at those intervals. Changepoint detection should be able to automatically identify those intervals, and once the intervals are identified, we can take the mean within each interval and compare it to the prescribed workout. So basically we know there are intervals *somewhere* within the data but we're not sure *where* they occur or for *how long*, so we let the algorithm try to identfy them.

I use the `{changepoint}` package to implement the following simple function, where `data$metric` is a user-selected input of either power, cadence, speed or heart rate and the sensitivity is a user-adjustment in the case that the default doesn't pick up on the intervals correctly


```r
get_changepoints <-
  function(data, sensitivity){
    # returns dataframe with mean of selected metric within each changepoint region
    
    # identifies changepoints for selected metric given selected sensitivity level
    cp_mean <-
      cpt.mean(data$metric,
               test.stat = 'Normal',
               method = 'BinSeg',
               Q = sensitivity,
               penalty = "SIC")
    
    #stores the values of the second in which each changepoint occurs
    changepoint_seconds <-
      cpts(cp_mean)
    
    #calculates avg value of selected metric within each changepoint region
    data %>%
      mutate(changes = ifelse(ride_sec %in% changepoint_seconds, ride_sec, NA_real_)) %>%
      tidyr::fill(changes, .direction = "downup") %>%
      group_by(changes) %>%
      mutate(mean = mean(metric, na.rm = T))
  }
```

You can check out all of the code for it on [GitHub](https://github.com/brndngrhm/my_ride), or view the app itself: https://myride.brendangraham.online
