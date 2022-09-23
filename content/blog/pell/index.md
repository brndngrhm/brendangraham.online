---
title: "Creating a Hex Bin Map to Show Changes Pell Grants"
author: Brendan Graham
date: '2022-09-10'
slug: pell
categories: 
  - tidy tuesday
  - data viz
  - hex bin map
tags:
  - tidy tuesday
  - data viz
  - hex bin map
subtitle: 
summary: 'This post analyzes a [TidyTuesday](https://github.com/rfordatascience/tidytuesday) data set about Pell Grants. After exploring the data I dig into changes in amounts post-2009 and create some visualizations.'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
draft: false
editor_options: 
  chunk_output_type: console
  markdown: 
    wrap: 72
---

<script src="{{< blogdown/postref >}}index_files/core-js/shim.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/react/react-dom.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactwidget/react-tools.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index_files/reactable-binding/reactable.js"></script>

## Get the data

First I read in the data and do some minor cleaning:

-   only include data post-1999 since it looks like 1999 isn’t a full
    year of data.

-   overwrite the `state_name` variable with the full state name instead
    of the abbreviation. This makes creating a hex bin map later on a
    little easier. One consequence of this is Puerto Rico and other US
    Territories are excluded

``` r
tt_url <- 
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv"

pell <- 
  readr::read_csv(tt_url) %>%
  janitor::clean_names() %>%
  filter(year > 1999) %>%
  mutate(state_name = usdata::abbr2state(abbr = state))

pell %>% 
  head(25) %>%
  get_table()
```

<div id="htmlwidget-1" class="reactable html-widget" style="width:auto;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"tag":{"name":"Reactable","attribs":{"data":{"state":["AK","AK","AK","AK","AK","AK","AK","AK","AK","AK","AL","AL","AL","AL","AL","AL","AL","AL","AL","AL","AL","AL","AL","AL","AL"],"award":[176368,111287.57,300101.39,380022.17,24900,20946,105450.5,3499882.17,2461738,408827,6163713.79,1805533.41,97580.5,7504705,1580385,5762405,3354757,1068798,4077959.65,449587,7242423.59,4377051,1344746.11,1255224,1785314.41],"recipient":[98,55,186,245,12,12,48,1910,1220,233,2488,895,46,3131,821,2685,1590,716,1991,207,3463,2457,841,840,889],"name":["Alaska Pacific University","Alaska Vocational Technical Center","Career Academy","Charter College","Ilisagvik College","New Concepts Beauty School","Sheldon Jackson College","University of Alaska - Anchorage","University of Alaska at Fairbanks","University of Alaska at Southeast","Alabama Agricultural & Mechanical University","Alabama Southern Community College","Alabama State College of Barber Styling","Alabama State University","Athens State University","Auburn University","Auburn University Montgomery","Bessemer State Technical College","Bevill State Community College","Birmingham Southern College","Bishop State Community College - Main Campus","Calhoun State Community College","Capps College","Central Alabama Community College","Chattahoochee Valley Community College"],"session":["2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01","2000-01"],"year":[2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,2000],"state_name":["Alaska","Alaska","Alaska","Alaska","Alaska","Alaska","Alaska","Alaska","Alaska","Alaska","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama","Alabama"]},"columns":[{"accessor":"state","name":"state","type":"character"},{"accessor":"award","name":"award","type":"numeric"},{"accessor":"recipient","name":"recipient","type":"numeric"},{"accessor":"name","name":"name","type":"character"},{"accessor":"session","name":"session","type":"character"},{"accessor":"year","name":"year","type":"numeric"},{"accessor":"state_name","name":"state_name","type":"character"}],"resizable":true,"filterable":true,"defaultPageSize":10,"paginationType":"numbers","showPageInfo":true,"minRows":1,"highlight":true,"striped":true,"compact":true,"nowrap":true,"inline":true,"dataKey":"5be4316b0d82a1284278fd9a536d76b4"},"children":[]},"class":"reactR_markup"},"evals":[],"jsHooks":[]}</script>

## Explore the data

It looks like some time around 2009 the total dollar amount of Pell
grants increased and remained at a higher level the prior to 2009. The
volume of grants remained constant though, so we see the impact of this
increase in the average amount per grant.

``` r
ggpubr::ggarrange(
  
  pell %>% 
    group_by(year) %>%
    tally() %>% 
    ggplot(., aes(x = year, y = n)) +
    geom_col(fill = bg_red) +
    bg_theme(base_size = 12, plot_title_size = 12) + 
    labs(x = "Year", y = "Count", title = "Total Pell Grant Volume per Year"),
  
  pell %>% 
    group_by(year) %>%
    summarise(total_amt = sum(award, na.rm = T)) %>% 
    ggplot(., aes(x = year, y = total_amt/1000000)) +
    geom_col(fill = bg_green) +
    bg_theme(base_size = 12, plot_title_size = 12) + 
    scale_y_continuous(labels = dollar) + 
    labs(x = "Year", y = "$mil", title = "Total Pell Grant Dollar Amount per Year"),
  
  pell %>%
    group_by(year) %>%
    summarise(mean_amt = mean(award, na.rm = T)) %>% 
    ggplot(., aes(x = year, y = mean_amt/1000000)) +
    geom_col(fill = bg_blue) +
    bg_theme(base_size = 12, plot_title_size = 12) + 
    scale_y_continuous(labels = dollar) + 
    labs(x = "Year", y = "$mil", title = "Mean Pell Grant Dollar Amount per Year"), 
  nrow = 3,
  ncol = 1
)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Looking state by states we can see not all states behave the same. Some
states spike post 2009 and come down to a new baseline level like AZ,
while some increase and remain steady at a new level, like CA.

``` r
pell %>% 
  group_by(state, year) %>%
  summarise(mean_amt = mean(award, na.rm = T)) %>% 
  ungroup() %>%
  ggplot(., aes(x = year, y = mean_amt, color = state)) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = dollar) + 
  gghighlight(label_key = state, use_direct_label = T, state %in% c("CA", "AZ"), use_group_by = FALSE) + 
  labs(x = "Year", y = "Amount ($)", title = "Mean Pell Grant Dollar Amount per Year") + 
  scale_color_npg() + 
  bg_theme(base_size = 14, plot_title_size = 14)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Digging into this post 2009 increase some more, there is some evidence
that the policy changes as a result of the 2008 financial crisis can
explain some of what we’re seeing in the data.

Specifically, that document notes that

> the recession of 2007–2009 and the subsequent slow recovery drew more
> students into the recipient pool. Eligibility increased as adult
> students and the families of dependent students experienced losses in
> income and assets; enrollment of eligible students also rose as people
> who had lost jobs sought to acquire new skills and people who would
> have entered the workforce enrolled in school because they could not
> find employment. The expansion of online education, particularly at
> for-profit institutions, attracted still more students, many of whom
> were eligible for Pell grant.

## Visualizing the Change

This plot ranks states in order of change post-2009, with Arizona
leading the way.

``` r
# https://www.cbo.gov/sites/default/files/cbofiles/attachments/44448_PellGrants_9-5-13.pdf

time_prep <- 
  pell %>%
  mutate(timeframe = ifelse(year < 2009, "pre_2009", "post_2009")) %>%
  group_by(year, state_name, timeframe) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  group_by(state_name, timeframe) %>%
  mutate(mean_grants = mean(total)) %>%
  select(state_name, timeframe, mean_grants)

pell_time_state_comparison <- 
  pell %>%
  mutate(timeframe = ifelse(year < 2009, "pre_2009", "post_2009")) %>%
  group_by(state_name, timeframe) %>%
  summarise(total_amt = sum(award, na.rm = T),
            mean_amt = mean(award, na.rm = T),
            sd_amt = sd(award, na.rm = T)) %>%
  left_join(., time_prep, by = c("state_name", "timeframe")) %>%
  distinct()


pell_time_state_comparison %>% 
  select(state_name, mean_amt, timeframe) %>%
  pivot_wider(names_from = timeframe, values_from = mean_amt) %>%
  mutate(diff = post_2009 - pre_2009) %>%
  filter(!(is.na(state_name))) %>%
  ggplot(., aes(x = reorder(state_name, diff), y = diff, label = paste0("$", round(diff/100000,2)),  fill = log(diff))) + 
  scale_y_continuous(labels = scales::dollar) + 
  scale_fill_gradient(low = "cornflowerblue", high = bg_green) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  bg_theme(base_size = 13, plot_title_size = 14) +
  geom_text(hjust = 1.05, family = 'RobotoMono-Regular', size = 3, color = 'white') + 
  labs(y = "Amount($)", x = "State", title =" States Ranked by Post-2009 Change in Mean Grant Amount")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="960" />

We can also visualize these increases using a hex bin map, in which each
state is represented by a hexagon , color coded with the post-2009
increase in avg Pell grant money received.

``` r
# https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html

# Download the Hexagones boundaries at geojson format here: 
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- 
  geojson_read(here::here("content", "blog", "pell", "us_states_hexgrid.geojson"),
               what = "sp")

# Bit of reformating
spdf@data <- 
  spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# Show framework
# plot(spdf)

library(broom)
spdf_fortified <- 
  tidy(spdf, region = "google_name")

library(rgeos)
centers <- 
  cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

``` r
# Merge geospatial and numerical information
spdf_fortified <- 
  spdf_fortified %>%
  left_join(. , pell_time_state_comparison %>% 
              select(state_name, mean_amt, timeframe) %>%
              pivot_wider(names_from = timeframe, values_from = mean_amt) %>%
              mutate(diff = post_2009 - pre_2009), by = c("id" = "state_name")) 
 
# Make a first chloropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  diff, x = long, y = lat, group = group)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-2.png" width="672" />

``` r
hist(spdf_fortified$diff)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-3.png" width="672" />

``` r
# Prepare binning

spdf_fortified$bin <-
  cut(spdf_fortified$diff, 
      breaks =c(seq(0, 12000000, 2000000), Inf), 
      labels = c("0-$2 mil", "$2-$4 mil", "$4-$6 mil", "$6-$8 mil", "$8-$10 mil", "$10-$12 mil", "$12+ mil"),
      include.lowest = TRUE)
 
# Prepare a color scale coming from the viridis color palette
library(viridis)
my_palette <-
  viridis::inferno(6)

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = bin, x = long, y = lat, group = group)) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void() +
  coord_map() + 
  scale_fill_manual( 
    values = my_palette, 
    name = "Change in Pell Grants $millions", 
    guide = guide_legend(keyheight = unit(3, units = "mm"),
                          keywidth=unit(12, units = "mm"),
                         label.position = "bottom",
                         title.position = 'top', nrow=1) 
  ) +
  ggtitle("Change in Mean Pell Grants by State\nPre/Post 2009") +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-4.png" width="672" />
