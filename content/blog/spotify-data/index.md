---
date: "2019-11-27"
external_link:
image:
  caption:
  focal_point: Smart
links:
summary: Use the Spotify API to analyze your favorite artists songs
tags:
- Project
- Data Viz
title: Everyone Everywhere Music Analysis Tool
url_code:
url_pdf:
url_slides:
url_video:
---

![](featured.png)

This app calls the Spotify API and leverages the "Features" that Spotify uses to classify songs. This project started when we were joking that we could use AI to write a new Everyone Everywhere song. That conversation reminded me of a [blog post](https://peerchristensen.netlify.com/post/clustering-springsteen-albums-with-spotifyr/) I had seen where the author used the `spotifyR` package to get song data and analyze the songs. I thought it would be interesting to analyze my own band's songs in the same manner, and also provide anyone a way to search for bands and see how Spotify classifies their songs in terms of energy, danceability, valence, liveness, etc...  
  
Check out the app [here](https://brndngrhm.shinyapps.io/EveryoneEverywhereMusicAnalysisTool/)  

Upon load, the app displays an analysis of Everyone Everywhere tracks. A user can pick a feature, such as energy, and the app will rank tracks according to energy, and show a distribution of track energy for every album associated with the band. The lower left chart uses k-means to cluster artist tracks, and the lower right radar charts show how albums compare to each other across several features.

View the project on [GitHub](https://github.com).