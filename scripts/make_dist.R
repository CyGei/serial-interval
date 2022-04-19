


make_dist <- function(distribution, mu, sd) {
  cv <- sd / mu
  params <- gamma_mucv2shapescale(mu, cv)
  dist <- distcrete::distcrete(
    distribution ,
    shape = params$shape,
    scale = params$scale,
    interval = 1,
    #day
    w = 0.5
  )
  
  p <- tibble(days = 0:100,
              density = dist$d(0:100)) %>%
    ggplot() +
    aes(x = days, y = density) +
    geom_col(width = 0.95,
             color = "black",
             fill = "steelblue") +
    theme_minimal()
  
  dist[["plot"]] <- p
  print(dist)
}

