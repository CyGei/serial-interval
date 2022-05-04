
cy_lhs <- function(intervals, q){
  strata <- data.frame(intervals, q) %>%  #where n defines the number of stratas (# of intervals with equal probabilities)
    mutate(lower_q = q,           # where q returns the number whose cumulative distribution matches the probability
           upper_q = lead(q),     # we wish to sample one point from each interval
           n_interval = paste0(intervals, "-", lead(intervals))) %>% 
    select(-c(intervals, q) ) 
  
  strata <- strata[-nrow(strata),]
  r <- vector()
  for (i in 1:nrow(strata)) {
    r[i] <- runif(1, min = strata$lower_q[i], max = strata$upper_q[i])   #randomly sample 1 point for each strata
  }
  
  return(r)
}

get_lhs_params <- function(n, 
                           min_mu, 
                           min_sd,
                           max_mu, 
                           max_sd){
  
  intervals <- seq(0,1, 1/n) #treshold values
  mu <- qtruncnorm(intervals,
                   mean = median(min_mu:max_mu), 
                   sd = median(min_sd:max_sd),
                   a = min_mu, b = max_mu)     #get the q for the mean
  min_cv <- min_sd/min_mu
  max_cv <- max_sd/max_mu
  cv <- qtruncnorm(intervals,
                   mean = median(min_cv:max_cv),
                   sd = median(min_sd:max_sd),
                   a = min_cv, b = max_cv)    # get the q for the cv
  
  
  mu_lhs <- cy_lhs(intervals, mu) # run the lhs function
  cv_lhs <- cy_lhs(intervals, cv)
  mu_random <- sample(mu_lhs, size = length(mu_lhs)) #reshuffle the samples
  cv_random <- sample(cv_lhs, size = length(cv_lhs))
  params <- epitrix::gamma_mucv2shapescale(mu_random,cv_random) #convert to gamma params
  return(params)
  
}
# 
# min_mu = 3.95
# min_sd = 1.51
# max_mu = 5.57
# max_sd = 2.23
# n = 100
# 
# 
# ggplot()+
#   aes(x = mu_random, y = cv_random)+
#   geom_point(data = data.frame(mu_random = mu_random,
#                                cv_random = cv_random), col = "red")+
#   scale_x_continuous(breaks =seq(min(mu_random),max(mu_random), length.out = n))+
#   scale_y_continuous(breaks = seq(min(cv_random),max(cv_random), length.out = n))+
#   # coord_equal(xlim = c(min(mu_random), max(mu_random)),
#   #             ylim = c(min(cv_random), max(cv_random)))+
#   theme(panel.border = element_rect(fill = NA),
#         panel.background = element_rect(fill = NA),
#         panel.grid.major = element_line(colour = "grey", size = rel(0.5)),
#         panel.ontop = TRUE)



