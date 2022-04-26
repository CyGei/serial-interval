library(truncnorm)

cy_lhs <- function(n, q){
  strata <- data.frame(n, q) %>%  #where n defines the number of stratas (# of intervals with equal probabilities)
    mutate(lower_q = q,           # where q returns the number whose cumulative distribution matches the probability
           upper_q = lead(q),     
           n_interval = paste0(n, "-", lead(n))) %>% 
    select(-c(n, q) ) 
  
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
  mu <- qtruncnorm(n,
                   mean = median(min_mu:max_mu), 
                   sd = median(min_sd:max_sd),
                   a = min_mu, b = max_mu)     #get the q for the mean
  min_cv <- min_sd/min_mu
  max_cv <- max_sd/max_mu
  cv <- qtruncnorm(n,
                   mean = median(min_cv:max_cv),
                   sd = median(min_sd:max_sd),
                   a = min_cv, b = max_cv)    # get the q for the cv
  
  
  mu_lhs <- cy_lhs(n, mu) # run the lhs function
  cv_lhs <- cy_lhs(n, cv)
  mu_random <- sample(mu_lhs, size = length(mu_lhs)) #reshuffle the samples
  cv_random <- sample(cv_lhs, size = length(cv_lhs))
  params <- epitrix::gamma_mucv2shapescale(mu_random,cv_random) #convert to gamma params
  return(params)
  
}


incubation_params <- get_lhs_params(
  min_mu = 4.9,
  min_sd = 2.2,
  max_mu = 6.7,
  max_sd = 5.2,
  n = seq(0, 1, 0.1)
)

generation_time_params<- get_lhs_params(
  min_mu = 3.95,
  min_sd = 1.51,
  max_mu = 5.57,
  max_sd = 2.23,
  n = seq(0, 1, 0.1)
)




