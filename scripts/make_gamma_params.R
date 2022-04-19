# 
# make_gamma_params <- function(n, min_mu, max_mu, min_sd, max_sd){
#   
#   ## create a vector of means from rescaled normal
#   mu_sample <- vector()
#   while (length(mu_sample)< n ) {
#     r <- rnorm(1, mean = median(min_mu:max_mu), sd = median(min_sd:max_sd))
#     if(r >= min_mu & r <= max_mu){
#       mu_sample <- c(mu_sample, r)
#     } else{
#       next
#     }
#   }
#   
#   min_cv <- min_sd/min_mu
#   max_cv <- max_sd/max_mu
#   
#   cv_sample <- vector()
#   while (length(cv_sample)< n ) {
#     r <- rnorm(1, mean = median(min_cv:max_cv), sd = median(min_sd:max_sd))
#     if(r >= min_cv & r <= max_cv){
#       cv_sample <- c(cv_sample, r)
#     } else{
#       next
#     }
#   }
#   
#   sd_sample <- cv_sample * mu_sample
#   params <- epitrix::gamma_mucv2shapescale(mu_sample, cv_sample)
#   
#   par(mfrow = c(2,3))
#   hist(mu_sample); hist(sd_sample);hist(cv_sample)
#   hist(params[["shape"]]);hist(params[["scale"]])
#   return(params)
# }
# 



make_gamma_params <- function(n, min_mu, max_mu, min_sd, max_sd){
  mu_sample <- rtruncnorm(n, 
                          mean = median(min_mu:max_mu), 
                          sd = median(min_sd:max_sd),
                          a = min_mu, b = max_mu)
  min_cv <- min_sd/min_mu
  max_cv <- max_sd/max_mu
  cv_sample <- rtruncnorm(n,
                          mean = median(min_cv:max_cv),
                          sd = median(min_sd:max_sd),
                          a = min_cv, b = max_cv)
  sd_sample <- cv_sample * mu_sample
  params <- epitrix::gamma_mucv2shapescale(mu_sample, cv_sample)
  
  par(mfrow = c(2,3))
  hist(mu_sample); hist(sd_sample);hist(cv_sample)
  hist(params[["shape"]]);hist(params[["scale"]])
  return(params)
}
