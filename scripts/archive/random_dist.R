

make_gamma_params <- function(n, min_mu, max_mu, min_sd, max_sd){
 
  ## create a vector of means from rescaled normal
  mu_sample <- vector()
  while (length(mu_sample)< n ) {
    r <- rnorm(1, mean = median(min_mu:max_mu), sd = median(min_sd:max_sd))
    print(r)
    if(r >= min_mu & r <= max_mu){
      mu_sample <- c(mu_sample, r)
    } else{
      cat(red("r is", r, "\n"))
      next
    }
  }

  min_cv <- min_sd/min_mu
  max_cv <- max_sd/max_mu
  
  cv_sample <- vector()
  while (length(cv_sample)< n ) {
    r <- rnorm(1, mean = median(min_cv:max_cv), sd = median(min_sd:max_sd))
    print(r)
    if(r >= min_cv & r <= max_cv){
      cv_sample <- c(cv_sample, r)
    } else{
      cat(red("r is", r, "\n"))
      next
    }
  }
  
  sd_sample <- cv_sample * mu_sample
  params <- epitrix::gamma_mucv2shapescale(mu_sample, cv_sample)
  
  par(mfrow = c(2,3))
  hist(mu_sample); hist(sd_sample);hist(cv_sample)
  hist(params[["shape"]]);hist(params[["scale"]])
  return(params)
}


incubation_params <- make_gamma_params(n = 1000, 
                                       min_mu = 4.9, 
                                       min_sd = 2.2, 
                                       max_mu = 6.7, 
                                       max_sd = 5.2)

# RANDOM INCUBATION -------------------------------------------------------
##Quesada et al for incubation

hist(vec)
r <- rnorm(1000, mean = 2, sd = 2.2)
r <- r[r > 0]
r <- r[sample(r, 100)] # not work
hist(r)
 
# WITH TRUNCNORM OR RUNIF -------------------------------------------------


make_gamma_params <- function(n, min_mu, max_mu, min_sd, max_sd){
  mu_sample <- rtruncnorm(n, a = min_mu, b = max_mu)
  min_cv <- min_sd/min_mu
  max_cv <- max_sd/max_mu
  cv_sample <- rtruncnorm(n, a = min_cv, b = max_cv)
  sd_sample <- cv_sample * mu_sample
  params <- epitrix::gamma_mucv2shapescale(mu_sample, cv_sample)
  
  par(mfrow = c(2,3))
  hist(mu_sample); hist(sd_sample);hist(cv_sample)
  hist(params[["shape"]]);hist(params[["scale"]])
  return(params)
}

comb<- data.frame(shape = incubation_params[["shape"]], scale = incubation_params[["scale"]])
nrow(unique(comb[,c("shape", "scale")]))
incubation_params <- make_gamma_params(n = 1000, 
                   min_mu = 4.9, 
                   min_sd = 2.2, 
                   max_mu = 6.7, 
                   max_sd = 5.2)



# william s.hart elife generation time

generation_time_params <- make_gamma_params(n = 1000, 
                                       min_mu = 3.95, 
                                       min_sd = 1.51, 
                                       max_mu = 5.57, 
                                       max_sd = 2.23)

df_test <- input_df %>%
  filter(household_id == "37446" | household_id== "12589")
df_test




out_result_list <- list()
unique_households <- unique(df_test$household_id)
for (i in 1:length(unique_households)) {
  
  household <- unique_households[i]
  hh_input_df <- df_test %>%
    filter(household_id == household)
  
  cat(
    green("Current i:", "[", i, "]", "\n"),
    green("Household ID:", "[", household, "]", "\n"),
    green("Progress:", "[", round((
      i / length(unique_households) * 100
    ), digits = 2), "%]", "\n")
  )
  

  out_all_combinations <- data.frame()
  for (j in 1:10) {
    cat(
      red("Current model:", "[", j, "]", "\n"),
      red("Progress:", "[", round((
        j / 10 * 100
      ), digits = 2), "%]", "\n")
    )
    incubation_distribution <- distcrete::distcrete(
      "gamma",
      shape = incubation_params[["shape"]][j],
      scale = incubation_params[["scale"]][j],
      interval = 1,
      w = 1
    )
    
    generation_time_distribution <- distcrete::distcrete(
      "gamma",
      shape = generation_time_params[["shape"]][j],
      scale = generation_time_params[["scale"]][j],
      interval = 1,
      w = 1
    )
    
    #outbreaker data:
    out_data <- outbreaker_data(
      dates = hh_input_df$fake_start_dt2,
      ids = hh_input_df$newID,
      w_dens = generation_time_distribution$d(1:100),
      f_dens = incubation_distribution$d(1:100) # incubation time distribution
    )
    
    #RUN THE MODEL
    out <- outbreaker(data = out_data,
                      config = config)
    out$incubation <-
      paste0("shape: ",
             incubation_params[["shape"]][j],
             " ",
             "scale: ",
             incubation_params[["shape"]][j])
    out$generation_time <-
      paste0("shape: ",
             generation_time_params[["scale"]][j],
             " ",
             "scale: ",
             generation_time_params[["scale"]][j])
    out_all_combinations <- rbind(out_all_combinations, out)
  }
  
  out_result_list[[household]] <- out_all_combinations # save results
}








# TEST QUARTER DAYS -------------------------------------------------------

df_test <- t %>%
  filter(household_id == "37446" | household_id== "12589")
df_test
View(df_test)

quarter_dist <- vector()
for (i in (1:100)) {
  val <- gi$d(i)/4
  vec<- rep(val, 4)
  quarter_dist<- c(quarter_dist, vec)
}
quarter_dist
plot(quarter_dist)
