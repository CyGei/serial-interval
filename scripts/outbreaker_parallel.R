# nested_data <- data %>% 
#   group_by(group_id) %>% 
#   nest()

mydf_list <- split(data, f = data$group_id)

library(future)
library(future.apply)

i=1
w_distribution <- distcrete::distcrete(
  "gamma",
  shape = generation_time_params[["shape"]][i],
  scale = generation_time_params[["scale"]][i],
  interval = 1,
  w = 1
)

f_distribution <- distcrete::distcrete(
  "gamma",
  shape = incubation_params[["shape"]][i],
  scale = incubation_params[["scale"]][i],
  interval = 1,
  w = 1
)


outbreaker_f <- function(x) {
  outbreaker2::outbreaker(
    data = outbreaker2::outbreaker_data(
      dates = x[["date_onset"]],
      ids = x[["id"]],
      w_dens = w_distribution$d(1:100),
      f_dens = f_distribution$d(1:100)
    ),
    config = config
  )
}
  
  
rm(i)

########################
##purrr:map
########################
test_map_time <-
  system.time({
    test_map <- map(mydf_list, outbreaker_f)
  })


########################
##furrr::future_map
########################
library(furrr)
test_future_map_time <- system.time({
  oplan <- plan(multisession, workers = 9)
  on.exit(plan(oplan))
  test_future_map <- future_map(mydf_list, outbreaker_f)
})

########################
## lapply
########################
test_lapply_time <- system.time({
  test_lapply <- lapply(mydf_list, outbreaker_f)
})

########################
## future.apply::future_lapply()
########################
test_future_lapply_time <- system.time({
  oplan <- plan(multisession, workers = 9)
  on.exit(plan(oplan))
  
  test_future_lapply <- future_lapply(mydf_list,
                                      outbreaker_f) #'future.seed=TRUE'????
})


########################
##parLapply
########################

library(parallel)

test_parLapply_time <- system.time({
  capacity <- 60
  ncores <- detectCores()
  workers <- floor(capacity * ncores / 100)
  
  cl <- makeCluster(workers)
  clusterExport(cl,
                c("w_distribution", "f_distribution", "mydf_list", "config"))
  clusterEvalQ(cl, {
    library("outbreaker2")
  })
  
  test_parLapply <- parLapply(cl, mydf_list, outbreaker_f)
  
  stopCluster(cl)
  
})

##timings
list(test_map_time =test_map_time, 
     test_future_map_time = test_future_map_time,
     test_lapply_time = test_lapply_time,
     test_parLapply_time = test_parLapply_time,
     test_future_lapply_time = test_future_lapply_time)




#distributions
distributions <- function(i){
  
  list(
    w_distribution = distcrete::distcrete(
      "gamma",
      shape = generation_time_params[["shape"]][i],
      scale = generation_time_params[["scale"]][i],
      interval = 1,
      w = 1
    ),
    
    f_distribution = distcrete::distcrete(
      "gamma",
      shape = incubation_params[["shape"]][i],
      scale = incubation_params[["scale"]][i],
      interval = 1,
      w = 1
    )
  )
}

lapply(1:3, distributions)




# COMPLETE LOOP -----------------------------------------------------------

# for n iterations:
#step 1  gen distributions
#step 2 run outbreaker
#step 3 bind all results

#strating with:
#step2
out_group <- function(i,
                       df_list,
                       date_column,
                       id_column,
                       outbreaker_configuration,
                       w_params,
                       f_params,
                      cluster) {
  
  w_distribution =  distcrete::distcrete(
    "gamma",
    shape = w_params[["shape"]][i],
    scale = w_params[["scale"]][i],
    interval = 1,
    w = 1
  )
  
  f_distribution = distcrete::distcrete(
    "gamma",
    shape = f_params[["shape"]][i],
    scale = f_params[["scale"]][i],
    interval = 1,
    w = 1
  )
  
  
  lapply(df_list[1:2],
         function(x) {
           outbreaker(
             data = outbreaker_data(
               dates = x[[date_column]],
               ids = x[[id_column]],
               w_dens = w_distribution$d(1:100),
               f_dens = f_distribution$d(1:100)
             ),
             config = outbreaker_configuration
           )
         })
  
}



get_clean_results <- function(){
  #step1
  result <- lapply(1:3, out_group,
                   df_list = mydf_list ,
                   date_column = "date_onset",
                   id_column = "id",
                   outbreaker_configuration = config,
                   w_params = generation_time_params ,
                   f_params = incubation_params )
  
  
  #step3
  result_flat<- unlist(result, recursive = FALSE)
  result_condensed <- tapply(result_flat, names(result_flat), dplyr::bind_rows) #x, index, function
  return(result_condensed)
}

clean_results <- get_clean_results()









# parLapply  -------------------------------------------------------------
#step2
out_group <- function(i,
                      df_list,
                      date_column,
                      id_column,
                      outbreaker_configuration,
                      w_params,
                      f_params,
                      cluster_capacity = 60){
 
  ncores <- detectCores()
  workers <- floor(cluster_capacity*ncores/100)
  cl <- makeCluster(workers)
  clusterExport(cl=cl, varlist=c("df_list",
                                 "date_column",
                                 "id_column",
                                 "outbreaker_configuration",
                                 "w_params",
                                 "f_params",
                                 "cluster_capacity"), 
                envir=environment())
  clusterEvalQ(cl,{
    library("outbreaker2")
  })

  w_distribution =  distcrete::distcrete(
    "gamma",
    shape = w_params[["shape"]][i],
    scale = w_params[["scale"]][i],
    interval = 1,
    w = 1
  )
  
  f_distribution = distcrete::distcrete(
    "gamma",
    shape = f_params[["shape"]][i],
    scale = f_params[["scale"]][i],
    interval = 1,
    w = 1
  )
  
  
  out <- parLapply(cl,
            df_list[1:2],
         function(x) {
           outbreaker2::outbreaker(
             data = outbreaker2::outbreaker_data(
               dates = x[[date_column]],
               ids = x[[id_column]],
               w_dens = w_distribution$d(1:100),
               f_dens = f_distribution$d(1:100)
             ),
             config = outbreaker_configuration
           )
         })
  
  
  return(out)
  stopCluster(cl)
  
}


get_clean_results <- function(){
  
  result <- lapply(1:3, out_group,
                   df_list = mydf_list ,
                   date_column = "date_onset",
                   id_column = "id",
                   outbreaker_configuration = config,
                   w_params = generation_time_params ,
                   f_params = incubation_params ) # how do I parallelise this
  
  
  #step3
  result_flat<- unlist(result, recursive = FALSE)
  result_condensed <- tapply(result_flat, names(result_flat), dplyr::bind_rows)
  
  return(result_condensed)
  
}

final_test <- get_clean_results()




