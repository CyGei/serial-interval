library(simulacr)

simulate_outbreak_groups <- function(n_groups,
                                     min_group_size,
                                     max_group_size,
                                     duration = 20,
                                     R_values,
                                     dist_incubation,
                                     dist_infectious_period,
                                     dist_reporting = NULL) {
  data <- data.frame()
  for (i in 1:n_groups) {
    temp <- simulate_outbreak(
      duration = duration,
      population_size = round(runif(1, min_group_size, max_group_size)),
      R_values = R_values,# random values on [1;3]
      dist_incubation = dist_incubation,
      dist_infectious_period = dist_infectious_period,
      dist_reporting = dist_reporting
    )
    temp <- temp %>% mutate(group_id = paste0("G", i),
                            id = paste0(group_id,"_", id),
                            source = paste0(group_id,"_", source)) %>% 
      relocate(group_id)
    
    data <- rbind(data, temp)
  }
  to_remove <- data %>%
    group_by(group_id) %>%
    count() %>%
    filter(n < min_group_size)
  to_remove
  
  data <-
    data %>% filter(!(group_id %in% to_remove$group_id))
  return(data)
  
}



  
  