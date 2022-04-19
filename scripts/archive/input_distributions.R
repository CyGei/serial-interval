

# INCUBATION PERIOD -------------------------------------------------------

incub_mus <- c(4.5, 6.3, 8.5 )
incub_sds <- c(3.5, 0.6, 1.4)

incub_list <- list()
for( i in 1:length(incub_mus)){
  incub_list[[i]] <- VirusWatch::make_dist("gamma",
                                          incub_mus[i],
                                          incub_sds[i],
                                          w=1)

}

incub_values <- list()
for (i in 1:length(incub_list)) {
  incub_values[[i]] <- incub_list[[i]][["d"]](0:100)
}


x <- sample( 1:length(incub_values), length(incub_values[[1]]), replace = TRUE)
x
incub_random <- vector()
for (i in 1:length(x)){
  temp_x <- x[i]
  incub_random[i]<- incub_values[[temp_x]][i]
}





# GENERATION TIME ---------------------------------------------------------

gi_mus <- c(2, 2.8)
gi_sds <- c(1, 1.5)

gi_list <- list()
for( i in 1:length(gi_mus)){
  gi_list[[i]] <- VirusWatch::make_dist("gamma",
                                           gi_mus[i],
                                           gi_sds[i],
                                        w=1)
}


gi_values <- list()
for (i in 1:length(gi_list)) {
  gi_values[[i]] <- gi_list[[i]][["d"]](0:100)
  
}

x <- sample( 1:length(gi_values), length(gi_values[[1]]), replace = TRUE)
x
gi_random <- vector()
for (i in 1:length(x)){
  temp_x <- x[i]
  gi_random[i]<- gi_values[[temp_x]][i]
}




# PLOTS -------------------------------------------------------------------
plot_gi_df <- data.frame()
for (i in 1:length(gi_values)) {
  temp_gi_value <- gi_values[[i]] %>% 
    as.data.frame() %>% 
    rename(value = 1) %>% 
    mutate(day = row_number(),
           group = i)
  plot_gi_df<- rbind(plot_gi_df,temp_gi_value) 
}
gi_random <- gi_random/sum(gi_random) #rescale

gi_random_df <- as.data.frame(gi_random) %>% 
  rename(value = 1) %>% 
  mutate(day = row_number(),
         group = i+1)

plot_gi_df<- rbind(plot_gi_df,gi_random_df)
plot_gi_df %>% 
  ggplot()+
  aes(x = day, y = value, col = as.character(group), alpha = 0.6) + 
  geom_line(size = 2)+
  xlim(0,20)+
  theme_minimal()+
  ggtitle("generation time distributioms & rescaled random sample")




plot_incub_df <- data.frame()
for (i in 1:length(incub_values)) {
  temp_incub_value <- incub_values[[i]] %>% 
    as.data.frame() %>% 
    rename(value = 1) %>% 
    mutate(day = row_number(),
           group = i)
  plot_incub_df<- rbind(plot_incub_df,temp_incub_value) 
}


# normalise <- function(x){
#   return(
#     (x-min(x))/(max(x) - min(x))
#     )
# }

normalise(incub_random) %>% sum()
incub_random <- incub_random/sum(incub_random) #rescale

incub_random_df <- incub_random %>% 
  as.data.frame() %>% 
  rename(value = 1) %>% 
  mutate(day = row_number(),
         group = i+1)

plot_incub_df<- rbind(plot_incub_df,incub_random_df)
plot_incub_df %>% 
  ggplot()+
  aes(x = day, y = value, col = as.character(group), alpha = 0.6) + 
  geom_line(size = 2)+
  xlim(0,20)+
  theme_minimal()+
  ggtitle("incubation periods & rescaled random sample")




rm(i, incub_mus, incub_sds, incub_values, temp_x, x, gi_mus, gi_sds, gi_values)

cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("input_distributions.R ran successfully\n") %+%
      "##################################################\n"
  )
)
