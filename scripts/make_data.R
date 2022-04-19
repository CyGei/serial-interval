idmaker <- function(x) #from stackoverflow
{
  max.val = x*100
  count <- nchar(as.character(max.val))                       # find out how many 'numbers' each ID will have after the letter
  size <- paste("%0",count,"d",sep="")                        # set the variable to be fed into 'sprintf' to ensure we have leading 0's
  lets <- toupper(sample(letters,x, replace=T))               # randomising the letters 
  nums <- sprintf(size,sample(1:max.val)[1:x])                # randominsing the numbers, and ensuing they all have the same number of characters
  ids <- paste(lets,nums,sep="")                              # joining them together
  return(ids)
}



draw_labels <- function(n = 1, size = 6) { #from simulacr
  stringi::stri_rand_strings(n = n, length = size, pattern = "[A-Za-z0-9]")
}


dist_plot <- function(data, group_var, date_var){
  s <- split(data, f = data[[group_var]]) 
  d<- lapply(s, function(x) dist(x[[date_var]]))
  vec <- lapply(d, function(x) as.vector(x))
  vals <- Reduce(c, vec)
  
  
  all_vals<- c(-vals, vals)
  
  plot <- data.frame(vals = all_vals) %>% 
    filter(vals>= -20 & vals <= 20 ) %>% 
    ggplot()+
    aes(x = vals)+
    geom_bar()
  
  print(plot)
  return(table(all_vals))
}

# 
# 
# IDs <- idmaker(10)
# IDs <- draw_labels(100) 
# unique_IDs <- unique(IDs)
# 
# vec_household_id <- vector()
# for (i in unique_IDs) {
#   random_n <- round(runif(1, min = 2, max = 6)) #assign randomly household size 1-6
#   household_id = rep(i, random_n) # rep household ID by household size
#   vec_household_id = c(vec_household_id, household_id)
# }  
# 
# 
# data <- data.frame(household_id = vec_household_id) %>% 
#   group_by(household_id) %>% 
#   mutate(individual_id = paste0(household_id, "_", row_number())) %>% #_1..._6 for household member id
#   ungroup() %>% 
#   group_by(individual_id) %>% 
#   mutate(start_dt = sample(seq(as.Date('2022/01/01'), as.Date('2022/01/20'), by="day"), 1), #sample any date between that period
#          start_dt = as.Date(start_dt)) %>% 
#   ungroup() %>% 
#   arrange(household_id, start_dt)
# 
# dist_plot(data)
# 
# data0 <- data %>%
#   mutate(change_to_same_date = ifelse(row_number() <= 50, TRUE, FALSE)) %>% #simulate extra 0values
#   group_by(household_id) %>% 
#   mutate(start_dt = as.Date(ifelse(change_to_same_date==TRUE, start_dt[1], start_dt), origin = "1970-01-01"), # edit for day-1 model
#          start_dt_minus1 = if_else(change_to_same_date == TRUE & row_number()==1, start_dt -1, start_dt))
# 
# dist_plot(data0)
# 
# rm(i, household_id, IDs, random_n, unique_IDs, vec_household_id)
# 
# 
# input_folder <- paste0("input data/",
#                         Sys.Date(),
#                         "/")
# input_folder <- paste0(getwd(), "/", input_folder)
# 
# dir.create(input_folder, recursive = TRUE)
# 
# 
# write.csv(data, paste0(input_folder,"/data.csv"))
# write.csv(data0, paste0(input_folder, "/data0.csv"))
# 
# cat(blue("results saved in:\n",
#          input_folder, "\n"))
