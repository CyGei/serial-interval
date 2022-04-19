
## This is the overall serial interval,
## looks pretty normal except for the dip at 0
all_si_df %>% 
  ggplot()+
  aes(x = serial_interval)+
  geom_histogram(aes(y = ..density..), col = "white")+
  stat_function(fun = dnorm, args = list(mean = mean(all_si_df$serial_interval),
                                         sd = sd(all_si_df$serial_interval)),
                color = "red")

mean(all_si_df$serial_interval)
sd(all_si_df$serial_interval)

empirical_dens <-
  approxfun(density(all_si_df$serial_interval)) # get empirical densities
f_emp<- empirical_dens(-10:20)


x = -10:20
x
f_norm <- dnorm(x, mean(all_si_df$serial_interval),sd(all_si_df$serial_interval))
hist(all_si_df$serial_interval, col = scales::alpha("black", 0.3), breaks = -10:20, freq = FALSE, border = "white")
lines(x,f_norm, col = "red")
lines(x,f_emp, col = "blue" )
## Let's get the normal curve 
## density value at 0

#EnvStats::demp(x, all_si_df$serial_interval)


f_norm[10]

# dist_norm<- distcrete(
#   "norm",
#   mean(all_si_df$serial_interval),
#   sd(all_si_df$serial_interval),
#   interval = 1,
#   w = 1) # discretize the normal curve
# dist_norm$d(0)





house_id = "40638"

si_0 <- all_si_df %>%
  filter(Household == house_id) 
si_0 %>% count(serial_interval)



data.frame( x = c(-3:3),
            empirical = empirical_dens(-3:3),
            normal = normal_curve$d(-3:3)) %>% 
  ggplot(aes(x = x))+
  geom_line(aes( y = normal), col = "red")+
  geom_point(aes(y = empirical))
ggplot()+
  aes(x = serial_interval)+
  geom_histogram(aes(y = ..scaled..))+
  scale_x_continuous(breaks = as.numeric(unique(si_0$serial_interval)))




  geom_histogram(aes(y = ..density..), col = "white")+
  stat_function(fun = dnorm, args = list(mean = mean(all_si_df$serial_interval),
                                         sd = sd(all_si_df$serial_interval)),
                color = "red")

empirical_dens <-
  approxfun(density(si_0$serial_interval)) # get empirical densities

empirical_dens0 <- empirical_dens(0)
empirical_dens0


data.frame(
  norm = normal_curve$d(-10:10),
  xval = c(-10:10),
  empirical = empirical_dens(-10:10)
) %>%
  ggplot() + aes(x = xval) + geom_line(aes(y = norm), col = "red") +
  geom_col(aes(y = empirical))
 
 
 

household_si0 <- all_si_df %>% 
  group_by(Household) %>% 
  filter(!any(serial_interval == 0)) %>% 
  .$Household %>% unique()
household_si0

all_si_df %>% filter(Household == "37174") %>% count(serial_interval)

all_si_df %>%
  filter(Household %in% household_si0 ) %>% 
  select(serial_interval, Variant) %>%
  group_by(Variant) %>%
  count(serial_interval) %>%
  ggplot() +
  aes(x = serial_interval, y = n, fill = Variant) +
  geom_col(col = "black") +
  # geom_line(size = 0.5) +
  # geom_area(alpha = 0.5 ) +
  facet_wrap( ~ Variant, scales = "free_y", ncol = 1) +
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "0")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "1")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "2")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "3")), 
             aes(xintercept = mean(serial_interval)))+
  geom_vline(data =
               all_si_df %>%
               filter(str_detect(Variant, "4")), 
             aes(xintercept = mean(serial_interval)))

# si_0 %>% 
#   #count(si) %>% 
#   ggplot()+
#   #aes(x = si, y = n)+
#   geom_histogram(aes(x = serial_interval, y = ..density..))+
#   stat_function(fun = dnorm,
#                 args = list(mean = 0,
#                             sd = 1), col = "red")+
#   geom_line()
# 
# 
# hist(si_0$serial_interval, prob = TRUE)
# lines(dist$d(-5:5), col = "red")
# 
# fit <-
#   MASS::fitdistr(si_0$serial_interval, "normal") # fit a normal curve
# param <- fit$estimate # get normal curve estimates
# dist <- distcrete("norm",
#                   param[1],
#                   param[2], interval = 1, w = 1)



dist <- distcrete("norm",
                  2,
                  3, interval = 1, w = 1) # discretize the normal curve

data.frame(
  norm = dist$d(-10:10),
  xval = c(-10:10),
  empirical = empirical_dens(-10:10)
) %>%
  ggplot() + aes(x = xval) + geom_line(aes(y = norm), col = "red")+ 
  geom_col(aes(y = empirical))

theoretical_dens0 <- dist$d(0)
theoretical_dens0

household_with_dips <- vector()
household_without_dips <- vector()
plots_with_dips <- list()
#43622

for(i in household_si0) {
  print(i)
  si_0 <- all_si_df %>%
    filter(Household == i) 
  
  empirical_dens <-
    approxfun(density(si_0$serial_interval)) # get empirical densities
  
  empirical_dens0 <- empirical_dens(0)
  cat(yellow("empri", empirical_dens0))
  cat(yellow("    diff = ", empirical_dens0 - theoretical_dens0))
  
  if (empirical_dens0 < 6 * theoretical_dens0) {
    cat(red("\n", "!! dip at 0 for household", i, "\n"))
    household_with_dips[[i]] <- c(i)
    plots_with_dips[[i]] <- si_0 %>%
      ggplot() +
      geom_bar(aes(x = serial_interval))
  } else{
    cat(green("\n", "no significant dip for household", i, "\n"))
    household_without_dips[i] <- c(i)
    
  }
}

household_with_dips

results_with_dip <- list()
for ( i  in household_with_dips) {
  print(i)
  results_with_dip[[i]] <- out_result_list[grepl(i, names(out_result_list))]
  
}

all_si_df %>% 
  filter(Household %in% household_with_dips[1]) %>% 
  select(serial_interval) %>%
  count(serial_interval) %>%
  ggplot() +
  aes(x = serial_interval, y = n) +
  geom_col()


fit <- fitdistrplus::fitdist(test$si, "norm")
plot(fit)
test %>% 
  ggplot()+
  geom_histogram(aes(x = si, y = ..density.. )) +
  geom_line(aes(y = dnorm(test$si, param[1], param[2]), x = si))

dens<- density(norm)
norm <- dnorm(test$si, param[1], param[2])
dist <- distcrete("norm", 
                  param[1], 
                  param[2], interval = 1, w = 1)

dist$d(0)




# DIP AT 0 ----------------------------------------------------------------

unique_households <-unique(all_si_df$Household) 
unique_households

all_si_df %>% 
  filter(Household %in% sample(unique_households, 20)) %>% 
  ggplot()+
  aes(x = serial_interval, fill = Household)+
  geom_bar()#position = position_dodge()




has_0 <- all_si_df %>% 
  group_by(Household) %>% 
  filter(serial_interval == 0) %>%
  .$Household %>% unique()
no_0 <- all_si_df %>% 
  group_by(Household) %>% 
  filter(serial_interval != 0) %>%
  .$Household %>% unique()

all_si_df %>% 
  filter(Household %in% has_0) %>% 
  ggplot()+
  aes(x = serial_interval, fill = Household)+
  geom_bar(position = position_dodge(width = 0.5), width = 0.4)+
  scale_x_continuous(breaks = -10:20)

all_si_df %>% 
  filter(Household %in% no_0) %>% 
  ggplot()+
  aes(x = serial_interval, fill = Household)+
  geom_bar(position = position_dodge(width = 0.5), width = 0.4)+
  scale_x_continuous(breaks = -10:20)




results_has0 <- list()
for ( i  in has_0) {
  print(i)
  results_has0[[i]] <- out_result_list[grepl(i, names(out_result_list))]
}


input_df_has0 <- input_df %>% 
  filter(household_id %in% has_0)

has_0_n_cases<- input_df_has0 %>% 
  group_by(household_id) %>% 
  summarise(household_n_cases)
summary(has_0_n_cases$household_n_cases)





