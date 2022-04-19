
# REPLACE DATES WITH QUARTER DAYS -----------------------------------------
t <- input_df %>%
  group_by(household_id) %>% 
  mutate(min_start_date = ifelse(start_dt2 == min(start_dt2), TRUE, FALSE),
         day = start_dt2 - min(start_dt2),
         quarter_day = day * 4,
         quarter_day_r = ifelse(min_start_date==TRUE, row_number(), quarter_day)) #note that row number starts at 1
View(t)  
t %>%
  filter(household_id == "37446") %>% select(newID, household_id, start_dt2, day, quarter_day, quarter_day_r) %>% View()

t %>%
  filter(household_id == "12589") %>% select(newID, household_id, start_dt2, day, quarter_day, quarter_day_r) %>% View()

#29289

# REPLACE densities -------------------------------------------------------

quarter_dist <- vector()
for (i in (1:100)) {
  val <- gi$d(i)/4
  vec<- rep(val, 4)
  quarter_dist<- c(quarter_dist, vec)
}
quarter_dist


quarter_dist2 <- vector()
for (i in (1:100)) {
  val <- incub$d(i)/4
  vec<- rep(val, 4)
  quarter_dist2 <- c(quarter_dist2, vec)
}
quarter_dist2 %>% plot()


input_df_quarter_test <- 
  input_df_quarter %>% 
  filter(household_id == "37446"| household_id == "12589")

# RUN MODEL ---------------------------------------------------------------



source("scripts/outbreaker_groups_quarter.R")
test_df <-input_df %>%  filter(household_id == "37446"| household_id == "12589")
test_df<- input_df %>% filter(household_id == "26883")
test_df %>%  select(newID, household_id, start_dt2, day, quarter_day, quarter_day_r) %>% View()
out_quarter <- outbreaker_groups_quarter(
  df = test_df,
  group_column = "household_id",
  individual_column = "newID",
  dates = "quarter_day_r",
  w_params = generation_time_params,
  f_params = incubation_params,
  outbreaker_config = config
)


plot(out_quarter[["12589"]], type = "alpha")
plot(out_quarter[["37446"]], type = "alpha")


for (i in unique(names(out_quarter))) {
  out_quarter[[i]] <- out_quarter[[i]] %>% filter(step > 500) # burnin
}

source("scripts/extract_serial_interval.R")

input_df_quarter_test_s <- split(input_df_quarter_test, f = input_df_quarter_test$household_id)

list_serial_interval_quarter <- list()
for (i in unique(names(out_quarter))) {
  list_serial_interval_quarter[[i]] <-
    data.frame(
      serial_interval = extract_serial_interval(out_quarter[[i]], input_df_quarter_test_s[[i]], date_col = "start_dt2"),
      Variant = input_df_quarter_test_s[[i]]$nVar[1]
    ) #index case variant is the variant for the whole household
}



quarter_si <- bind_rows(list_serial_interval_quarter, .id = "Household")


quarter_si %>% ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")+
  facet_wrap(~Household)


for (i in unique(quarter_si$Household)) {
  cat("\n")
  temp <- subset(quarter_si, Household == i)
  t <- table(temp$serial_interval, useNA = "always")
  print(i)
  print(t)
}


# days -1 model -----------------------------------------------------------
day1_df <- input_df %>% 
  filter(household_id == "37446" | household_id == "12589")

out_day1 <- outbreaker_groups(
  df = test_df,
  group_column = "household_id",
  individual_column = "newID",
  dates = "fake_start_dt2",
  w_params = generation_time_params,
  f_params = incubation_params,
  outbreaker_config = config
)


for (i in unique(names(out_day1))) {
  out_day1[[i]] <- out_day1[[i]] %>% filter(step > 500) # burnin
}

source("scripts/extract_serial_interval.R")

day1_df_s <- split(day1_df, f = day1_df$household_id)

list_serial_interval_day1 <- list()
for (i in unique(names(day1_df_s))) {
  list_serial_interval_day1[[i]] <-
    data.frame(
      serial_interval = extract_serial_interval(out_result_list[[i]], day1_df_s[[i]], date_col = "start_dt2"),
      Variant = day1_df_s[[i]]$nVar[1]
    ) #index case variant is the variant for the whole household
}

day1_si <- bind_rows(list_serial_interval_day1, .id = "Household")
day1_si

day1_si %>% ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")+
  facet_wrap(~Household)

for (i in unique(day1_si$Household)) {
  cat("\n")
  temp <- subset(day1_si, Household == i)
  t <- table(temp$serial_interval, useNA = "always")
  print(i)
  print(t)
}

