# INFO --------------------------------------------------------------------
## input_df ==> the linelist
## out_result_list ==> a list containing all outbreaker results per household
## all_si_df ==> a dataframe containing all serial interval values per household



investigate <- function(ID) {
  
  linelist  <-
    input_df %>% filter(household_id == ID)
  
  cat("##########\n",
      "start dates: \n", 
      as.character(linelist$start_dt2),
      "\n##########\n")
  
  serial_interval <-
    all_si_df %>%  filter(Household == ID) %>%
    summarise(
      mean = mean(serial_interval),
      min = min(serial_interval),
      max = max(serial_interval),
      median = median(serial_interval)
    )
  
  breaks <- all_si_df %>%  filter(Household == ID) %>% 
    count(serial_interval) %>% .$serial_interval %>% as.vector()
  PLOT_serial_interval <-
    all_si_df %>%  filter(Household == ID) %>%
    ggplot(aes(x = serial_interval)) +
    geom_bar(width = 1)+
    scale_x_continuous(breaks = breaks )
  
  outbreaker_result <- out_result_list[[ID]]
  
  ALPHA_outbreaker_result <-
  out_result_list[[ID]] %>% select(starts_with("alpha_")) %>% 
    lapply(., function(.) table(.,  useNA = "always")) 
  
  
  PLOT_outbreaker_result <-
    plot(out_result_list[[ID]], type = "t_inf")
  
  investigate_list <<- list(linelist = linelist,
                            serial_interval = serial_interval, 
                            outbreaker_result = outbreaker_result, 
                            ALPHA_outbreaker_result = ALPHA_outbreaker_result,
                            PLOT_outbreaker_result = PLOT_outbreaker_result,
                            PLOT_serial_interval = PLOT_serial_interval)
  return(investigate_list)
}



# HOUSEHOLDS WITH 0  ------------------------------------------------------
## We first check which households have at least one observation 
## with a serial interval == 0 days

households0_ids <- all_si_df %>%
  group_by(Household) %>%
  filter(serial_interval == 0) %>%
  .$Household %>% unique()

length(households0_ids)
## There are 53 households that have 0 day serial interval values
## lets describe these households:



#GET info on number of cases, symptom onset dates
households0_n_cases <- 
  input_df %>%
  filter(household_id %in% households0_ids) %>%
  group_by(household_id) %>%
  summarise(household_n_cases = household_n_cases[1],
            min_start_date = min(start_dt2),
            max_start_date = max(start_dt2),
            date_interval = max_start_date - min_start_date) 

households0_n_cases %>% 
  arrange(household_n_cases) # minimum 3 cases (cf rule)
households0_n_cases %>% 
  arrange(desc(date_interval)) #very long periods of "household outbreak"
investigate("14565") #how could individual 1 infect any other individual?

households0_si <- 
  all_si_df %>% 
  filter(Household %in% households0_ids) %>% 
    group_by(Household) %>% 
    summarise(mean = mean(serial_interval),
              min = min(serial_interval),
              max = max(serial_interval),
              median = median(serial_interval), 
              perc0 = sum(serial_interval==0)/length(serial_interval) * 100) %>% 
  rename(household_id = Household)

households0_si %>% 
  arrange(perc0)
investigate("54624")

households0_si %>% 
  arrange(desc(perc0))
investigate("26541")


households0_master <-
  merge(households0_n_cases,
        households0_si,
        by = "household_id",
        all.x = TRUE)

households0_master %>% 
  arrange(desc(date_interval)) 

all_si_df %>% 
  filter(Household %in% households0_master$household_id[1:14]) %>% 
  ggplot()+
  aes(x = serial_interval, fill = Household)+
  geom_bar(position = "stack", width = 0.4)+ # position_dodge(width = 0.5)
  scale_x_continuous(breaks = -10:20)+
  geom_vline(aes(xintercept = 0), col = "red")+
  facet_wrap(~Household)



# HOUSEHOLDS WITHOUT 0  ------------------------------------------------------
## We first check which households have at least one observation 
## with a serial interval == 0 days

households_no0_ids <- all_si_df %>%
  group_by(Household) %>%
  filter(!any(serial_interval == 0)) %>%
  .$Household %>% unique()

length(households_no0_ids)
## There are 53 households that have 0 day serial interval values
## lets describe these households:

households_no0_n_cases <- 
  input_df %>%
  filter(household_id %in% households_no0_ids) %>%
  group_by(household_id) %>%
  summarise(household_n_cases = household_n_cases[1],
            min_start_date = min(start_dt2),
            max_start_date = max(start_dt2),
            date_interval = max_start_date - min_start_date) 
households_no0_n_cases


households_no0_si <- 
  all_si_df %>% 
  filter(Household %in% households_no0_ids) %>% 
  group_by(Household) %>% 
  summarise(mean = mean(serial_interval),
            min = min(serial_interval),
            max = max(serial_interval),
            median = median(serial_interval)) %>% 
  rename(household_id = Household)
households_no0_si


households_no0_master <-
  merge(households_no0_n_cases,
        households_no0_si,
        by = "household_id",
        all.x = TRUE)

households_no0_master %>% 
  arrange(desc(date_interval)) 

all_si_df %>% 
  filter(Household %in% households_no0_master$household_id[1:100]) %>% 
  ggplot()+
  aes(x = serial_interval, fill = Household)+
  geom_bar(position = "stack", width = 0.4)+ # position_dodge(width = 0.5)
  scale_x_continuous(breaks = -10:20)+
  geom_vline(aes(xintercept = 0), col = "red")+
  theme(legend.position = "none")

investigate("12506")






# SIMULATION --------------------------------------------------------------
## 2 indiviudals with a start date on 1st of Jan and a start date on 20th of Jan
## 3rd individual with start date from 2nd to 21th of Jan



input_dip_df <- data.frame()
for (i in 1:20) {
  cat(green("i is", i, "\n"))
  
  date_id1 <- c(as.Date("2020-01-01"))
  date_id3 <- c(as.Date("2020-01-20"))
  date_id2 <- date_id1 + i
  dates <- c(date_id1, date_id2, date_id3)
  ids <- c("id1", "id2", "id3")
  scenario<- paste0("scenario_", i)
  
  temp_df <- data.frame(dates = dates,
             ids = ids,
             scenario = scenario,
             i = i)
  
  input_dip_df<- rbind(input_dip_df, temp_df)
}

input_dip_df <- input_dip_df %>% 
  group_by(scenario) %>% 
  arrange()
input_dip_df

unique_scenarios <- unique(input_dip_df$scenario)
unique_scenarios

out_result_dip <- list()
for (i in unique_scenarios) {
  
  cat(green("i is", i, "\n" ))
  scenario_i <- i
  temp_df <- subset(input_dip_df, scenario == scenario_i)
  
  #outbreaker data:
  out_data <- outbreaker_data(
    dates = temp_df$dates,# dates of onset
    ids = temp_df$ids,
    w_dens = gi$d(1:100),#generation time distribution
    f_dens = incub$d(1:100) # incubation time distribution
  )
  
  #RUN THE MODEL
  out <- outbreaker(data = out_data,
                    config = config)
  
  out_result_dip[[scenario_i]] <- out # save results
  
}

saveRDS(out_result_dip,
        file = paste0(home_directory, "out_result_dip.RData"))

out_result_dip<- readRDS("S:/CoronaWatch/Working/Individual_working_folders/Cy/serial-interval/out_result_dip.RData")


for (i in unique(names(out_result_dip))) {
  out_result_dip[[i]] <- out_result_dip[[i]] %>% filter(step > 500) # burnin
}



extract_serial_interval <- function(outbreaker_result, input_data, burnin = 500) {
  
  mat_infectors <- outbreaker_result %>% 
    filter(step > burnin) %>% 
    select(dplyr::starts_with("alpha")) %>% 
    as.matrix()
  
  vec_infectors <- mat_infectors %>% 
    as.vector()
  
  n_cases <- ncol(mat_infectors)
  vec_infectees <- rep(seq_len(n_cases), each = nrow(mat_infectors))
  
  onset_infectees <- input_data$dates[vec_infectees]
  onset_infectors <- input_data$dates[vec_infectors]
  
  serial_interval <- as.integer(onset_infectees - onset_infectors)
  serial_interval <- serial_interval[!is.na(serial_interval)]
}


input_df_dip_list <- split(input_dip_df, f = input_dip_df$scenario)

list_serial_interval_dip <- list()
for (i in unique(names(out_result_dip))) {
  list_serial_interval_dip[[i]] <-
    data.frame(
      serial_interval = extract_serial_interval(out_result_dip[[i]], input_df_dip_list[[i]])) 
}

all_si_dip <- bind_rows(list_serial_interval_dip, .id = "scenario") 

mean(all_si_df$serial_interval)
sd(all_si_df$serial_interval)
Rmisc::CI(all_si_df$serial_interval, ci = 0.95)

all_si_dip %>% 
  ggplot()+
  aes(x = serial_interval, fill = scenario)+
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")


all_si_dip %>% 
  group_by(scenario) %>%
  summarise( perc0 = sum(serial_interval==0)/length(serial_interval) * 100)
## scenario 11 has 0 values

ggpubr::ggarrange(incub$plot +
                    xlim(0, 20) +
                    labs(title = "incubation period"),
                  gi$plot +
                    xlim(0, 15) +
                    labs(title = "generation time"))

##### INVESTIGATE SCENARIO 11
input_dip_df %>% 
  filter(scenario == "scenario_19")

all_si_dip %>%  filter(scenario == "scenario_19") %>%
  count(serial_interval)

out_result_dip[["scenario_19"]] %>% select(starts_with("alpha_")) %>% 
  lapply(., function(.) table(., useNA = "always")) 

plot(out_result_dip[["scenario_19"]], type = "t_inf")



# HOW MANY REPORT SYMPTOMS ON THE SAME DATE -------------------------------
all_si_df %>% 
  ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")+
  scale_x_continuous(n.breaks = 40 )


households_with_same_date <- input_df %>%
  group_by(household_id, start_dt2) %>%
  count() %>%
  filter(n >= 2) %>%
  .$household_id %>%
  unique()

length(households_with_same_date)

all_si_df %>% 
  filter(Household %in% households_with_same_date) %>% 
  ggplot()+
  aes(x = serial_interval, fill = Household)+
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")

## If we remove households_with_same_date from the all_si_df, 
## there shouldn't be any 0 values

all_si_df %>% 
  filter(!(Household %in% households_with_same_date)) %>% 
  ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")


invest <- list()
for (i in households_with_same_date) {
  invest[[i]] <- investigate(i)
}


for (i in unique(input_df$household_id)) {
  print(i)
  temp_df <- subset(input_df, household_id == i)
  temp_df %>% 
    mutate(diff_day = start_dt2 - lag(start_dt2)) %>% View()
}

input_df2 <- input_df %>% group_by(household_id) %>% 
  mutate(diff_day = start_dt2 - lag(start_dt2))

input_df2 %>% count(diff_day) %>% filter(diff_day == 1) %>% View()

#pairwise distance!!!!!
# USE DIST to compute all the date differences, dist()

input_df %>% group_by(household_id) %>% dist(start_dt2)


dist_results = list()
for (i in unique(input_df$household_id)) {
  temp = subset(input_df, household_id == i)
  dist_results[[i]] <- dist(temp$start_dt2) %>% as.matrix()
  
}

test <- input_df %>% 
  select(household_id, start_dt2) %>% 
  split(.$household_id)  

dist_results2 <- map(test, ~dist(.x$start_dt2))
vec <- lapply(dist_results2, function(x) as.vector(x)) %>% Reduce(c,.)


test <- df %>% 
  select(household_id, start_dt2) %>% 
  split(.$household_id)  
dist_results2 <- map(test, ~dist(.x$start_dt2))
vec2 <- lapply(dist_results2, function(x) as.vector(x)) %>% Reduce(c,.)
table(vec2)

whole_data <- data.frame(si = vec2) %>%
  ggplot(aes(x = si)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 50, 1), limits = c(NA, 50))+
  ggtitle("whole data")

partial_data <-data.frame(si = vec) %>%
  ggplot(aes(x = si)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 50, 1), limits = c(NA, 50))+
  ggtitle("patial data")


ggpubr::ggarrange(whole_data, partial_data)
# FIRST 20 HOUSEHOLDS -----------------------------------------------------

sample_df <-all_si_df %>% 
  filter(Household %in% unique(all_si_df$Household)[1:20] )  


sample_df%>% 
  ggplot()+
  aes(x = serial_interval, fill = Household) +
  geom_bar(width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")
  

sample_df %>% 
  group_by(Household) %>%
  summarise( perc0 = sum(serial_interval==0)/length(serial_interval) * 100)

investigate("12589")
plot(out_result_list[["12589"]], type = "alpha")
investigate("33424")





# TESTS -------------------------------------------------------------------


dates1<- as.Date(c("2020-09-17","2020-09-17", "2020-09-22")) #ok
dates2<- as.Date(c("2020-09-17","2020-09-17", "2020-09-17")) #not ok
dates3<- as.Date(c("2020-09-17","2020-09-20", "2020-09-20")) #ok
dates4<- as.Date(c("2020-09-17","2020-09-17")) # not ok
dates5<- as.Date(c("2020-08-01","2020-09-26", "2020-09-26", "2020-09-26")) #ok
dates6<- as.Date(c("2020-09-16","2020-09-17")) #ok
dates7<- as.Date(c("2020-09-17","2020-09-17", "2020-09-17","2020-09-20")) # ok


dates2<- as.Date(c("2020-09-17","2020-09-17", "2020-09-17")) #not ok

dates2 %>% as.numeric()

out_data <- outbreaker_data(
  dates = c(1, 1.1, 1), # dates of onset
  w_dens = gi$d(1:100),#generation time distribution
  f_dens = incub$d(1:100) # incubation time distribution
)

test <- outbreaker(data = out_data, config = config )
View(test)
test %>% select(starts_with("alpha_")) %>% 
  lapply(., function(.) table(.,  useNA = "always")) 

plot(test, type = "alpha")
out_result_list<- list()
out_result_list[["test"]] <- test

input_df <- data.frame(start_dt2 = dates7, nVar = "omicron")
input_df_list <- list()
input_df_list[["test"]] <- input_df

     