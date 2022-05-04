
# SET-UP ---------------------------------------------------------------

home_directory <-"~/OneDrive - Imperial College London/VirusWatch/serial-interval/"
setwd(home_directory)
source("scripts/libraries.R")


# LATIN HYPERCUBE SAMPLING ------------------------------------------------
source("scripts/LHS.R")

incubation_params <- get_lhs_params(
  min_mu = 4.9,
  min_sd = 2.2,
  max_mu = 6.7,
  max_sd = 5.2,
  n = 10
)

generation_time_params<- get_lhs_params(
  min_mu = 3.95,
  min_sd = 1.51,
  max_mu = 5.57,
  max_sd = 2.23,
  n = 10
)


# DATA -----------------------------------------

source("scripts/simulacr_groups.R") #generate fake data, min household size is 2 

incubation <- make_disc_gamma(incubation_params$shape[1], incubation_params$scale[1]) # numbers = unscaled PMF
infectious_period <- make_disc_gamma(10, 7) # distcrete object

data <- simulate_outbreak_groups(n_groups = 20,
                                 min_group_size = 2,
                                 max_group_size = 6,
                                 duration = 20,
                                 R_values = runif(100, 1, 6), # random values on [1;3]
                                 dist_incubation = incubation,
                                 dist_infectious_period = infectious_period)

View(data)


length(unique(data$group_id)) #how many groups
length(unique(data$id)) # how many individuals
data %>% count(group_id) %>% summarise(mean = mean(n)) # avg individuals per household

source("scripts/make_data.R")
dist_plot(data, 
          group_var = "group_id", 
          date_var = "date_onset") #if infector-infectee pair unknown dist()

infector <- data$source
infectee <- data$id
#given that simulacr tells you who infected whom,
#we can tell what's the true serial interval distribution:
onset_infectees <- data[["date_onset"]]
onset_infectors <- data[["date_onset"]][match(infector, infectee)]
serial_interval <- as.integer(onset_infectees - onset_infectors)
serial_interval <- serial_interval[!is.na(serial_interval)]

data.frame(serial_interval = serial_interval) %>% 
  ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(aes(y = ..prop..), width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")+
  ggtitle("TRUE SERIAL INTERVAL") 

mean(serial_interval)
median(serial_interval)
summary(serial_interval)

# OUTBREAKER MODEL --------------------------------------------------------

config <- create_config(
  move_kappa = FALSE,# do not look for missing cases
  
  move_pi = FALSE,  # reporting rate
  
  move_mu = FALSE,#mutation rate
  
  init_kappa = 1,#number of generations before the last sampled ancestor
  
  init_pi = 1,#100% of reporting rate = all cases are reported
  
  find_import = TRUE,# imported cases,
  
  init_tree = "star")



source("scripts/outbreaker_future_groups.R")

data_split <- split(data, f = data$group_id)

time.out_result_list <-system.time({
  out_result_list <- outbreaker_future_groups(
    n = 10,
    df_list =  data_split,
    date_column = "date_onset",
    id_column = "id",
    outbreaker_configuration = config,
    w_params = generation_time_params ,
    f_params = incubation_params,
    workers = 10)
})


# SAVE RESULTS ------------------------------------------------------------
output_folder <- paste0("output data/",
                        Sys.Date(),
                        "/",
                        "outbreaker/")
output_folder <- paste0(getwd(), "/", output_folder)

dir.create(output_folder, recursive = TRUE)

saveRDS(out_result_list,
        file = paste0(output_folder, "/out_result_list.rds"))

cat(blue("results saved in:\n",
         output_folder, "\n"))

# EXTRACT SERIAL INTERVAL -------------------------------------------------

source("scripts/extract_serial_interval.R")

list_serial_interval <- sapply(
  names(out_result_list),
  FUN = function(x) {
    data.frame( serial_interval = 
                  extract_serial_interval(out_result_list[[x]],
                                          data_split[[x]],
                                          date_col = "date_onset") )
  },
  simplify = FALSE,
  USE.NAMES = TRUE
)


all_si_df <- bind_rows(list_serial_interval, .id = "Household") 

mean(all_si_df$serial_interval)
sd(all_si_df$serial_interval)
Rmisc::CI(all_si_df$serial_interval, ci = 0.95)
summary(all_si_df$serial_interval)
summary(serial_interval)

all_si_df %>% ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(aes(y = ..prop..), width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")





# PERT DISTRIBUTION -------------------------------------------------------
library("mc2d")

curve(dpert(x,min=-7,mean=0,max=4), from = -10, to = 11, lty=2 ,ylab="density")
data <- data %>% 
  mutate(rpert_date_onset = date_onset + round(rpert(1,min=-7,mean=0,max=4)))



###### OLD ######
# # TABLES ------------------------------------------------------------------
# source("scripts/tables.R")
# table_variants
# 
# # FIGURES -----------------------------------------------------------------
#  all_si_df %>% ggplot()+
#   aes(x = serial_interval) +#fill = Household
#   geom_bar(width = 1)+
#   geom_vline(aes(xintercept = 0), color = "red")
# 
# source("scripts/figures.R")
# panel_conf_violin 
# fig_si_conf + theme(legend.position = "none") + scale_y_continuous(n.breaks = 10)
# fig_violin + theme(legend.position = "none") +  scale_y_continuous(n.breaks = 10)
# fig_ridgeline
# fig_histogram_ridges 
# 
# # RESULTS ----------------------------------------------------------------
# source("scripts/results.R")

