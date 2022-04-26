
# SET-UP ---------------------------------------------------------------

home_directory <-"~/OneDrive - Imperial College London/VirusWatch/serial-interval/"
setwd(home_directory)
source("scripts/libraries.R")

# GENERATION AND INCUBATION DISTRIBUTIONS ---------------------------------

source("scripts/make_gamma_params.R") #generates random gamma distribution parameters 
                                      #from a range of values extracted from the literature
                                      # based on truncated normal sampling

incubation_params <- make_gamma_params(n = 1000, 
                                       min_mu = 4.9, 
                                       min_sd = 2.2, 
                                       max_mu = 6.7, 
                                       max_sd = 5.2)


generation_time_params <- make_gamma_params(n = 1000, 
                                            min_mu = 3.95, 
                                            min_sd = 1.51, 
                                            max_mu = 5.57, 
                                            max_sd = 2.23)



# DATA -----------------------------------------
# source("scripts/make_data.R") #generate fake data, min household size is 2 
# head(data0)
# dist_plot(data0) #plot all the possible serial inteval values using dist()

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


length(unique(data$group_id))
length(unique(data$id))
data %>% count(group_id) %>% summarise(mean = mean(n))

source("scripts/make_data.R")
dist_plot(data, 
          group_var = "group_id", 
          date_var = "date_onset") #if infector-infectee pair unknown

infector <- data$source
infectee <- data$id
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


source("scripts/outbreaker_groups.R")

out_result_list <- outbreaker_groups(
  df = data,
  group_column = "group_id",
  individual_column = "id",
  dates = "date_onset",
  w_params = generation_time_params,
  f_params = incubation_params,
  n = 10,
  outbreaker_config = config
)


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

data_list <- split(data, f = data$group_id)

list_serial_interval <- list()
for (i in unique(names(out_result_list))) {
  list_serial_interval[[i]] <-
    data.frame(
      serial_interval = extract_serial_interval(out_result_list[[i]], data_list[[i]], date_col = "date_onset")
      #,
      #Variant = data0_list[[i]]$nVar[1]
    ) #index case variant is the variant for the whole household
}

list_serial_interval
all_si_df <- bind_rows(list_serial_interval, .id = "Household") 

mean(all_si_df$serial_interval)
sd(all_si_df$serial_interval)
Rmisc::CI(all_si_df$serial_interval, ci = 0.95)
summary(all_si_df$serial_interval)


all_si_df %>% ggplot()+
  aes(x = serial_interval) +#fill = Household
  geom_bar(aes(y = ..prop..), width = 1)+
  geom_vline(aes(xintercept = 0), color = "red")





# PERT DISTRIBUTION -------------------------------------------------------
library("mc2d")

curve(dpert(x,min=-7,mean=0,max=4), from = -10, to = 11, lty=2 ,ylab="density")
data <- data %>% 
  mutate(rpert_date_onset = date_onset + round(rpert(1,min=-7,mean=0,max=4)))




# LATIN HYPERCUBE SAMPLING ------------------------------------------------

library("lhs")
# a design with 10 samples from 4 parameters
A <- randomLHS(10, 2) 
A
plot(A)
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B

incubation_params <- make_gamma_params(n = 10, 
                                       min_mu = 4.9, 
                                       min_sd = 2.2, 
                                       max_mu = 6.7, 
                                       max_sd = 5.2)
incubation_params
min_mu = 4.9
min_sd = 2.2 
max_mu = 6.7 
max_sd = 5.2

B[,1] <- qtruncnorm( A[,1],
          mean = median(min_mu:max_mu), 
          sd = median(min_sd:max_sd),
          a = min_mu, b = max_mu) #musample

min_cv <- min_sd/min_mu
max_cv <- max_sd/max_mu

B[,2] <- qtruncnorm(A[,2],
                        mean = median(min_cv:max_cv),
                        sd = median(min_sd:max_sd),
                        a = min_cv, b = max_cv) #cv sample

B

x <- data.frame(mu =B[,1], cv = B[,2] )

x
x %>% ggplot()+
  aes(x = mu, y = cv)+
  geom_point(col = "red")+
  scale_x_continuous(breaks = seq(min(4.8),max(6.6), 0.2))+
  scale_y_continuous(breaks = seq(min(0.4),max(0.8), 0.2))+
  coord_equal(xlim = c(4.8, 6.6),
              ylim = c(0.4, 0.8))+
  theme(panel.border = element_rect(fill = NA),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey", size = rel(0.5)),
        panel.ontop = TRUE)



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

