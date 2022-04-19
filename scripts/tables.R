
# TABLES ------------------------------------------------------------------
theme_gtsummary_journal(journal = "jama") 

# DEMOGRAPHICS ---------------------------------------------------
table_demographics <- input_df %>%
  dplyr::mutate(
    ageG =
      dplyr::case_when(
        hh_age_on_entry < 40 ~ "0-39",
        40 <= hh_age_on_entry &
          hh_age_on_entry <= 64 ~ "40-64",
        hh_age_on_entry > 64 ~ "65 +"
      ),
    london = dplyr::case_when(
      region == "London" ~ "London",
      region == "" ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  ungroup() %>%
  select(ageG, london, sex_bin, nVar, start_dt2) %>% #age3,region,
  rename(
    Age = ageG,
    Sex = sex_bin,
    Region = london,
    Variant = nVar,
    "Date of symptom onset" = start_dt2
  )  %>%
  mutate(Sex = ifelse(Sex == "", NA, Sex),
         Region = ifelse(Region == "", NA, Region)) %>%
  tbl_summary(by = Variant,
              missing = "ifany",
              missing_text = "NA") %>%
  add_overall()


print(table_demographics)



# SERIAL INTERVAL BY VARIANTS -------------------------------------

ci_df <- data.frame()
for (i in unique(all_si_df$Variant)) {
  temp_i <- subset(all_si_df, Variant == i)
  ci <-
    DescTools::MeanCI(temp_i$serial_interval) %>% t() %>% as.data.frame()
  ci <- ci %>% mutate(Variant = i)
  ci_df <- rbind(ci_df, ci)
}

temp_table <- all_si_df %>% 
  group_by(Variant) %>% 
  summarise(no_of_pairs = n(),
            mean = round(mean(serial_interval), digits = 2),
            median = round(median(serial_interval), digits = 2),
            sd = round(sd(serial_interval), digits = 2)) 

table_variants <- merge( temp_table %>% arrange(Variant), 
                         ci_df[-1] %>% arrange(Variant),
                         by ="Variant", 
                         all.x = TRUE) %>% 
  mutate(lwr.ci = round(lwr.ci, digits = 2),
         upr.ci = round(upr.ci, digits = 2)) 


print(gt(table_variants))




# GAMMA DISTRIBUTIONS --------------------------------------------------------------
# 
# 
# all_dist <- data.frame()
# all_params <- data.frame()
# 
# for (var in unique(all_si_df$Variant)) {
#   temp_all_si_df <- subset(all_si_df, Variant == var)
#   si <- epitrix::fit_disc_gamma(temp_all_si_df$serial_interval)
#   
#   dist_temp<- si$distribution$d(-20:20) %>% as.data.frame() %>% 
#     rename("d" = ".") %>% 
#     mutate(Variant = var,
#            si = row_number())
#   all_dist<- rbind(all_dist, dist_temp)
#   
#   params_temp <- si$distribution$parameters %>% 
#     as.data.frame() %>% 
#     mutate(Variant = var)
#   all_params <- rbind(all_params, params_temp) 
# }
# 
# table_gamma_dist <- all_params %>% 
#   arrange(infector_nVar) %>% 
#   rename(Variant = infector_nVar) %>%
#   mutate(across(where(is.numeric), ~round(.x, digits = 3))) %>% 
#   relocate(Variant)
# 
# print(table_gamma_dist)


rm(i, ci, ci_df, temp_table)





cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("tables.R ran successfully\n") %+%
      "##################################################\n"
  )
)
