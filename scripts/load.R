
# FILES -------------------------------------------------------------------

ellen_directory <-
  "S:\\CoronaWatch\\Working\\Dashboard\\symptom profile\\Work\\_DATA_derived_realtime\\"

illness_episodes <-
  "symp-data04_v6_collapsed_no_PCR_repeats.csv" #name of file



# DATA INFO --------------------------------------------------------------

ellen <-
  file_index(directory = ellen_directory) #fetch real time directory

last_update <- ellen %>%
  filter(file_name == illness_episodes) %>% #extract file of interest
  slice(which.max(date_modified)) %>% .$date_modified # get file latest update

cat(blue(
  "Input data last updated on:\n",
  last_update,
  "\n",
  green$underline$bold(as.Date(Sys.Date()) - as.Date(last_update), "day(s) ago\n")
))



# READ DATA ---------------------------------------------------------------


raw_df <- ellen %>%
  filter(file_name == "symp-data04_v6_collapsed_no_PCR_repeats.csv") %>%
  slice(which.max(date_modified)) %>%
  .$file_path %>%
  read.csv()  # extract and read latest




# SAVE DATA ---------------------------------------------------------------


write.csv(raw_df,
          paste0("input data/raw_df_",
                 format(last_update, "%Y-%m-%d"),
                 ".csv"),
          row.names = F)


cat(green("Input data saved\n"))


rm(ellen, illness_episodes, ellen_directory)



cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("load.R ran successfully\n") %+%
      "##################################################\n"
  )
)
