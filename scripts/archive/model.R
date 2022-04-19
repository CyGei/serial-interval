start.time <- Sys.time()

# MODEL RUN ---------------------------------------------------------------
out_result_list <- list()
unique_households <- unique(input_df$household_id)


for (i in 1:length(unique_households)) {

  household <- unique_households[i]
  
  cat(
    blue("Current i:", "[", i, "]", "\n"),
    green("Household ID:", "[", household, "]", "\n"),
    red("Progress:", "[", round((
      i / length(unique_households) * 100
    ), digits = 2), "%]", "\n")
  )
  
  
  #subset for unique household:
  hh_input_df <- input_df %>%
    filter(household_id == household) %>%
    arrange(fake_start_dt2)
  
  
  #outbreaker data:
  out_data <- outbreaker_data(
    dates = hh_input_df$fake_start_dt2,# dates of onset
    ids = hh_input_df$newID,
    w_dens = gi$d(1:100),#generation time distribution
    f_dens = incub$d(1:100) # incubation time distribution
  )
  
  
  
  #RUN THE MODEL
  out <- outbreaker(data = out_data,
                    config = config)
  
  out_result_list[[household]] <- out # save results
  
}


end.time <- Sys.time()

cat(red(
  "Model runtime:\n",
  difftime(end.time, start.time, units = "mins"),
  " minutes\n"
))

rm(pb, out, out_data, hh_input_df, end.time, start.time)


# SAVE RESULTS ------------------------------------------------------------

output_folder <- paste0("output data/",
                        last_update,
                        "/",
                        "outbreaker/")

output_folder <- paste0(getwd(), "/", output_folder)

dir.create(output_folder, recursive = TRUE)

saveRDS(out_result_list,
        file = paste0(output_folder, "out_result_list.RData"))

cat(blue("results saved in:\n",
         output_folder, "\n"))


cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("model.R ran successfully\n") %+%
      "##################################################\n"
  )
)
