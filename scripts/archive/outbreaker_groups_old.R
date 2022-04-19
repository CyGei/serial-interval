

outbreaker_groups <- function(df, group_column, individual_column, dates,w_params, f_params, outbreaker_config){
  start.time <- Sys.time()
  out_result_list <- list()
  unique_group_column <- unique(df[[group_column]])

  for (i in 1:length(unique_group_column)) {
    
    group <- unique_group_column[i]
    
    group_temp_df <- subset(df, df[[group_column]]==group)
    
    cat(
      green("Group ID:", "[", group, "]", "\n"),
      green("Progress:", "[", round((
        i / length(unique_group_column) * 100
      ), digits = 2), "%]", "\n")
    )
    
    
    out_all_combinations <- data.frame()
    for (j in 1:10) {
      cat(
        red("Current model:", "[", j, "]", "\n"),
        red("Progress:", "[", round((
          j / 10 * 100
        ), digits = 2), "%]", "\n")
      )
      
      
      
      w_distribution <- distcrete::distcrete(
        "gamma",
        shape = w_params[["shape"]][j],
        scale = w_params[["scale"]][j],
        interval = 1,
        w = 1
      )
      
      f_distribution <- distcrete::distcrete(
        "gamma",
        shape = f_params[["shape"]][j],
        scale = f_params[["scale"]][j],
        interval = 1,
        w = 1
      )
      

      
      #outbreaker data:
      out_data <- outbreaker_data(
        dates = group_temp_df[[dates]],
        ids = group_temp_df[[individual_column]],
        w_dens = w_distribution$d(1:100),
        f_dens = f_distribution$d(1:100) # incubation time distribution
      )
      
      #RUN THE MODEL
      out <- outbreaker(data = out_data,
                        config = outbreaker_config)
      
      out$w <-
        paste0("shape: ",
               w_params[["scale"]][j],
               " ",
               "scale: ",
               w_params[["scale"]][j])
      out$f <-
        paste0("shape: ",
               f_params[["shape"]][j],
               " ",
               "scale: ",
               f_params[["shape"]][j])
  
      out_all_combinations <- rbind(out_all_combinations, out)
    }
    
    out_result_list[[group]] <- out_all_combinations # save results
    
  }
  
  end.time <- Sys.time()
  runtime <- difftime(end.time, start.time, units = "mins")
  
  cat(blue(
    "Model runtime:\n",
    runtime,
    " minutes\n"
  ))
 return(out_result_list)
}






