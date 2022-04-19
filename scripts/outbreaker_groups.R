outbreaker_groups <-
  function(df,
           group_column,
           individual_column,
           dates,
           w_params,
           f_params,
           n,
           outbreaker_config){
    
    if (!(length(w_params[[1]]) > n & length(f_params[[1]]) > n &
          length(w_params[[2]]) > n & length(f_params[[2]]) > n )){
      stop("not enough parmeters to sample from, increase sampled parameters or decrease n")
    } else{
      unique_groups <- unique(df[[group_column]])
      out_all_runs <- data.frame()
      for (i in 1:n) {
        
        w_distribution <- distcrete::distcrete(
          "gamma",
          shape = w_params[["shape"]][i],
          scale = w_params[["scale"]][i],
          interval = 1,
          w = 1
        )
        
        f_distribution <- distcrete::distcrete(
          "gamma",
          shape = f_params[["shape"]][i],
          scale = f_params[["scale"]][i],
          interval = 1,
          w = 1
        )
        
        print_w <- paste0("param1:",
               w_params[[1]][i],
               " ",
               "param2: ",
               w_params[[2]][i])
        
        print_f <- paste0("param1: ",
               f_params[[1]][i],
               " ",
               "param2: ",
               f_params[[2]][i])
        cat(red("w = ", print_w, "\n",
                "f =", print_f, "\n"))
        
        
        out_single_run <- list()
        for (j in 1:length(unique_groups)) {
          
          group <- unique_groups[j]
          
          group_df <- subset(df, df[[group_column]]==group)
          
          #outbreaker data:
          out_data <- outbreaker_data(
            dates = group_df[[dates]],
            ids = group_df[[individual_column]],
            w_dens = w_distribution$d(1:100),
            f_dens = f_distribution$d(1:100) 
          )
          
          out <- outbreaker(data = out_data, config = outbreaker_config)
          
          out$group <- group
          
          out$w <- print_w
          out$f <- print_f
          
          cat(green("household:", group, "\n"))
          
          out_single_run[[group]] <- out
        }
        
        out_all_runs <- c(out_all_runs, out_single_run)
        
      }
      
    }
    out_bind_all_runs <- tapply(out_all_runs, names(out_all_runs), dplyr::bind_rows)
    return(out_bind_all_runs)
  }

