

extract_serial_interval <- function(outbreaker_result, input_data, date_col, burnin = 500) {
  
  mat_infectors <- outbreaker_result %>% 
    filter(step > burnin) %>% 
    select(dplyr::starts_with("alpha")) %>% 
    as.matrix()
  
  vec_infectors <- mat_infectors %>% 
    as.vector()
  
  n_cases <- ncol(mat_infectors)
  vec_infectees <- rep(seq_len(n_cases), each = nrow(mat_infectors))
  
  onset_infectees <- input_data[[date_col]][vec_infectees]
  onset_infectors <- input_data[[date_col]][vec_infectors]
  
  serial_interval <- as.integer(onset_infectees - onset_infectors)
  serial_interval <- serial_interval[!is.na(serial_interval)]
}

