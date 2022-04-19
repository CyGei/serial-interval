#FROM RAW ILLNESS EPISODES DATA
#
cat(red("############################\n",
        "       RAW DATA\n",
        "############################\n"))
cat("how many illnesses?",
    green(nrow(raw_df)),
    sep = "\n")

cat("how many individuals?",
    green(length(unique(raw_df$newID))),
    sep = "\n")


cat("how many households?",
    green(length(unique(raw_df$household_id))),
    sep = "\n")


#date interval
raw_df$start_dt2<- as.Date(raw_df$start_dt, format="%d%b%Y")
raw_df$end_dt2<- as.Date(raw_df$end_dt, format="%d%b%Y")
min(raw_df$start_dt2) 
max(raw_df$end_dt2)

cat("date interval?",
    green("from:", paste0(min(raw_df$start_dt2)),
    "to:", paste0(max(raw_df$start_dt2))),
    sep = "\n")


cat("how many swabbed?",
    green(paste0(table(raw_df$swabbed)[2]),"\n"),
    "Amongst swabbed, how many positive?",
    green(table(raw_df$swaboutcome)[3]),
    sep = "\n")

cat(red("\n ############################\n",
        "       INPUT DF\n",
        "############################\n"))

cat("how many positive individuals eligible for our analysis ?",
    green(length(unique(input_df$newID))),
    sep = "\n")

cat("how many  households?",
    green(length(unique(input_df$household_id))),
    sep = "\n")


cat("date interval from earliest COVID+ illness symptom onset to lastest?",
    green("from:", paste0(min(input_df$start_dt2) ),
    "to:", paste0(max(input_df$start_dt2))),
    sep = "\n")


cat("how many serial interval estimates ?",
   green(nrow(all_si_df)),
    sep = "\n")


