

df <- raw_df %>%                                  # read raw data
  drop_na(household_id) %>%                       # drop unknown households
  filter(swaboutcome == "positive") %>%             # filter for positive cases
  mutate(
    across(c(start_dt, end_dt), ~ as.Date(.x, format = "%d%b%Y")),
    across(c(illnessid, newID,household_id, swabbed, swaboutcome), 
      as.character),
    nVar = as.character(nVar),
    nVar = case_when(
      nVar == "" ~ "[0] Unknown",
      nVar == "[0] Wild Type" ~ "[1] Wild Type",
      nVar == "[1] Alpha" ~ "[2] Alpha",
      nVar == "[2] Delta" ~ "[3] Delta",
      nVar == "[3] Omicron" ~ "[4] Omicron"
    )
  ) %>%
  group_by(household_id) %>%
  arrange(start_dt, .by_group = TRUE) %>%
  mutate(case_type = ifelse(row_number() == 1, "index_case", "secondary_case")) %>%
  as.data.frame()




df <- df %>%
  dplyr::select(
    illnessid,
    newID,
    household_id,
    start_dt,
    end_dt,
    swabbed,
    swaboutcome,
    nVar,
    region,
    sex_bin,
    hh_age_on_entry,
    age3,
    case_type
  ) 

# 14 day FOLLOW-UP --------------------------------------------------------
#removing households where  index case occurs within
#two weeks of survey data to allow for complete follow-up

last_update <-
  as.Date(last_update)  #extract date of last data update

#get the survey date (1st monday of from the last update date)
last_monday <-
  function(x) {
    day<- 7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
    return(day)
  }

date_limit <-
  last_monday(last_update) - 14 #2 weeks before our survey date

households_to_exclude <- df %>%
  filter(case_type == "index_case") %>% #get all index cases
  filter(start_dt >= date_limit) %>%  # extract those who's symptom onset date is within 2 weeks of survey date
  .$household_id %>% unique()          # extract the household ids


#remove households where index case reports a symptom onset within 2 weeks of survey date
df <- df %>%
  filter(!(household_id %in% households_to_exclude))

rm(last_monday, households_to_exclude, date_limit)



cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("clean.R ran successfully\n") %+%
      "##################################################\n"
  )
)
