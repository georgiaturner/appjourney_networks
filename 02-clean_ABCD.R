
####################################################
# SCRIPT TO LOAD IN ABCD DATA FOR APP-JOURNEY REPLICATION

####################################################
# Set up workspace & define parameters
####################################################

rm(list = ls())

library(tidyverse)

# Which sessions from the longitudinal ABCD data? 

abcd_session <- "ses-07A" 

####################################################
# Load data
####################################################

####################################################
# read in EARS data: App usage events

appusage2_logs <- arrow::read_parquet("novel_technologies/ears/ears_appusage2_logs.parquet") %>%
  filter(session_id == abcd_session) %>%
  
  arrange(participant_id, dtt_start) %>%
  group_by(participant_id) %>%
  
  # identify phone sessions, such that the start/end of a session is defined as more than 5 seconds from the previous app session to the next
  mutate(
    gap_seconds      = as.numeric(difftime(dtt_start, lag(dtt_end), units = "secs")),
    phone_session_id = cumsum(replace_na(gap_seconds > 5 , FALSE)) + 1
  ) %>%
  ungroup() 

####################################################
# read in questionnaire data

# Depression, Anxiety and ADHD
youth_selfreport <- arrow::read_parquet("phenotype/mh_y_ysr.parquet") %>%
  select(participant_id, session_id, mh_y_ysr_dtt, mh_y_ysr_age, 
         mh_y_ysr__dsm__anx_tscore, 
         mh_y_ysr__dsm__dep_tscore, 
         mh_y_ysr__dsm__adhd_tscore,
         mh_y_ysr__synd__attn_tscore) %>%
  filter(session_id == abcd_session, 
         participant_id %in% appusage2_logs$participant_id)

####################################################
# read in demographics data to calculate sex

# Depression, Anxiety and ADHD
abcd_static_sex <- arrow::read_parquet("phenotype/ab_g_stc.parquet") %>% 
  select(participant_id, ab_g_stc__cohort_sex)


####################################################
# Filter to only seven days of data 
####################################################

appusage2_sevendays <- appusage2_logs %>%
  group_by(participant_id) %>%
  
  # Remove first and last day (potentially incomplete)
  filter(
    dt > min(dt, na.rm = TRUE),
    dt < max(dt, na.rm = TRUE)
  ) %>%
  
  # Number the remaining days
  arrange(dt, .by_group = TRUE) %>%
  mutate(day_num = dense_rank(dt)) %>%
  
  # Keep only participants with at least 7 remaining days
  filter(n_distinct(dt) >= 7) %>%
  
  # Keep only the first 7 days
  filter(day_num <= 7) %>%
  
  ungroup()


appusage2_sevendays_qnr <- appusage2_sevendays %>%
  # add MH data 
  left_join( select(youth_selfreport, -session_id), by = "participant_id") %>%
  # add sex assigned at birth
  left_join(abcd_static_sex, by = "participant_id")

length(unique(appusage2_logs$participant_id))
length(unique(appusage2_sevendays_qnr$participant_id))


####################################################
# Clean, check and summarise relevant vars
####################################################

appusage2_sevendays_qnr <- select(appusage2_sevendays_qnr, -session_id)

## Count for info

appusage2_logs %>%
  distinct(participant_id, dt) %>%
  count(participant_id, name = "n_dts") %>%
  summarise(
    min    = min(n_dts),
    max    = max(n_dts),
    mean   = mean(n_dts),
    median = median(n_dts),
    sd     = sd(n_dts)
  )

appusage2_sevendays_qnr %>%
  distinct(participant_id, dt) %>%
  count(participant_id, name = "n_dts") %>%
  summarise(
    min    = min(n_dts),
    max    = max(n_dts),
    mean   = mean(n_dts),
    median = median(n_dts),
    sd     = sd(n_dts)
  )

# How long from final EARS day to MH questionnaires, in cleaned final dataset?

appusage2_sevendays_qnr_diffDays <- appusage2_sevendays_qnr %>%
  group_by(participant_id) %>%
  slice_tail(n = 1) %>%
  mutate(days_diff = as.numeric(dt - as.Date(mh_y_ysr_dtt))) %>% 
  ungroup() %>%
  summarise(
    min    = min(days_diff),
    max    = max(days_diff),
    mean   = mean(days_diff),
    median = median(days_diff),
    sd     = sd(days_diff)
  )

# Count sex

appusage2_sevendays_qnr_sex <- appusage2_sevendays_qnr %>%
  group_by(participant_id) %>%
  slice(1) %>%
  select(participant_id, ab_g_stc__cohort_sex)

sum(appusage2_sevendays_qnr_sex$ab_g_stc__cohort_sex== 1) 
sum(appusage2_sevendays_qnr_sex$ab_g_stc__cohort_sex== 2) 


################################################################################
############                    Saved processed dfs            #################
################################################################################

#save_directory <- stringi::stri_join(dirname(here()), "data_processed", "", sep = "/")
save_directory <- "data_processed/"
write.csv(appusage2_sevendays_qnr, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), "abcd_appusage2_sevendays_qnr.csv"))

