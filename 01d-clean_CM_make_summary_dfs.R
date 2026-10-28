################################################################################
################################################################################
############ Creating the different summary data frames    #####################
############ Zelda Brufal                                  #####################
############ JULY 2024                                     #####################
################################################################################
################################################################################


save_directory <- stringi::stri_join(parent_dir, "data_processed", "", sep = "/")

# Check if the directory exists; if not, create it
if (!dir.exists(save_directory)) {
  dir.create(save_directory, recursive = TRUE)
  message("Directory created: ", save_directory)
} else {
  message("Directory already exists: ", save_directory)
}
################################################################################
############ saving event level data frame     #####################
################################################################################

print("saving event- and participant-level data frames")

event_level_df       <- processed_app_data
participant_level_df <- processed_qnr_data
write_csv(event_level_df, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), "event_level_df.csv"))
write_csv(participant_level_df, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), "participant_level_df.csv"))

################################################################################
############ Creating the app-session level data frame     #####################
################################################################################

print("creating and saving app-session level data frame")

appsession_level_df <- processed_app_data %>%
  dplyr::group_by(app_session_id) %>%
  dplyr::summarise(ppid              = first(ppid),
                   day               = first(day),
                   App               = first(App),
                   session_id        = first(session_id),
                   app_session_start = min(Date_and_Time),
                   app_session_end   = max(Date_and_Time),
                   app_session_duration_secs = as.numeric(difftime(app_session_end, app_session_start, units = "secs")),
                   app_session_order_rank    = first(app_session_order_rank)) %>%
  ungroup() %>% 
  filter(!is.na(app_session_id))

write_csv(appsession_level_df, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), "appsession_level_df.csv"))


################################################################################
############ Creating session level data frame     #############################
################################################################################

print("creating and saving session level data frame")

session_level_df <- processed_app_data %>%
  dplyr::group_by(session_id) %>%
  dplyr::summarise(
    ppid                   = first(ppid),
    day                    = first(day),
    session_id             = first(session_id),
    session_start          = min(Date_and_Time),
    session_end            = max(Date_and_Time),
    session_duration_secs  = as.numeric(difftime(session_end, session_start, units = "secs")),
    no_app_sessions        = n_distinct(app_session_id, na.rm = TRUE),
    # note that the first_app, second_app and third_app can in theory all be the same app, they don't have to be different
    first_app              = App[which(app_session_order_rank == 1)[1]],
    second_app             = App[which(app_session_order_rank == 2)[1]],
    third_app              = App[which(app_session_order_rank == 3)[1]],
    max_rank               = max(app_session_order_rank, na.rm = TRUE),  # Get max rank for each session
    penultimate_app        = App[which(app_session_order_rank == (max_rank - 1))[1]],  # Use max rank within session
    last_app               = App[which(app_session_order_rank == max_rank)[1]],  # Use max rank within session
    .groups = 'drop'
  ) %>%
  ungroup()

write_csv(session_level_df, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), "session_level_df.csv"))

 