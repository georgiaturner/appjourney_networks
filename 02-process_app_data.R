################################################################################
############# LOADING IN AND PROCESSING ITACO DATA                ##############
############# August 2024.                                        ##############
############# Zelda Brufal                                        ##############
################################################################################


############# Defining some variables for later ################################

Day_in_seconds  <- 60*60*24
Week_in_seconds <- Day_in_seconds*7
options(digits.secs = 3) # so that milliseconds will be displayed in date and time column

############# Getting to the right folder in the directory ###################


#  Creating a list of all the participant folders and also creating list of ppids

participant_folders <- list.dirs(path = (data_directory), recursive = FALSE, full.names = TRUE)
ppids               <- paste0(list.dirs(path = (data_directory), recursive = FALSE, full.names = FALSE), ".") # Adding a dot so the multiple files from the same folder (ppid) are correctly labelled
ppids_for_participant_level_data_frame <- list.dirs(path = (data_directory), recursive = FALSE, full.names = FALSE)

################################################################################
#############   Making the data frame that all the inidivudal  #################
#############   file data frames will be going into            #################
################################################################################

combined_data_frame <- data.frame()

################################################################################
#############   This is the loop that read in all the files as  ################
#############   data frames for each participant, joins them    ################
#############.  together and then does some basic processing    ################
################################################################################

# Note: the is 'usable participant data' which does not include the seven participants
# for whom the App malfunctioned and did not record the Apps they used: Z17,Z18,Z24,Z26,Z29,Z30,Z31
# Nor does it currently include the Mexico participants.

for (folder in participant_folders) {
  
  print(paste("Processing folder:", folder))
  
  # List all text files in the current folder
  files <- list.files(path = folder, pattern = '*.txt', recursive = TRUE, full.names = TRUE)
  
  for (file_path in files) {
    print(paste("Processing file:", file_path))
    
    # Read the file
    reading_files <- readLines(file_path, encoding = "UTF-8")
    
    ################################################################################
    # correct for weird parsing in Z110, who for some reason never had a semicolon after 
    # TYPE_SCREEN_Mode_on or TYPE_SCREEN_Mode_off so they got wrongly parsed. Just
    # add the semicolons back:
    if (grepl("Z110$", folder)) {
      pattern       <- "(TYPE_SCREEN_Mode_(on|off))(\\d+)"
      reading_files <- gsub(pattern, "\\1;\\3", reading_files)
    }
    ################################################################################
    
    
    # Read the data into a data frame
    data_frame <- read.table(textConnection(reading_files), header = FALSE, sep = ";", fill = TRUE, col.names = paste(1:6))
    
    # Add ppid column
    data_frame$ppid <- basename(folder)
    
    # processing incl. removing non user initiaated events
    data_frame <- data_frame %>%
      dplyr::select(-X1, -X6) %>%
      dplyr::rename(Event         = X2,
                    Date_and_Time = X3,
                    Node_info     = X4,
                    App           = X5) %>%
      # reorder columns
      dplyr::select(ppid, Event, Date_and_Time, Node_info, App) %>%         
      dplyr::ungroup() %>%
      dplyr::filter(!grepl("TYPE_WINDOWS_CHANGED|TYPE_WINDOW_STATE_CHANGED", Event)) %>% # remove the non user initiated events. These are events that the App recorded as screen changes etc, but which were not caused by a user action
      dplyr::mutate(across(everything(), as.character)) %>%
      dplyr::mutate(Date_and_Time = lubridate::ymd_hms(Date_and_Time, tz = "GMT", quiet = FALSE)) # format the date and time into POSIXct

    # Verify date_and_time column
    if (!all(sapply(data_frame$Date_and_Time, inherits, what = "POSIXct"))) {
      stop("STOP: Not all elements in the Date_and_Time column are of class POSIXct.")
    }
    
    # add to the combined data frames
    combined_data_frame <- dplyr::bind_rows(combined_data_frame, data_frame)
    
    # Remove the individual data frame to free up memory on computer (makes a lot quicker)
    rm(data_frame)
  }
}


#############################################################################

# Inspect for irregularities
combined_data_frame[which(is.na(combined_data_frame$ppid)),]
combined_data_frame[which(is.na(combined_data_frame$Event)),]
combined_data_frame[which(is.na(combined_data_frame$Date_and_Time)),] # there should be 10 rows, each of which column names were wrongly parsed
combined_data_frame[which(is.na(combined_data_frame$Node_info)),]
combined_data_frame[which(is.na(combined_data_frame$App)),] # should be only pauses and plays

# Deal with irregularities - this part is specific to this dataset, after having inspected above,
# and will therefore need to be updated if any more data is added

# store copy of combined data frame before cleaning
combined_data_frame_clean <- combined_data_frame

#############################################################################

# There are NAs in Date_and_Time and in App. 
#
# The NAs in App are when the participant paused the iTACO app data collection, 
# which for ethical reasons they could do.
# These rows are therefore not problematic at the moment
# Note that the names of the files include date as MM-DD-YYYY, 
# but the column in Date_and_Time includes it in YYYY-MM-DD even in the raw data.
#
# The NAs in Date_and_Time are when the dataset wrongly included new title lines - these need to be removed. 
# In the current dataset, these are in the raw files:
# -- Ppid M2   -> 'Log-m2-26-04-2024’
# -- Ppid Z103 -> 'Log-z103-25-07-2024’
# -- Ppid Z52  -> 'Log-z52-10-07-2024'
# -- Ppid Z72  -> 'Log-z72-15-07-2024'
# -- Ppid Z86  -> 'Log-z86-27-07-2024', 'Log-z86-28-07-2024', 'Log-z86-30-07-2024'
# -- Ppid Z110 -> 'Log-z110-26-07-2024'

combined_data_frame_clean <- combined_data_frame_clean %>%
  filter(!(ppid %in% c("m2", "Z103", "Z52", "Z72", "Z86", "Z110") & is.na(Date_and_Time)))

# Check irregularities are now removed
combined_data_frame_clean[which(is.na(combined_data_frame_clean$ppid))]
combined_data_frame_clean[which(is.na(combined_data_frame_clean$Event)),]
combined_data_frame_clean[which(is.na(combined_data_frame_clean$Date_and_Time)),]
combined_data_frame_clean[which(is.na(combined_data_frame_clean$Node_info)),]
combined_data_frame_clean[which(is.na(combined_data_frame_clean$App)),]

print("Finished loading files, now moving on to processing (can take a while)")

################################################################################
##### Removing the space before some of the apps ###############################
################################################################################

combined_data_frame_clean$App <- str_trim(combined_data_frame_clean$App)

# get rid of NAs because below when assigning app_session_id, the cumsum function only works if the one above is not NA
combined_data_frame_clean[which(is.na(combined_data_frame_clean$App)) ,"App"] <- ""

# For some participants, sometimes there are Apps meeting where NodeInfo is null and App is "".
# Check who these participants are:
unique(filter(combined_data_frame_clean, Node_info == "null" & App == "")$ppid)
unique(filter(combined_data_frame_clean, Node_info == "null" & App == "")$Event)
# These are non-user initiated 'system' events, so remove:
combined_data_frame_clean <- combined_data_frame_clean %>%
  filter(!(Node_info == "null" & App == ""))


################################################################################
######      Arranging by date and time and adding 'day' column.         ########
################################################################################

combined_data_frame_clean <- combined_data_frame_clean %>%
  dplyr::group_by(ppid) %>%
  dplyr::arrange(Date_and_Time, .by_group = TRUE) %>% # necessary because some files are not in order, e.g. participant M1 has 01/05/2024 before the rest of their files.
  dplyr::mutate(day = as.numeric(as.Date(Date_and_Time) - min(as.Date(Date_and_Time))) + 1) %>%
  ungroup()

################################################################################
###### CHECKING TO SEE IF PARTICIPANTS HAVE DAY FILES MISSING ##################
################################################################################

ppid_with_gaps <- combined_data_frame_clean %>%
  distinct(ppid, day) %>%               # Ensure unique combinations of ppid and day
  arrange(ppid, day) %>%                # Sort by ppid and day
  group_by(ppid) %>%                    # Group by ppid
  mutate(day_diff = day - lag(day)) %>% # Calculate difference between consecutive days
  summarise(has_gap = any(day_diff > 1, na.rm = TRUE)) %>% # Check if any difference > 1
  filter(has_gap) %>%                   # Filter ppids with gaps
  pull(ppid)                            # Extract only the ppids

print(ppid_with_gaps)
# In the current dataset the ppids with gaps are m2 and Z86. Both these have over 7 days of data without gaps
# anyway, not including the missing days.


################################################################################
#####################     Assigning session IDs     ############################
################################################################################

# This now means that if a session runs over the start of a new day, it is still counted as the same session. This will 
# affect things like the individual daily screen time if counted based on number of sessions,
# because it will be counting screen time from a different day. 
# It was decided to do this because only a tiny number of sessions spanned over a day change. Some of these were indications that 
# there was missing data 
# Add this code : | day_no != lag(day_no, default = first(day_no)) to the function below to change the definition of a session
# so that if a session runs over a day it splits it in two (to retain accurate daily screen time)

processed_app_data <- combined_data_frame_clean %>%
  dplyr::mutate(session_id     = cumsum(Event == "TYPE_SCREEN_Mode_on" | ppid != lag(ppid, default = first(ppid))) + 1) %>%
  dplyr::mutate(app_session_id = if_else(Event %in% c("TYPE_CAPTURED_PAUSED", "TYPE_CAPTURED_PLAYED", "TYPE_SCREEN_Mode_off", "TYPE_SCREEN_Mode_on"),
                                         NA,
                                         cumsum( (ppid != lag(ppid, default = "") | App != lag(App, default = "")) & App != "") )) %>%
  group_by(ppid, session_id) %>%
  dplyr::mutate(app_session_order_rank = if_else(
    Event %in% c("TYPE_CAPTURED_PAUSED", "TYPE_CAPTURED_PLAYED", "TYPE_SCREEN_Mode_off", "TYPE_SCREEN_Mode_on"),
    NA,
    cumsum((ppid != lag(ppid) | App != lag(App, default = "")) & App != ""  ))) %>%
  ungroup() %>% 
  filter(!(Event =="TYPE_VIEW_SELECTED")) # because this is a system event not a user-generated event


################################################################################
## cleaning


#### make all ppid column upper case
processed_app_data$ppid <- toupper(processed_app_data$ppid)

