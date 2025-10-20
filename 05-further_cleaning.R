
################################################################################
############# CLEANING AND EXCLUSIONS FOR ANALYSIS                ##############
############# Jan 2025.                                           ##############
############# Georgia Turner, georgianjt@gmail.com                ##############
################################################################################

# This script applies further cleaning to the pre-processed dataframes, such as excluding certain data.

rm(list = ls())
seed <- 1
set.seed(seed)

################## Setting up environment  #####################################
library(tidyverse)
library(here)
library(ggplot2)

here_dir   <- here()
parent_dir <- dirname(here_dir)
setwd(parent_dir)

################################################################################
############                load in data            ############################
################################################################################

filepath                   <- str_c(parent_dir, "/data_processed/")
event_level_filename       <- "250908_1519_event_level_df.csv"
app_session_level_filename <- "250908_1523_appsession_level_df.csv"
session_level_filename     <- "250908_1523_session_level_df.csv"
participant_level_filename <- "250908_1520_participant_level_df.csv"

event_level_df             <- read_csv(str_c(filepath, event_level_filename)) 
app_session_level_df       <- read_csv(str_c(filepath, app_session_level_filename))
session_level_df           <- read_csv(str_c(filepath, session_level_filename))
participant_level_df       <- read_csv(str_c(filepath, participant_level_filename))

################################################################################
##########  Inspect distributions and remove outliers            ###############
################################################################################

# decided not to remove outliers as they are difficult to interpret / come up with a 
# standardised rule for, and as these inspections show they represent a v small proportion
# of the data.

# check for weird app session length
ggplot2::ggplot(app_session_level_df, aes(x = app_session_duration_secs)) +
  geom_histogram(fill = "blue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of App Session Duration (Seconds)",
    x = "App Session Duration (Seconds)",
    y = "Frequency"
  ) +
  theme_classic()

n_app_session_over_1hr <- length(which(app_session_level_df$app_session_duration_secs > 3600))
print(str_c(n_app_session_over_1hr, " app sessions over 1 hour out of ", nrow(app_session_level_df), ", i.e. ", n_app_session_over_1hr/ nrow(app_session_level_df), " %."))


# check for weird session length
ggplot(session_level_df, aes(x = session_duration_secs)) +
  geom_histogram(fill = "blue", color = "white", alpha = 0.7) +
  labs(
    title = "Distribution of Session Duration (Seconds)",
    x = "Session Duration (Seconds)",
    y = "Frequency"
  ) +
  theme_minimal()

n_session_over_1hr <- length(which(session_level_df$session_duration_secs > 3600))
print(str_c(n_session_over_1hr, " sessions over 1 hour out of ", nrow(session_level_df), ", i.e. ", n_session_over_1hr/ nrow(session_level_df), " %."))

################################################################################
############  Inspect sessions containing a pause.                ##############
################################################################################

# count how many sessions contain a pause or play
pause_or_play_indices <- (which(event_level_df$Event == "TYPE_CAPTURED_PAUSED" | event_level_df$Event == "TYPE_CAPTURED_PLAYED"))
# list of participants with pauses:
ppids_with_pause <- unique(event_level_df[pause_or_play_indices,]$ppid)
length(ppids_with_pause)
# list of sessions with pauses:
sessions_with_pause <- unique(event_level_df[pause_or_play_indices,]$session_id)
length(sessions_with_pause)
# days with pauses:
days_with_pause <- event_level_df %>%
  filter(Event %in% c("TYPE_CAPTURED_PAUSED", "TYPE_CAPTURED_PLAYED")) %>%
  distinct(ppid, day)   # keep unique (ppid, day) combos
nrow(days_with_pause)                    # count them
days_all <- event_level_df %>%
  distinct(ppid, day)   # keep unique (ppid, day) combos
nrow(days_all)                    # count them

# There are 34 participants, or 240 sessions with pauses. 
# as 34 participants is a lot to remove, for the purposes of the current 
# analysis plan we will just remove all sessions with pauses from the app data (event, app session and session dfs).

event_level_df_processed       <- filter(event_level_df, !(session_id %in% sessions_with_pause))
app_session_level_df_processed <- filter(app_session_level_df, !(session_id %in% sessions_with_pause))
session_level_df_processed     <- filter(session_level_df, !(session_id %in% sessions_with_pause))


################################################################################
############  Identify participants who didn't pass all attention checks  ######
################################################################################

participant_level_df_processed <- filter(participant_level_df, Finished=="TRUE")

# inspect ppids that didn't pass various checks
participants_fail_qual_precommitment  <- filter(participant_level_df_processed, quality_precommitment != "Yes, I will")
print(participants_fail_qual_precommitment$ppid)
participants_fail_qual_postcommitment <- filter(participant_level_df_processed, quality_postcommitment != "Yes")
print(participants_fail_qual_postcommitment$ppid)
participants_fail_attcheck1 <- filter(participant_level_df_processed, !grepl("false", quality_attention_check_1))
print(participants_fail_attcheck1$ppid)
participants_fail_attcheck2 <- filter(participant_level_df_processed, !grepl("false", quality_attention_check_2))
print(participants_fail_attcheck2$ppid)
participants_fail_attcheck3 <- filter(participant_level_df_processed, !grepl("Moderately", quality_attention_check_3))
print(participants_fail_attcheck3$ppid)

# Don't exclude any for the moment but come back to these and try sensitivity analyses without them at the end.
# M7 is the only ppid who failed 2/3 attention checks.

################################################################################
#### recode most and least automatic apps to match the App in the data  ########
################################################################################


####### check for esoteric stuff 

# 1/ check for multiple whatsapp names
which(
  unique(
    filter(
      event_level_df_processed,
      ppid %in% filter(
        participant_level_df_processed,
        str_detect(tolower(most_automatic_app_self_reported), "whatsapp") |
          str_detect(tolower(least_automatic_app_self_reported), "whatsapp")
      )$ppid
    )$App
  ) == "com.whatsapp"
) 
# this came up with one index meaning that one of the participants who mentioned whatsapp included com.whatsapp in their used apps.  
which(unique(filter(event_level_df_processed, 
                    ppid %in% (filter(participant_level_df_processed, tolower(most_automatic_app_self_reported)=="whatsapp"))$ppid | 
                      ppid %in% (filter(participant_level_df_processed, tolower(least_automatic_app_self_reported)=="whatsapp"))$ppid)$App)=="com.whatsapp.w4b")
# this came up with 0 indices meaning that none of the participants who mentioned whatsapp included com.whatsapp.w4b in their used apps.  
# we see from this that no participants whose most or least automatic app was whatsapp used the whatsapp business app (whatsapp.w4b) so we can safely replace them all with com.whatsapp

# 2/ for participant M7 who mentioned calculator, check which calculator they had
grep("calculator", unique(filter(event_level_df, ppid == "M7")$App), ignore.case = TRUE)
# turns out they didnt use an app with calculator during the data collection period (which makes sense as least automatic)
# so just store this info as 'not_used'

# for participants who mentioned calendar...
grep("calendar", unique(filter(event_level_df, ppid == "Z21")$App), ignore.case = TRUE)
grep("calendar", unique(filter(event_level_df, ppid == "M8")$App), ignore.case = TRUE) # for m8 it is android, for Z21 there are two so hard to know...


####### Add to df

# initiate new column
participant_level_df_processed$most_automatic_app_recoded  <- ""
participant_level_df_processed$least_automatic_app_recoded <- ""

# Update the most_automatic_app_recoded column based on the condition
participant_level_df_processed <- participant_level_df_processed %>%
  mutate(
    most_automatic_app_recoded = case_when(
      tolower(most_automatic_app_self_reported) == "instagram"                      ~ "com.instagram.android",
      tolower(most_automatic_app_self_reported) == "instagram, for messaging"       ~ "com.instagram.android",
      tolower(most_automatic_app_self_reported) == "tiktok"                         ~ "com.zhiliaoapp.musically",
      tolower(most_automatic_app_self_reported) == "facebook"                       ~ "com.facebook.katana", # here we assume they refer to the FB app, if they referred to the messenger app it would be com.facebook.orca
      tolower(most_automatic_app_self_reported) == "maybe facebook"                 ~ "com.facebook.katana", # here we assume they refer to the FB app, if they referred to the messenger app it would be com.facebook.orca
      tolower(most_automatic_app_self_reported) == "x"                              ~ "com.twitter.android", 
      tolower(most_automatic_app_self_reported) == "twitter"                        ~ "com.twitter.android", 
      tolower(most_automatic_app_self_reported) == "twitter/x"                      ~ "com.twitter.android", 
      tolower(most_automatic_app_self_reported) == "x app i think"                  ~ "com.twitter.android", 
      tolower(most_automatic_app_self_reported) == "x (formerly twitter)"           ~ "com.twitter.android", 
      tolower(most_automatic_app_self_reported) == "whatsapp"                       ~ "com.whatsapp", 
      tolower(most_automatic_app_self_reported) == "most automatic app is whatsapp" ~ "com.whatsapp", 
      tolower(most_automatic_app_self_reported) == "whattsap, just to check if i have any messages without thinking about it" ~ "com.whatsapp" , # 
      tolower(most_automatic_app_self_reported) == "youtube"                        ~ "com.google.android.youtube", 
      tolower(most_automatic_app_self_reported) == "probably youtube."              ~ "com.google.android.youtube", 
      tolower(most_automatic_app_self_reported) == "youtube shorts"                 ~ "com.google.android.youtube", 
      tolower(most_automatic_app_self_reported) == "spotify"                        ~ "com.spotify.music", 
      tolower(most_automatic_app_self_reported) == "chrome"                         ~ "com.android.chrome", 
      tolower(most_automatic_app_self_reported) == "chrome - i access instagram through this." ~ "com.android.chrome", 
      tolower(most_automatic_app_self_reported) == "gmail"                          ~ "com.google.android.gm", 
      tolower(most_automatic_app_self_reported) == "outlook"                        ~ "com.microsoft.office.outlook", 
      tolower(most_automatic_app_self_reported) == "netflix"                        ~ "com.netflix.mediaclient",
      tolower(most_automatic_app_self_reported) == "strava"                         ~ "com.strava",
      tolower(most_automatic_app_self_reported) == "duolingo"                       ~ "com.duolingo",
      tolower(most_automatic_app_self_reported) == "snapchat"                       ~ "com.snapchat.android",
      tolower(most_automatic_app_self_reported) == "clue"                           ~ "com.clue.android",
      tolower(most_automatic_app_self_reported) == "clue, period tracking app"      ~ "com.clue.android",
      tolower(most_automatic_app_self_reported) == "google maps"                    ~ "com.google.android.apps.maps",
      tolower(most_automatic_app_self_reported) == "google maps maybe?"             ~ "com.google.android.apps.maps",
      tolower(most_automatic_app_self_reported) == "least automatic app is calculator" ~ "not_used",
      tolower(most_automatic_app_self_reported) == "pinterest"                      ~ "com.pinterest",
      tolower(most_automatic_app_self_reported) == "notes"                          ~ "com.samsung.android.app.notes",
      tolower(most_automatic_app_self_reported) == "timetree"                       ~ "work.jubilee.timetree",
      tolower(most_automatic_app_self_reported) == "natwest"                        ~ "com.rbs.mobile.android.natwest",
      tolower(most_automatic_app_self_reported) == "my pedometer app (running in the background)" ~ "com.tayu.tau.pedometer", # note that they say running in the background so it may deviate from the foreground apps we meant - something to look at if data is irregular
      ppid                                      == "Z87"                            ~ "com.instagram.android" , # note that they said instagram or instander. they have both so just went with instagram 
      ppid                                      == "M6"                             ~ "no_unique_app" , # there are two googles, com.google.android.googlequicksearchbox and com.android.chrome
      ppid                                      == "Z133"                           ~ "no_unique_app" , # there are two googles, com.google.android.googlequicksearchbox and com.android.chrome
      ppid                                      == "M10"                            ~ "not_used" , # they said dexscreener but there is no such app in their used apps
      ppid                                      == "Z53"                            ~ "not_used" , # they said google wallet but there is no such app in their used apps
      ppid                                      == "Z137"                           ~ "com.whatsapp" , # they said whatsapp and instagram so just did one
      ppid                                      == "Z123"                           ~ "com.google.android.apps.authenticator2" , # they said whatsapp and instagram so just did one
      ppid                                      == "Z132"                           ~ "com.zhiliaoapp.musically" , # they said tiktok or insta so just did one
      ppid                                      == "Z128"                           ~ "com.facebook.katana" , # they said instagram but lately fb/youtube, so just went with fb
      TRUE ~ ""  # Retain the existing value if no match
    ),
    least_automatic_app_recoded = case_when(
      tolower(least_automatic_app_self_reported) == "instagram"     ~ "com.instagram.android",
      tolower(least_automatic_app_self_reported) == "instagram, for messaging"     ~ "com.instagram.android",
      tolower(least_automatic_app_self_reported) == "tiktok"        ~ "com.zhiliaoapp.musically",
      tolower(least_automatic_app_self_reported) == "facebook"      ~ "com.facebook.katana", # here we assume they refer to the FB app, if they referred to the messenger app it would be com.facebook.orca
      tolower(least_automatic_app_self_reported) == "maybe facebook"~ "com.facebook.katana", # here we assume they refer to the FB app, if they referred to the messenger app it would be com.facebook.orca
      tolower(least_automatic_app_self_reported) == "x"             ~ "com.twitter.android", 
      tolower(least_automatic_app_self_reported) == "twitter"       ~ "com.twitter.android", 
      tolower(least_automatic_app_self_reported) == "twitter/x"     ~ "com.twitter.android", 
      tolower(least_automatic_app_self_reported) == "x app i think" ~ "com.twitter.android", 
      tolower(least_automatic_app_self_reported) == "x (formerly twitter)"           ~ "com.twitter.android", 
      tolower(least_automatic_app_self_reported) == "whatsapp"                       ~ "com.whatsapp", 
      tolower(least_automatic_app_self_reported) == "most automatic app is whatsapp" ~ "com.whatsapp", 
      tolower(least_automatic_app_self_reported) == "youtube"                        ~ "com.google.android.youtube", 
      tolower(least_automatic_app_self_reported) == "probably youtube."              ~ "com.google.android.youtube", 
      tolower(least_automatic_app_self_reported) == "youtube shorts"                 ~ "com.google.android.youtube", 
      tolower(least_automatic_app_self_reported) == "spotify"                        ~ "com.spotify.music", 
      tolower(least_automatic_app_self_reported) == "chrome"                         ~ "com.android.chrome", 
      tolower(least_automatic_app_self_reported) == "gmail"                          ~ "com.google.android.gm", 
      tolower(least_automatic_app_self_reported) == "outlook"                        ~ "com.microsoft.office.outlook",
      tolower(least_automatic_app_self_reported) == "netflix"                        ~ "com.netflix.mediaclient",
      tolower(least_automatic_app_self_reported) == "strava"                         ~ "com.strava",
      tolower(least_automatic_app_self_reported) == "duolingo"                       ~ "com.duolingo",
      tolower(least_automatic_app_self_reported) == "snapchat"                       ~ "com.snapchat.android",
      tolower(least_automatic_app_self_reported) == "clue"                           ~ "com.clue.android",
      tolower(least_automatic_app_self_reported) == "clue, period tracking app"      ~ "com.clue.android",
      tolower(least_automatic_app_self_reported) == "google maps"                    ~ "com.google.android.apps.maps",
      tolower(least_automatic_app_self_reported) == "google maps maybe?"             ~ "com.google.android.apps.maps",
      tolower(least_automatic_app_self_reported) == "pinterest"                      ~ "com.pinterest",
      tolower(least_automatic_app_self_reported) == "metronome beats"                ~ "com.andymstone.metronome",
      tolower(least_automatic_app_self_reported) == "notes"                          ~ "com.samsung.android.app.notes",
      tolower(least_automatic_app_self_reported) == "timetree"                       ~ "work.jubilee.timetree",
      tolower(least_automatic_app_self_reported) == "natwest"                        ~ "com.rbs.mobile.android.natwest",
      tolower(least_automatic_app_self_reported) == "puregym, which i only use when i need gym access" ~ "com.puregym",
      tolower(least_automatic_app_self_reported) == "banking apps (eg hsbc, monzo)"  ~ "uk.co.hsbc.hsbcukmobilebanking", # they said hsbc and monzo so just put hsbc
      ppid                                       == "Z63"                            ~ "not_used",
      ppid                                       == "M7"                             ~ "not_used",
      ppid                                       == "Z56"                            ~ "not_used",
      ppid                                       == "M8"                             ~ "com.google.android.calendar" ,
      ppid                                       == "Z16"                            ~ "com.mobispector.bustimes" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "Z15"                            ~ "com.microsoft.office.outlook" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "Z25"                            ~ "co.uk.getmondo" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "Z28"                            ~ "com.google.android.keep" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "M3"                             ~ "com.tesco.inform.prod" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "M2"                             ~ "com.android.deskclock" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "M6"                             ~ "jp.pokemon.pokemonsleep" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "M5"                             ~ "com.breathing.zone" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "M9"                             ~ "com.sh.smart.caller" , # worked this out by manually looking at all the unique apps that participant used
      ppid                                       == "Z21"                            ~ "no_unique_app" , # there are two calendars, com.samsung.android.calendar and com.google.android.calendar, so we dont know which it is
      ppid                                       == "Z135"                           ~ "no_unique_app" , # there are two clocks, com.sec.android.app.clockpackage and com.samsung.android.app.clockpack, so we dont know which it is
      ppid                                       == "Z69"                            ~ "com.microsoft.office.outlook" ,
      ppid                                       == "Z53"                            ~ "com.android.settings" , 
      ppid                                       == "Z65"                            ~ "not_used" , 
      ppid                                       == "Z64"                            ~ "com.sec.android.app.camera" , 
      ppid                                       == "Z52"                            ~ "com.android.chrome" , 
      ppid                                       == "Z87"                            ~ "com.ecosia.android" , 
      ppid                                       == "Z92"                            ~ "not_used" , # they said games but there is no such app in their used apps
      ppid                                       == "Z104"                           ~ "com.google.android.deskclock" , 
      ppid                                       == "Z79"                            ~ "com.microsoft.office.outlook" , 
      ppid                                       == "Z93"                            ~ "com.google.android.apps.maps" , 
      ppid                                       == "Z84"                            ~ "com.bestfuncoolapps.TakeYourPills" , 
      ppid                                       == "Z85"                            ~ "co.uk.Nationwide.Mobile" , # they said nationwide or costa so we just went with nationwide
      ppid                                       == "Z109"                           ~ "not_used" , # they said bitwarden but not in list of apps
      ppid                                       == "Z107"                           ~ "com.azure.authenticator" , # they said banking or authenticator so just went with authenticator
      ppid                                       == "Z131"                           ~ "app.snoop" , 
      ppid                                       == "Z115"                           ~ "no_unique_app" , # there are two googles
      ppid                                       == "Z117"                           ~ "com.google.android.apps.maps" , 
      ppid                                       == "Z136"                           ~ "com.google.android.deskclock" , 
      ppid                                       == "Z134"                           ~ "com.wetherspoon.orderandpay" , 
      ppid                                       == "Z137"                           ~ "com.netflix.mediaclient" , 
      ppid                                       == "Z143"                           ~ "not_used" , 
      ppid                                       == "Z138"                           ~ "com.YostarJP.BlueArchive" , 
      ppid                                       == "Z123"                           ~ "com.android.chrome" , 
      ppid                                       == "Z121"                           ~ "com.grppl.android.shell.BOS" , 
      ppid                                       == "Z128"                           ~ "com.android.vending" , 
      ppid                                       == "Z132"                           ~ "com.sec.android.daemonapp" , 
      TRUE ~ ""  # Retain the existing value if no match
    )
  )

# useful commands
#View(select(participant_level_df_processed, ppid, most_automatic_app_self_reported, least_automatic_app_self_reported, most_automatic_app_recoded, least_automatic_app_recoded))
#which(grepl("pinterest", unique(event_level_df$App), ignore.case = TRUE))

################################################################################
############ add in vars to participant_level_df    ############################
################################################################################

################################################################################
# set up variables to make vars dfs

ppids <- unique(participant_level_df_processed$ppid)

home_apps <- c(
  "com.android.systemui", 
  "com.miui.home", 
  "com.google.android.apps.nexuslauncher",
  "com.motorola.launcher3",
  "android",
  "bitpit.launcher",
  "com.hihonor.android.launcher",
  "com.transsion.hilauncher",
  "com.sec.android.app.launcher",
  "com.teslacoilsw.launcher",
  "com.android.launcher",
  "com.github.kolacbb.launcher",
  "com.qqlabs.minimalistlauncher",
  "com.huawei.android.launcher",
  "com.android.launcher3",
  "com.oppo.launcher"
)

mode_first_app <- session_level_df_processed %>%
  group_by(ppid, first_app) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!(first_app %in% home_apps) & !is.na(first_app)) %>%
  group_by(ppid) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE)

mode_second_app <- session_level_df_processed %>% 
  group_by(ppid, second_app) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!(second_app %in% home_apps) & !is.na(second_app)) %>%
  group_by(ppid) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE)

mode_third_app <- session_level_df_processed %>% 
  group_by(ppid, third_app) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!(third_app %in% home_apps) & !is.na(third_app)) %>%
  group_by(ppid) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE)

mode_last_app <- session_level_df_processed %>% 
  group_by(ppid, last_app) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!(last_app %in% home_apps) & !is.na(last_app)) %>%
  group_by(ppid) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE)

mode_penultimate_app <- session_level_df_processed %>% 
  group_by(ppid, penultimate_app) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(!(penultimate_app %in% home_apps) & !is.na(penultimate_app)) %>%
  group_by(ppid) %>%
  slice_max(order_by = count, n = 1, with_ties = FALSE)

# for ppid==Z99, the first app was always the system user interface. As this is one of the excluded
# home screens, Z99 does not appear in mode_first_app. Therefore for this one we replace their
# first and second with their second and third (to be consistent with all participants that it is the 
# apps used not including system user interface/home)
Z99_first_app <- mode_second_app %>%
  filter(ppid == "Z99") %>%
  rename(first_app = second_app)
mode_first_app <- mode_first_app %>%
  filter(ppid!="Z99") %>%
  bind_rows(Z99_first_app)
Z99_second_app <- mode_third_app %>%
  filter(ppid == "Z99") %>%
  rename(second_app = third_app)
mode_second_app <- mode_second_app %>%
  filter(ppid!="Z99") %>%
  bind_rows(Z99_second_app)


################################################################################
# make participant level vars dfs to be added to merged participant level df processed

# get info for each participants
proportion_appconsistency_per_ppid_vars <- 
  session_level_df_processed %>% 
  left_join(mode_first_app, by = "ppid", suffix = c("", "_mode")) %>% 
  left_join(mode_second_app, by = "ppid", suffix = c("", "_mode")) %>% 
  left_join(mode_penultimate_app, by = "ppid", suffix = c("", "_mode")) %>% 
  left_join(mode_last_app, by = "ppid", suffix = c("", "_mode")) %>% 
  group_by(ppid) %>%
  summarise(proportionFirstMode = mean(first_app == first_app_mode, na.rm = TRUE),
            proportionSecondMode = mean(second_app == second_app_mode, na.rm = TRUE),
            proportionPenultimateMode = mean(penultimate_app == penultimate_app_mode, na.rm = TRUE),
            proportionLastMode = mean(last_app == last_app_mode, na.rm = TRUE))


session_vars <- session_level_df_processed %>% # note that we have already removed all sessions containing a pause above
  group_by(ppid) %>%               # Group by ppid
  summarise(mean_session_duration_secs     = mean(session_duration_secs, na.rm = TRUE),
            median_session_duration_secs   = median(session_duration_secs, na.rm = TRUE),
            sd_session_duration_secs       = sd(session_duration_secs, na.rm = TRUE)
  )

app_session_per_ppid_vars <- app_session_level_df_processed %>% # note that we have already removed all app sessions in sessions containing a pause above
  group_by(ppid) %>%               # Group by ppid
  summarise(mean_app_session_duration_secs    = mean(app_session_duration_secs, na.rm = TRUE),
            median_app_session_duration_secs  = median(app_session_duration_secs, na.rm = TRUE),
            sd_app_session_duration_secs      = sd(app_session_duration_secs, na.rm = TRUE)
  ) 

app_session_per_session_vars <- app_session_level_df_processed %>% # note that this df only includes sessions with >0 apps, ie that arent just screen on and screen off, so the mean numbers of apps per session etc will be accordingly affected
  group_by(ppid, session_id) %>%
  dplyr::summarize(
    distinct_apps                    = n_distinct(App),
    nondistinct_apps                 = n_distinct(app_session_id),
    distinct_total_app_session_ratio = distinct_apps / nondistinct_apps, # quantifies tendency to return to same app
    .groups = "drop"
  ) %>%
  group_by(ppid) %>%
  dplyr::summarize(
    mean_distinct_apps_per_session        = mean(distinct_apps, na.rm = TRUE),
    median_distinct_apps_per_session      = median(distinct_apps, na.rm = TRUE),
    sd_distinct_apps_per_session          = sd(distinct_apps, na.rm = TRUE),
    mean_apps_per_session                 = mean(nondistinct_apps, na.rm = TRUE),
    median_apps_per_session               = median(nondistinct_apps, na.rm = TRUE),
    sd_apps_per_session                   = sd(nondistinct_apps, na.rm = TRUE),
    mean_distinct_total_app_session_ratio   = mean(distinct_total_app_session_ratio, na.rm = TRUE),
    median_distinct_total_app_session_ratio = median(distinct_total_app_session_ratio, na.rm = TRUE),
    sd_distinct_total_app_session_ratio     = mean(distinct_total_app_session_ratio, na.rm = TRUE),
    .groups = "drop"
  )

touch_events <- c("TYPE_VIEW_CLICKED", "TYPE_VIEW_LONG_CLICKED",  "TYPE_VIEW_SCROLLED",  "CONTENT_CHANGE_TYPE_DRAG_STARTED", "CONTENT_CHANGE_TYPE_DRAG_DROPPED")

# Summarize per session_id and ppid
event_per_session_vars <- event_level_df_processed %>%
  group_by(session_id, ppid) %>%
  dplyr::summarize(
    touchevent_count = sum(Event %in% touch_events),  # Count only touch events
    scrolled_count   = sum(Event == "TYPE_VIEW_SCROLLED"),
    clicked_count    = sum(Event == "TYPE_VIEW_CLICKED"),
    session_duration = as.numeric(difftime(max(Date_and_Time), min(Date_and_Time), units = "secs")),
    session_start    = min(Date_and_Time),
    session_end      = max(Date_and_Time),
    .groups = "drop"
  ) %>%
  mutate(
    touchevents_per_sec  = touchevent_count / session_duration,
    scrolls_per_sec      = scrolled_count / session_duration,
    clicks_per_sec       = clicked_count / session_duration
  ) %>%
  group_by(ppid) %>%
  dplyr::summarize(
    mean_event_per_session      = mean(touchevent_count),
    median_event_per_session    = median(touchevent_count),
    sd_event_per_session        = sd(touchevent_count),
    mean_scrolled_per_session   = mean(scrolled_count),
    median_scrolled_per_session = median(scrolled_count),
    sd_scrolled_per_session     = sd(scrolled_count),
    mean_clicked_per_session    = mean(clicked_count),
    median_clicked_per_session  = median(clicked_count),
    sd_clicked_per_session      = sd(clicked_count),
    mean_touchevents_per_sec    = mean(touchevents_per_sec, na.rm = TRUE),
    median_touchevents_per_sec  = median(touchevents_per_sec, na.rm = TRUE),
    sd_touchevents_per_sec      = sd(touchevents_per_sec, na.rm = TRUE),
    mean_scrolls_per_sec        = mean(scrolls_per_sec, na.rm = TRUE),
    median_scrolls_per_sec      = median(scrolls_per_sec, na.rm = TRUE),
    sd_scrolls_per_sec          = sd(scrolls_per_sec, na.rm = TRUE),
    mean_clicks_per_sec         = mean(clicks_per_sec, na.rm = TRUE),
    median_clicks_per_sec       = median(clicks_per_sec, na.rm = TRUE),
    sd_clicks_per_sec           = sd(clicks_per_sec, na.rm = TRUE),
    .groups = "drop"
  )

iti_df <- event_level_df_processed %>% filter(Event %in% touch_events)
iti_df$iti <- NA  
# Loop through each ppid
for (pp in ppids) {
  # Compute ITI for current ppid
  temp_df <- iti_df %>%
    filter(ppid == pp) %>%
    group_by(session_id) %>%
    mutate(iti = Date_and_Time - lag(Date_and_Time))  # Compute ITI
  print(pp)
  # Assign computed ITI values back to event_level_df
  iti_df$iti[iti_df$ppid == pp] <- temp_df$iti
}
iti_per_ppid <- iti_df %>% 
  group_by(ppid) %>%
  mutate(mean_iti   = mean(iti, na.rm = TRUE),
         median_iti = median(iti, na.rm = TRUE),
         sd_iti     = sd(iti, na.rm = TRUE)) %>% 
  slice(1) %>%
  select(ppid, mean_iti, median_iti, sd_iti)

isi_df <- session_level_df_processed
isi_df$isi <- NA  
# Loop through each ppid
for (pp in ppids) {
  # Compute ITI for current ppid
  temp_df <- isi_df %>%
    filter(ppid == pp) %>%
    group_by(ppid) %>%
    mutate(
      isi = session_start - lag(session_end))  # Compute ITI
  print(pp)
  # Assign computed ITI values back to event_level_df
  isi_df$isi[isi_df$ppid == pp] <- temp_df$isi
}
isi_per_ppid <- isi_df %>% 
  group_by(ppid) %>%
  mutate(mean_isi   = mean(isi, na.rm = TRUE),
         median_isi = median(isi, na.rm = TRUE),
         sd_isi     = sd(isi, na.rm = TRUE)) %>% 
  slice(1) %>% 
  select(ppid, mean_isi, median_isi, sd_isi)



# Summarize per app_session_id and ppid
event_per_app_session_vars <- event_level_df_processed %>%
  filter(Event %in% touch_events) %>%
  group_by(app_session_id, ppid) %>%
  dplyr::summarize(
    event_count    = n(),
    scrolled_count = sum(Event == "TYPE_VIEW_SCROLLED"),
    clicked_count  = sum(Event == "TYPE_VIEW_CLICKED"),
    .groups = "drop"
  ) %>%
  group_by(ppid) %>%
  dplyr::summarize(
    mean_event_per_app_session      = mean(event_count),
    mean_scrolled_per_app_session.  = mean(scrolled_count),
    mean_clicked_per_app_session    = mean(clicked_count),
    median_event_per_app_session    = median(event_count),
    median_scrolled_per_app_session = median(scrolled_count),
    median_clicked_per_app_session  = median(clicked_count),
    sd_event_per_app_session        = sd(event_count),
    sd_scrolled_per_app_session     = sd(scrolled_count),
    sd_clicked_per_app_session      = sd(clicked_count),
    
    .groups = "drop"
  )

event_per_ppid_vars <- event_level_df_processed %>%
  filter(Event %in% c("TYPE_VIEW_SCROLLED", "TYPE_VIEW_CLICKED")) %>%
  group_by(ppid, Event) %>%
  dplyr::summarize(event_count     = n(), .groups = "drop") %>%
  spread(key = Event, value = event_count, fill = 0) %>%
  mutate(scrollclick_ratio  = `TYPE_VIEW_SCROLLED` / `TYPE_VIEW_CLICKED`
         ) %>%
  select(ppid, scrollclick_ratio)

event_per_day_vars <- event_level_df_processed %>%
  # remove first, first and last day (As may have been incomplete)
  # and then days with pause.
  # must do removals in this order as if we remove pause and then first then we might remove
  # the first and last days that did not contain pauses, whereas if these overlap we only
  # want to include it once
  group_by(ppid) %>%
  filter(day != min(day) & day != max(day)) %>%  # Exclude first and last day per ppid
  ungroup() %>%
  anti_join(days_with_pause, by = c("ppid", "day")) %>%   # Exclude days with pause
  filter(Event == "TYPE_SCREEN_Mode_on") %>%
  group_by(ppid, day) %>%
  dplyr::summarize(screen_on_per_day = n()) %>%
  ungroup() %>%
  group_by(ppid) %>%
  dplyr::summarize(
    mean_screen_on_per_day   = mean(screen_on_per_day, na.rm = TRUE),
    median_screen_on_per_day = median(screen_on_per_day, na.rm = TRUE),
    sd_screen_on_per_day     = sd(screen_on_per_day, na.rm = TRUE)
    ) %>%
  ungroup()

time_per_day_vars <- session_level_df_processed %>%
  # remove first, first and last day (As may have been incomplete)
  # and then days with pause.
  # must do removals in this order as if we remove pause and then first then we might remove
  # the first and last days that did not contain pauses, whereas if these overlap we only
  # want to include it once
  group_by(ppid) %>%
  filter(day != min(day) & day != max(day)) %>%  # Exclude first and last day per ppid
  ungroup() %>%
  anti_join(days_with_pause, by = c("ppid", "day")) %>%   # Exclude days with pause
  # note that some participants have only days with pause so this gets rid of some participants - specifically, Z108, Z51 and M12 
  # (Z108 doesnt pause on Day 1 but it gets rid of this too as an incomplete first day)
  
  group_by(ppid) %>%
  summarise(mean_timespent_per_day_secs = sum(session_duration_secs) / n_distinct(day)) %>%
  mutate(mean_timespent_per_day_hrs = mean_timespent_per_day_secs / 3600) %>%
  ungroup()


################################################################################
# Merge into new participant and app level variables.

# Merge session_vars with participant_level_df
participant_level_df_merged <- participant_level_df_processed %>%
  left_join(proportion_appconsistency_per_ppid_vars, by = "ppid") %>%
  left_join(session_vars, by = "ppid") %>%
  left_join(app_session_per_ppid_vars, by = "ppid") %>%
  left_join(app_session_per_session_vars, by = "ppid") %>%
  left_join(event_per_session_vars, by = "ppid") %>%
  left_join(iti_per_ppid, by = "ppid") %>%
  left_join(event_per_app_session_vars, by = "ppid") %>%
  left_join(event_per_ppid_vars, by = "ppid") %>%
  left_join(event_per_day_vars, by = "ppid") %>%
  left_join(time_per_day_vars, by = "ppid")
  
# View the final merged data frame
colnames(participant_level_df_merged)

################################################################################
############                    Saved processed dfs            #################
################################################################################

save_directory <- stringi::stri_join(dirname(here()), "data_processed", "", sep = "/")
event_level_df_processed$Date_and_Time <- format(event_level_df_processed$Date_and_Time, "%Y-%m-%d %H:%M:%OS3")
write.csv(event_level_df_processed, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), event_level_filename), row.names = FALSE)
write_csv(app_session_level_df_processed, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), app_session_level_filename))
write_csv(session_level_df_processed, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), session_level_filename))
write_csv(participant_level_df_merged, str_c(save_directory, "/",format(Sys.time(), format = "%y%m%d_%H%M_"), participant_level_filename))



