
################################################################################
############# DESCRIPTIVES                                        ##############
############# Jan 2025.                                           ##############
############# Georgia Turner, georgianjt@gmail.com                ##############
################################################################################

# This loads in the cleaned and preprocessed dataframes and plots some descriptives.

rm(list = ls())
seed <- 1
set.seed(seed)

################## Setting up environment  #####################################
library(tidyverse)
library(here)
library(ggplot2)
library(tm); library(wordcloud2) # the libraries for the wordcloud
library(lubridate) # for recoding the date
library(corrplot) # for plotting cross correlations
library(Hmisc)  # for rcorr()
library(forcats)
library(rlang)
library(RColorBrewer)
library(BayesFactor)
library(nlme)
library(ggdist)
library(gridExtra)


here_dir   <- here()
parent_dir <- dirname(here_dir)
setwd(here_dir)

source('00-plotting_functions.R')

save_directory <- stringi::stri_join(dirname(here()), "data_processed", "", sep = "/")

################################################################################
############                load in data            ############################
################################################################################

######## CM dataset

filepath                   <- str_c(parent_dir, "/data_processed/")

CM_event_level_filename       <- "250909_1851_250908_1519_event_level_df.csv"
CM_app_session_level_filename <- "250909_1852_250908_1523_appsession_level_df.csv"
CM_session_level_filename     <- "250909_1852_250908_1523_session_level_df.csv"
CM_participant_level_filename <- "250909_1852_250908_1520_participant_level_df.csv"

#event_level_df               <- read.csv(str_c(filepath, CM_event_level_filename), stringsAsFactors = FALSE)
# Event level df takes much longer to load, so comment out unless explicitly needed
#event_level_df$Date_and_Time <- as.POSIXct(event_level_df$Date_and_Time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
CM_app_session_level_df         <- read_csv(str_c(filepath, CM_app_session_level_filename))
CM_session_level_df             <- read_csv(str_c(filepath, CM_session_level_filename))
CM_participant_level_df         <- read_csv(str_c(filepath, CM_participant_level_filename))
TableAppNameConversion_df    <- read_csv(str_c(filepath, 'TableS2.csv')) # list of all the conversions from raw answers to self-repoted MostAuto and LeastAuto Apps, to interpreted actual app names and underlying apps
GooglePlay_CategoryMap_df    <- read_csv(str_c(filepath, '260514_1452_GooglePlayAppCategory_Map.csv'))

######## ABCD dataset

ABCD_filename                 <- "261005_1216_abcd_appusage2_sevendays_qnr.csv"
ABCD_app_session_level_df     <- read_csv(str_c(filepath, ABCD_filename)) %>%
  rename(session_id       = phone_session_id, # previous name was phone_session_id, here we change to session_id
         ppid             = participant_id,
         App              = app) %>%
  mutate(app_session_id = row_number())

ABCD_mean_screen_time <- ABCD_app_session_level_df |>
  dplyr::summarise(
    sec_foreground = sum(sec_foreground, na.rm = TRUE),
    .by = c(ppid, dt)
  ) |>
  group_by(ppid) |>
  dplyr::summarise(
    mean_timespent_per_day_secs = mean(sec_foreground, na.rm = TRUE),
    mean_timespent_per_day_hrs  = (mean_timespent_per_day_secs / 3600),
    .groups = "drop"
  )

ABCD_participant_level_df     <- select(ABCD_app_session_level_df,
                                        ppid,
                                        mh_y_ysr__dsm__anx_tscore,
                                        mh_y_ysr__dsm__dep_tscore,
                                        mh_y_ysr__dsm__adhd_tscore
) %>% 
  group_by(ppid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(mean_timespent_per_day_hrs = ABCD_mean_screen_time$mean_timespent_per_day_hrs)


################################################################################
#################             plot descriptives     ############################
################################################################################

# This section contains exploratory descriptives which do not go in the final paper.

### CM Dataset

# Calculate the total frequency for each gender
total_frequency <- CM_participant_level_df %>%
  group_by(gender) %>%
  dplyr::summarize(Total = n())
# Create a bar chart with custom colors and labels
ggplot(CM_participant_level_df, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(aes(label = Total, y = Total), data = total_frequency, vjust = -0.5) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red", "Other" = "gray")) +
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency")

#### Plot questionnaire distributions
plot_density_by_gender(CM_participant_level_df, PHQ_8_score, 10)
plot_density_by_gender(CM_participant_level_df, GAD_7_score, 15)
plot_density_by_gender(CM_participant_level_df, OCI_R_score, 21)
plot_density_by_gender(CM_participant_level_df, SRHI_phone_use_score)
plot_density_by_gender(CM_participant_level_df, SRHI_social_media_use_score)
plot_density_by_gender(CM_participant_level_df, SRHI_most_automatic_app_score)
plot_density_by_gender(CM_participant_level_df, SRHI_least_automatic_app_score)
plot_density_by_gender(CM_participant_level_df, BIS_total_score)
plot_density_by_gender(CM_participant_level_df, BAS_total_score)
plot_density_by_gender(CM_participant_level_df, BAS_drive_score)
plot_density_by_gender(CM_participant_level_df, BAS_fun_seeking_score)
plot_density_by_gender(CM_participant_level_df, BAS_reward_responsiveness_score)

plot_density(CM_participant_level_df, SRHI_phone_use_score)
plot_density(CM_participant_level_df, SRHI_social_media_use_score)
plot_density(CM_participant_level_df, SRHI_most_automatic_app_score)
plot_density(CM_participant_level_df, SRHI_least_automatic_app_score)
plot_all_densities(CM_participant_level_df)


################################################################################
## General reporting info for methods section
################################################################################

#### count days
#day_df <- event_level_df %>% 
#  group_by(ppid) %>%
#  filter(day == max(day)) %>%
#  slice(1) %>%
#  ungroup()
#sd(day_df$day)
##mean(day_df$day)

################################################################################
## Fig 3: MostAuto and LeastAuto App names
################################################################################

## Add GooglePlay categories to table; only do this if not already done (so then set to F)
GooglePlayAdded <- T
if (!GooglePlayAdded) {
  TableAppNameConversion_df <- TableAppNameConversion_df %>%
    left_join(
      GooglePlay_CategoryMap_df %>%
        select(App, GooglePlay_category),
      by = c("Underlying_MostAuto_App_inDCAdata" = "App")
    ) %>%
    rename(
      MostAutoApp_GooglePlayCategory = GooglePlay_category
    ) %>%
    left_join(
      GooglePlay_CategoryMap_df %>%
        select(App, GooglePlay_category),
      by = c("Underlying_LeastAuto_App_inDCAdata" = "App")
    ) %>%
    rename(
      LeastAutoApp_GooglePlayCategory = GooglePlay_category
    )
  write_csv(TableAppNameConversion_df, str_c(filepath, 'TableS2.csv'))
}


unique(TableAppNameConversion_df$Raw_selfreported_MostAutoApp)
TableAppNameConversion_df_plotAutoApps <- filter(TableAppNameConversion_df, 
                                  Interpreted_MostAuto_App_name != "No unique App" &
                                    Interpreted_LeastAuto_App_name != "No unique App")

Fig3 <- plot_combined_apps(
  TableAppNameConversion_df_plotAutoApps,
  Interpreted_MostAuto_App_name, MostAuto_App_category,
  Interpreted_LeastAuto_App_name, LeastAuto_App_category
)
ggsave(str_c(parent_dir,"/output/figures/Fig3.pdf"),plot = Fig3, width = 10, height = 4, units = "in")

################################################################################
## Fig 4: MostAuto and LeastAuto App descriptives
################################################################################

## Fig 4a: SRHI questionnaire scores
#####################

plot_raincloud(CM_participant_level_df, SRHI_phone_use_score, "Phone Use")
plot_raincloud(CM_participant_level_df, SRHI_most_automatic_app_score, "Most Automatic", automatic_palette["Most Automatic"])
plot_raincloud(CM_participant_level_df, SRHI_social_media_use_score, "Social Media Use")
plot_raincloud(CM_participant_level_df, SRHI_least_automatic_app_score, "Least Automatic", automatic_palette["Least Automatic"])
Fig4a <- plot_combined_rainclouds_SRHI(CM_participant_level_df, automatic_palette)
ggsave(str_c(parent_dir,"/output/figures/Fig4a.pdf"), plot = Fig4a, width = 10, height = 7, units = "in")

######### stats ######### 

# calculate exclusion datasets

SRHI_least_automatic_app_score_outliers = find_outliers(CM_participant_level_df, "SRHI_least_automatic_app_score")$outlier_ids
CM_participant_level_df_NoSRHILeastOutliers <- filter(CM_participant_level_df, !(ppid %in% SRHI_least_automatic_app_score_outliers))
ppid_failed_attention <- c("Z73", "Z109", "M7" ,  "Z54" , "Z115" ,"Z141", "Z137","Z55") # participants who failed at least one attention check
CM_participant_level_df_NoAttFail <- filter(CM_participant_level_df, !(ppid %in% ppid_failed_attention))

# SRHI most vs. least auto app

# no exclusions
t.test(CM_participant_level_df$SRHI_most_automatic_app_score, CM_participant_level_df$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df$SRHI_most_automatic_app_score,
  y = CM_participant_level_df$SRHI_least_automatic_app_score,
  paired = TRUE
)

# outliers excluded
t.test(CM_participant_level_df_NoSRHILeastOutliers$SRHI_most_automatic_app_score, CM_participant_level_df_NoSRHILeastOutliers$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoSRHILeastOutliers$SRHI_most_automatic_app_score,
  y = CM_participant_level_df_NoSRHILeastOutliers$SRHI_least_automatic_app_score,
  paired = TRUE
)

# inattentive participants excluded
t.test(CM_participant_level_df_NoAttFail$SRHI_most_automatic_app_score, CM_participant_level_df_NoAttFail$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoAttFail$SRHI_most_automatic_app_score,
  y = CM_participant_level_df_NoAttFail$SRHI_least_automatic_app_score,
  paired = TRUE
)


# SRHI least auto app vs. social media and phone

# no exclusions
t.test(CM_participant_level_df$SRHI_social_media_use_score, CM_participant_level_df$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df$SRHI_social_media_use_score,
  y = CM_participant_level_df$SRHI_least_automatic_app_score,
  paired = TRUE
)
t.test(CM_participant_level_df$SRHI_phone_use_score, CM_participant_level_df$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df$SRHI_phone_use_score,
  y = CM_participant_level_df$SRHI_least_automatic_app_score,
  paired = TRUE
)

# outliers excluded
t.test(CM_participant_level_df_NoSRHILeastOutliers$SRHI_social_media_use_score, CM_participant_level_df_NoSRHILeastOutliers$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoSRHILeastOutliers$SRHI_social_media_use_score,
  y = CM_participant_level_df_NoSRHILeastOutliers$SRHI_least_automatic_app_score,
  paired = TRUE
)
t.test(CM_participant_level_df_NoSRHILeastOutliers$SRHI_phone_use_score, CM_participant_level_df_NoSRHILeastOutliers$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoSRHILeastOutliers$SRHI_phone_use_score,
  y = CM_participant_level_df_NoSRHILeastOutliers$SRHI_least_automatic_app_score,
  paired = TRUE
)

# inattentive participants excluded
t.test(CM_participant_level_df_NoAttFail$SRHI_social_media_use_score, CM_participant_level_df_NoAttFail$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoAttFail$SRHI_social_media_use_score,
  y = CM_participant_level_df_NoAttFail$SRHI_least_automatic_app_score,
  paired = TRUE
)
t.test(CM_participant_level_df_NoAttFail$SRHI_phone_use_score, CM_participant_level_df_NoAttFail$SRHI_least_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoAttFail$SRHI_phone_use_score,
  y = CM_participant_level_df_NoAttFail$SRHI_least_automatic_app_score,
  paired = TRUE
)

# most auto app 

# no exclusions
t.test(CM_participant_level_df$SRHI_social_media_use_score, CM_participant_level_df$SRHI_most_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df$SRHI_social_media_use_score,
  y = CM_participant_level_df$SRHI_most_automatic_app_score,
  paired = TRUE
)
t.test(CM_participant_level_df$SRHI_phone_use_score, CM_participant_level_df$SRHI_most_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df$SRHI_phone_use_score,
  y = CM_participant_level_df$SRHI_most_automatic_app_score,
  paired = TRUE
)

# inattentive participants excluded
t.test(CM_participant_level_df_NoAttFail$SRHI_social_media_use_score, CM_participant_level_df_NoAttFail$SRHI_most_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoAttFail$SRHI_social_media_use_score,
  y = CM_participant_level_df_NoAttFail$SRHI_most_automatic_app_score,
  paired = TRUE
)
t.test(CM_participant_level_df_NoAttFail$SRHI_phone_use_score, CM_participant_level_df_NoAttFail$SRHI_most_automatic_app_score, paired = T)
ttestBF(
  x = CM_participant_level_df_NoAttFail$SRHI_phone_use_score,
  y = CM_participant_level_df_NoAttFail$SRHI_most_automatic_app_score,
  paired = TRUE
)

## Fig 4b: App session duration
#####################

automatic_app_ppids <- CM_participant_level_df %>% 
  filter(!(most_automatic_app_recoded == "no_unique_app") & # no unique app is where we couldnt work out what they meant as it could have been multiple, so excluding these 
           !(least_automatic_app_recoded== "no_unique_app"))

automatic_app_appsessions_duration <- CM_app_session_level_df %>% 
  right_join(automatic_app_ppids, by = "ppid") %>%  
  filter(App == most_automatic_app_recoded | App == least_automatic_app_recoded) %>% 
  group_by(ppid) %>% 
  summarise(
    mean_duration_most    = mean(app_session_duration_secs[most_automatic_app_recoded == App], na.rm = TRUE),
    mean_duration_least   = mean(app_session_duration_secs[least_automatic_app_recoded == App], na.rm = TRUE),
    median_duration_most  = median(app_session_duration_secs[most_automatic_app_recoded == App], na.rm = TRUE),
    median_duration_least = median(app_session_duration_secs[least_automatic_app_recoded == App], na.rm = TRUE),
    sd_duration_most      = sd(app_session_duration_secs[most_automatic_app_recoded == App], na.rm = TRUE),
    sd_duration_least     = sd(app_session_duration_secs[least_automatic_app_recoded == App], na.rm = TRUE)
  ) %>%
  filter(!is.nan(mean_duration_most) & !is.nan(mean_duration_least))  # Remove NaN values
# note that we only filter for mean not sd. Therefore e.g. participant M5 has sd_leastAuto as NA because they only used the leastAuto App once so it has a mean not an SD

# Call the function for each statistic (mean, median, or sd)
plot_statistic("mean")   # For mean
plot_statistic("median") # For median
plot_statistic("sd")     # For standard deviation
Fig4b <- plot_statistic("mean")
ggsave(str_c(parent_dir,"/output/figures/Fig4b.pdf"), plot = Fig4b, width = 5, height = 7, units = "in")

######### stats ######### 

t.test(
  automatic_app_appsessions_duration$mean_duration_most,
  automatic_app_appsessions_duration$mean_duration_least,
  paired = TRUE
)
ttestBF(
  x = automatic_app_appsessions_duration$mean_duration_most,
  y = automatic_app_appsessions_duration$mean_duration_least,
  paired = TRUE
)

## Fig 4c: Proportion of sessions that App appears
#####################

automatic_app_appsessions <- CM_app_session_level_df %>%  
  right_join(automatic_app_ppids, by = "ppid") %>%  
  select(ppid, App, most_automatic_app_recoded, least_automatic_app_recoded, session_id, app_session_duration_secs) %>%
  group_by(session_id) %>%
  mutate(
    most_autoapp_in_session  = as.integer(any(App == most_automatic_app_recoded)),
    least_autoapp_in_session = as.integer(any(App == least_automatic_app_recoded))
  ) %>%
  slice(1) %>%
  ungroup()


# Summarize data: Calculate the proportion for each ppid
ppid_proportions <- automatic_app_appsessions %>%
  group_by(ppid) %>%
  summarise(
    `Most Automatic`  = mean(most_autoapp_in_session),
    `Least Automatic` = mean(least_autoapp_in_session)
  ) %>%
  pivot_longer(cols      = c(`Most Automatic`, `Least Automatic`), 
               names_to  = "app_type", 
               values_to = "proportion")

overall_means <- ppid_proportions %>%
  group_by(app_type) %>%
  summarise(mean_proportion = mean(proportion))

mean(filter(ppid_proportions, app_type == "Most Automatic")$proportion)
sd(filter(ppid_proportions, app_type == "Most Automatic")$proportion)
mean(filter(ppid_proportions, app_type == "Least Automatic")$proportion)
sd(filter(ppid_proportions, app_type == "Least Automatic")$proportion)

# Determine the y-position for annotation
y_max <- max(ppid_proportions$proportion) + 0.05

Fig4c <- ggplot(ppid_proportions, aes(x = app_type, y = proportion, fill = app_type)) +
  stat_halfeye(
    adjust = 0.4,
    justification = -0.3,
    width = 0.4,
    point_colour = NA,
    slab_size = 0.5,
    interval_size = NA
  ) +
  geom_boxplot(
    width = 0.1,
    outlier.shape = NA,
    alpha = 0.5,
    aes(color = app_type)
  ) +
  geom_jitter(
    aes(color = app_type),
    width = 0.08,
    alpha = 0.3,
    size = 2,
    stroke = 0,
    shape = 16
  ) +
  geom_line(
    data = overall_means,
    aes(x = app_type, y = mean_proportion, group = 1),
    color = "black", linewidth = 1
  ) +
  geom_point(
    data = overall_means,
    aes(x = app_type, y = mean_proportion),
    color = "black", size = 4, shape = 21
  ) +
  geom_segment(
    aes(x = 1, xend = 2, y = y_max, yend = y_max),
    color = "black"
  ) +
  labs(
    title = "Proportion of Sessions with Most/Least Automatic Apps per ppid",
    x = "App Type",
    y = "Proportion of Sessions"
  ) +
  scale_fill_manual(values = automatic_palette) +
  scale_color_manual(values = automatic_palette) +
  theme_classic() +
  theme(legend.position = "none")
ggsave(str_c(parent_dir,"/output/figures/Fig4c.pdf"), plot = Fig4c, width = 5, height = 7, units = "in")

######### stats ######### 

# Conduct a paired t-test
t.test(
  ppid_proportions$proportion[ppid_proportions$app_type == "Most Automatic"],
  ppid_proportions$proportion[ppid_proportions$app_type == "Least Automatic"],
  paired = TRUE
)
ttestBF(
  x = ppid_proportions$proportion[ppid_proportions$app_type == "Most Automatic"],
  y = ppid_proportions$proportion[ppid_proportions$app_type == "Least Automatic"],
  paired = TRUE
)

## Fig 4d: Proportion of each position in session that each App appears (first, second, penultimate, last)
#####################


automatic_app_sessions <- CM_session_level_df %>% 
  right_join(automatic_app_ppids, by = "ppid") %>%
  group_by(ppid) %>%
  mutate(
    first_app_mostAuto        = mean(!is.na(first_app) & first_app == most_automatic_app_recoded, na.rm = TRUE),
    second_app_mostAuto       = mean(!is.na(second_app) & second_app == most_automatic_app_recoded, na.rm = TRUE),
    penultimate_app_mostAuto  = mean(!is.na(penultimate_app) & penultimate_app == most_automatic_app_recoded, na.rm = TRUE),
    last_app_mostAuto         = mean(!is.na(last_app) & last_app == most_automatic_app_recoded, na.rm = TRUE),
    first_app_leastAuto       = mean(!is.na(first_app) & first_app == least_automatic_app_recoded, na.rm = TRUE),
    second_app_leastAuto      = mean(!is.na(second_app) & second_app == least_automatic_app_recoded, na.rm = TRUE),
    penultimate_app_leastAuto = mean(!is.na(penultimate_app) & penultimate_app == least_automatic_app_recoded, na.rm = TRUE),
    last_app_leastAuto        = mean(!is.na(last_app) & last_app == least_automatic_app_recoded, na.rm = TRUE)
  ) %>%
  slice(1) %>%
  ungroup()


# Pivot to long format, keeping only the numeric proportion columns
plot_data_appsession_positions <- automatic_app_sessions %>%
  select(ppid, first_app_mostAuto, second_app_mostAuto, penultimate_app_mostAuto, last_app_mostAuto,
         first_app_leastAuto, second_app_leastAuto, penultimate_app_leastAuto, last_app_leastAuto) %>%
  pivot_longer(cols = -ppid, 
               names_to = c("app_position", "auto_type"), 
               names_pattern = "(first|second|penultimate|last)_app_(mostAuto|leastAuto)") %>%
  mutate(app_position = factor(app_position, 
                               levels = c("first", "second", "penultimate", "last")))

# Compute mean proportion for each app position & auto type
summary_data <- plot_data_appsession_positions %>%
  group_by(app_position, auto_type) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value   = sd(value, na.rm = TRUE),
    n          = sum(!is.na(value)),
    se_value   = sd_value / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    auto_type = recode(auto_type,
                       "mostAuto"  = "Most Automatic",
                       "leastAuto" = "Least Automatic"),
    # 95% CI (optional)
    ci_lower = mean_value - 1.96 * se_value,
    ci_upper = mean_value + 1.96 * se_value
  )

Fig4d <- ggplot(summary_data, aes(x = app_position, y = mean_value, fill = auto_type, group = auto_type)) +
  geom_line(aes(color = auto_type), size = 1, alpha = 0.5) +
  geom_point(size = 5, shape = 21, color = "black") +
  geom_errorbar(aes(ymin = ci_upper,
                    ymax = ci_lower,
                    color = auto_type),
                width = 0.15, alpha = 0.7) +
  labs(
    x = "App Position",
    y = "Proportion",
    title = "Proportion of Most & Least Automatic Apps by Position"
  ) +
  scale_color_manual(values = automatic_palette) +
  scale_fill_manual(values = automatic_palette) +
  theme_classic()
ggsave(str_c(parent_dir,"/output/figures/Fig4d.pdf"),, plot = Fig4d, width = 6.5, height = 7, units = "in")


######### stats ######### 

# Create difference scores per participant per position
diff_df <- automatic_app_sessions %>%
  transmute(
    ppid,
    first       = first_app_mostAuto - first_app_leastAuto,
    second      = second_app_mostAuto - second_app_leastAuto,
    penultimate = penultimate_app_mostAuto - penultimate_app_leastAuto,
    last        = last_app_mostAuto - last_app_leastAuto
  ) %>%
  pivot_longer(
    cols = -ppid,
    names_to = "position",
    values_to = "difference"
  ) %>%
  mutate(
    ppid = as.factor(ppid),
    position = factor(position, levels = c("first", "second", "penultimate", "last")))

diff_df$ppid <- as.factor(diff_df$ppid)
diff_df$position <- as.factor(diff_df$position)

anova_lme <- lme(difference ~ position, random = ~1 | ppid, data = diff_df)
anova(anova_lme)
anovaBF(
  difference ~ position + ppid,
  data = diff_df,
  whichRandom = "ppid"
)

#############
# Now do post-hoc tests

# Get all unique pairs of positions
pairs <- combn(levels(diff_df$position), 2, simplify = FALSE)

# Run paired t-tests for each pair
ttest_results <- map_dfr(pairs, function(pair) {
  wide_pair <- diff_df %>%
    filter(position %in% pair) %>%
    pivot_wider(names_from = position, values_from = difference)
  
  ttest <- t.test(wide_pair[[pair[1]]], wide_pair[[pair[2]]], paired = TRUE)
  ttest_Bayes <- ttestBF(
    x = wide_pair[[pair[1]]],
    y = wide_pair[[pair[2]]],
    paired = TRUE
  )
  
  tibble(
    contrast = paste(pair[1], "vs", pair[2]),
    t_freq   = as.numeric(ttest$statistic),
    df_freq  = ttest$parameter,
    p_freq   = ttest$p.value,
    BF       = as.vector(ttest_Bayes)
  )
})

ttest_results

################################################################################
## Fig S1: Questionnaire cross-correlations
################################################################################

# A: CM dataset

qnr_vars <- CM_participant_level_df %>% dplyr::select(PHQ_8_score, GAD_7_score, OCI_R_score, ATQ_short_score,
                                     SRHI_phone_use_score, SRHI_social_media_use_score, 
                                     SRHI_most_automatic_app_score, SRHI_least_automatic_app_score, 
                                     BIS_total_score,BAS_fun_seeking_score,BAS_drive_score,
                                     BAS_reward_responsiveness_score, mean_timespent_per_day_hrs )


qnr_vars_cor_matrix <- cor(qnr_vars, use = "pairwise.complete.obs")

# Assume qnr_vars is your data frame with variables of interest
cor_results <- rcorr(as.matrix(qnr_vars))  # calculates r and p values

###### make figure

# Extract matrices
cor_matrix <- cor_results$r
p_matrix <- cor_results$P
p_matrix[which(is.na(p_matrix))] <- 1
##### Figure S1
FigS1a <- corrplot(cor_matrix, method = "color", 
         p.mat = p_matrix, sig.level = c(0.001, 0.01, 0.05), 
         insig = "label_sig", pch.cex = 2.5)
pdf(
  str_c(parent_dir, "/output/figures/FigS1a.pdf"),
  width = 12,
  height = 12
)
p_matrix[upper.tri(p_matrix)] <- t(p_matrix)[upper.tri(p_matrix)]
corrplot(cor_matrix, method = "color", 
         p.mat = p_matrix, sig.level = c(0.001, 0.01, 0.05), 
         insig = "label_sig", pch.cex = 2.7, tl.pos = "n") 
dev.off()

###### calculate stats

cor_results$r["PHQ_8_score", "GAD_7_score"]
cor_results$P["PHQ_8_score", "GAD_7_score"]
correlationBF(CM_participant_level_df$PHQ_8_score, CM_participant_level_df$GAD_7_score)

cor_results$r["PHQ_8_score", "OCI_R_score"]
cor_results$P["PHQ_8_score", "OCI_R_score"]
correlationBF(CM_participant_level_df$PHQ_8_score, CM_participant_level_df$OCI_R_score)

cor_results$r["GAD_7_score", "OCI_R_score"]
cor_results$P["GAD_7_score", "OCI_R_score"]
correlationBF(CM_participant_level_df$GAD_7_score, CM_participant_level_df$OCI_R_score)

cor_results$r["PHQ_8_score", "ATQ_short_score"]
cor_results$P["PHQ_8_score", "ATQ_short_score"]
correlationBF(CM_participant_level_df$PHQ_8_score, CM_participant_level_df$ATQ_short_score)

cor_results$r["GAD_7_score", "ATQ_short_score"]
cor_results$P["GAD_7_score", "ATQ_short_score"]
correlationBF(CM_participant_level_df$GAD_7_score, CM_participant_level_df$ATQ_short_score)

cor_results$r["PHQ_8_score", "BIS_total_score"]
cor_results$P["PHQ_8_score", "BIS_total_score"]
correlationBF(CM_participant_level_df$PHQ_8_score, CM_participant_level_df$BIS_total_score)

cor_results$r["GAD_7_score", "BIS_total_score"]
cor_results$P["GAD_7_score", "BIS_total_score"]
correlationBF(CM_participant_level_df$GAD_7_score, CM_participant_level_df$BIS_total_score)

cor_results$r["ATQ_short_score", "BIS_total_score"]
cor_results$P["ATQ_short_score", "BIS_total_score"]
correlationBF(CM_participant_level_df$GAD_7_score, CM_participant_level_df$BIS_total_score)

cor_results$r["SRHI_phone_use_score", "SRHI_social_media_use_score"]
cor_results$P["SRHI_phone_use_score", "SRHI_social_media_use_score"]
correlationBF(CM_participant_level_df$SRHI_phone_use_score, CM_participant_level_df$SRHI_social_media_use_score)

cor_results$r["SRHI_phone_use_score", "SRHI_most_automatic_app_score"]
cor_results$P["SRHI_phone_use_score", "SRHI_most_automatic_app_score"]
correlationBF(CM_participant_level_df$SRHI_phone_use_score, CM_participant_level_df$SRHI_most_automatic_app_score)

cor_results$r["SRHI_social_media_use_score", "SRHI_most_automatic_app_score"]
cor_results$P["SRHI_social_media_use_score", "SRHI_most_automatic_app_score"]
correlationBF(CM_participant_level_df$SRHI_social_media_use_score, CM_participant_level_df$SRHI_most_automatic_app_score)

cor_results$r["SRHI_phone_use_score", "mean_timespent_per_day_hrs"]
cor_results$P["SRHI_phone_use_score", "mean_timespent_per_day_hrs"]
correlationBF(CM_participant_level_df$SRHI_phone_use_score, CM_participant_level_df$mean_timespent_per_day_hrs)

cor_results$r["SRHI_social_media_use_score", "mean_timespent_per_day_hrs"]
cor_results$P["SRHI_social_media_use_score", "mean_timespent_per_day_hrs"]
correlationBF(CM_participant_level_df$SRHI_social_media_use_score, CM_participant_level_df$mean_timespent_per_day_hrs)

cor_results$r["BIS_total_score", "BAS_fun_seeking_score"]
cor_results$P["BIS_total_score", "BAS_fun_seeking_score"]
correlationBF(CM_participant_level_df$BIS_total_score, CM_participant_level_df$BAS_fun_seeking_score)

cor_results$r["BAS_fun_seeking_score", "BAS_drive_score"]
cor_results$P["BAS_fun_seeking_score", "BAS_drive_score"]
correlationBF(CM_participant_level_df$BAS_fun_seeking_score, CM_participant_level_df$BAS_drive_score)

cor_results$r["BAS_fun_seeking_score", "BAS_reward_responsiveness_score"]
cor_results$P["BAS_fun_seeking_score", "BAS_reward_responsiveness_score"]
correlationBF(CM_participant_level_df$BAS_fun_seeking_score, CM_participant_level_df$BAS_reward_responsiveness_score)

cor_results$r["BAS_reward_responsiveness_score", "BAS_drive_score"]
cor_results$P["BAS_reward_responsiveness_score", "BAS_drive_score"]
correlationBF(CM_participant_level_df$BAS_reward_responsiveness_score, CM_participant_level_df$BAS_drive_score)

# B: ABCD dataset

ABCD_qnr_vars <- ABCD_participant_level_df %>% dplyr::select(mh_y_ysr__dsm__anx_tscore, 
                                                             mh_y_ysr__dsm__dep_tscore,
                                                             mh_y_ysr__dsm__adhd_tscore,
                                                             mean_timespent_per_day_hrs)


ABCD_qnr_vars_cor_matrix <- cor(ABCD_qnr_vars, use = "pairwise.complete.obs")

# Assume qnr_vars is your data frame with variables of interest
ABCD_cor_results <- rcorr(as.matrix(ABCD_qnr_vars))  # calculates r and p values

###### make figure

# Extract matrices
ABCD_cor_matrix <- ABCD_cor_results$r
ABCD_p_matrix <- ABCD_cor_results$P
ABCD_p_matrix[which(is.na(ABCD_p_matrix))] <- 1

##### Figure S1b
FigS1b <- corrplot(ABCD_cor_matrix, method = "color", 
                  p.mat = ABCD_p_matrix, sig.level = c(0.001, 0.01, 0.05), 
                  insig = "label_sig", pch.cex = 2.3)
pdf(str_c(parent_dir,"/output/figures/FigS1b.pdf"),
    width = 12, height = 12)
ABCD_p_matrix[upper.tri(ABCD_p_matrix)] <- t(ABCD_p_matrix)[upper.tri(ABCD_p_matrix)]
corrplot(ABCD_cor_matrix, method = "color", 
         p.mat = ABCD_p_matrix, sig.level = c(0.001, 0.01, 0.05), 
         insig = "label_sig", pch.cex = 5, tl.pos = "n") 
dev.off()

###### calculate stats

ABCD_cor_results$r["mh_y_ysr__dsm__anx_tscore", "mh_y_ysr__dsm__dep_tscore"]
ABCD_cor_results$P["mh_y_ysr__dsm__anx_tscore", "mh_y_ysr__dsm__dep_tscore"]
correlationBF(ABCD_participant_level_df$mh_y_ysr__dsm__anx_tscore, ABCD_participant_level_df$mh_y_ysr__dsm__dep_tscore)

ABCD_cor_results$r["mh_y_ysr__dsm__adhd_tscore", "mh_y_ysr__dsm__dep_tscore"]
ABCD_cor_results$P["mh_y_ysr__dsm__adhd_tscore", "mh_y_ysr__dsm__dep_tscore"]
correlationBF(ABCD_participant_level_df$mh_y_ysr__dsm__adhd_tscore, ABCD_participant_level_df$mh_y_ysr__dsm__dep_tscore)

ABCD_cor_results$r["mh_y_ysr__dsm__adhd_tscore", "mh_y_ysr__dsm__anx_tscore"]
ABCD_cor_results$P["mh_y_ysr__dsm__adhd_tscore", "mh_y_ysr__dsm__anx_tscore"]
correlationBF(ABCD_participant_level_df$mh_y_ysr__dsm__adhd_tscore, ABCD_participant_level_df$mh_y_ysr__dsm__anx_tscore)

# corrs with time spent 
ABCD_cor_results$r["mean_timespent_per_day_hrs", "mh_y_ysr__dsm__dep_tscore"]
ABCD_cor_results$P["mean_timespent_per_day_hrs", "mh_y_ysr__dsm__dep_tscore"]
correlationBF(ABCD_participant_level_df$mean_timespent_per_day_hrs, ABCD_participant_level_df$mh_y_ysr__dsm__dep_tscore)

ABCD_cor_results$r["mean_timespent_per_day_hrs", "mh_y_ysr__dsm__anx_tscore"]
ABCD_cor_results$P["mean_timespent_per_day_hrs", "mh_y_ysr__dsm__anx_tscore"]
correlationBF(ABCD_participant_level_df$mean_timespent_per_day_hrs, ABCD_participant_level_df$mh_y_ysr__dsm__anx_tscore)

ABCD_cor_results$r["mean_timespent_per_day_hrs", "mh_y_ysr__dsm__adhd_tscore"]
ABCD_cor_results$P["mean_timespent_per_day_hrs", "mh_y_ysr__dsm__adhd_tscore"]
correlationBF(ABCD_participant_level_df$mean_timespent_per_day_hrs, ABCD_participant_level_df$mh_y_ysr__dsm__adhd_tscore)



