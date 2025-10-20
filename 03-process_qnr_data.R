##################################################################################
##################################################################################
############        Mental health questionnaire coding      ######################
##################################################################################
##################################################################################

mh_data_raw <- read.csv("CICESE- Main - Georgia owned_August 30, 2024_04.03.csv")

##################################################################################
############        Renaming columns                        ######################
##################################################################################


mh_data_clean <- mh_data_raw %>%
  
  dplyr::rename('ppid' = 'Q24')%>% # naming the ppid column %>%
  dplyr::mutate(ppid = toupper(ppid)) %>%
  dplyr::rename('gender' = 'Q25.1')%>% # naming gender column 
  dplyr::rename('duration_secs' = 'Duration..in.seconds.')%>% # time taken to do questionnaire 
  dplyr::rename('date' = 'RecordedDate')%>% # date of qnr 
  
  ###### Quality / compliance checks - important to do these first because taking the attention checks out of the previous Q4 etc
  dplyr::rename('quality_precommitment' = 'Q30') %>% 
  dplyr::rename('quality_postcommitment' = 'Q32') %>% 
  dplyr::rename('quality_write2engsentences' = 'Q31') %>% # identifying and labelling attention check q1
  dplyr::rename_with(~ gsub("Q4_11", "quality_attention_check_1", .)) %>% # identifying and labelling attention check q1
  dplyr::rename_with(~ gsub("Q4_13", "quality_attention_check_2", .)) %>% # identifying and labelling attention check q2
  dplyr::rename_with(~ gsub("Q13_12", "quality_attention_check_3", .)) %>% # identifying and labelling attention check q1
  
  ###### Questions of interest
  dplyr::rename('most_automatic_app_self_reported' = 'Q23')%>% # naming most automatic app (self report) column
  dplyr::rename('least_automatic_app_self_reported' = 'Q24.1')%>% # naming least automatic app (self report) column
  dplyr::rename_with(~ gsub("SRHI_", "SRHI_phone_use_", .)) %>% # renaming SRHI (on phone use)
  dplyr::rename_with(~ gsub("Q19_", "SRHI_social_media_use_", .)) %>% # renaming SRHI (on social media use)
  dplyr::rename_with(~ gsub("Q20_", "SRHI_most_automatic_app_", .)) %>% # renaming SRHI (on their self report most automatically used app)
  dplyr::rename_with(~ gsub("Q22_", "SRHI_least_automatic_app_", .))  %>% # renaming SRHI (on their self report least automatically used app)
  dplyr::rename('This_week_did_you_use_any_social_media_sites_on_an_Internet_browser' = 'Q25')%>%
  dplyr::rename('If_so_which_sites_did_you_use' = 'Q27')%>%
  dplyr::rename('On_average_how_many_hours_did_you_spend_on_each_of_these_sites_via_the_browser_last_week' = 'Q29')%>%
  dplyr::rename('Without_looking_at_the_screen_time_on_your_phone_what_would_you_estimate_is_the_average_amount_of_hours_you_spent_on_your_phone_this_week' = 'Q31.1')%>%
  dplyr::rename('On_average_I_receive_notifications_on_my_phone_screen' = 'Q22')%>%
  dplyr::rename('Do_you_have_any_other_comments_about_your_phone_use' = 'Q32.1')%>%
  dplyr::rename('Do_you_have_any_other_comments_about_this_study_in_general' = 'Q33')%>%
  dplyr::rename_with(~ gsub("Q8_", "PHQ8_", .))%>% # renaming PHQ-8 
  dplyr::rename_with(~ gsub("Q9_", "GAD7_", .))%>% # renaming GAD_7
  dplyr::rename_with(~ paste0("Q13_", seq_along(.)), starts_with("Q13_"))%>% # change to get rid of the attention check that was Q13_12
  dplyr::rename_with(~ gsub("Q13_", "OCI_R_", .)) %>%# renaming 0CI
  dplyr::rename_with(~ paste0("Q4_", seq_along(.)), starts_with("Q4_"))%>% # change to get rid of the attention check that was Q4_11 and
  dplyr::rename_with(~ gsub("Q4_", "BISBAS_", .)) %>% # renaming BISBAS
  dplyr::rename_with(~ gsub("Q23_", "ATQ_short_", .)) %>%  # renaming ATQ

  dplyr::select(ppid, 
                gender,
                date,
                duration_secs,
                Finished,
                ppid,
                
                quality_precommitment,
                quality_postcommitment,
                quality_write2engsentences,
                quality_attention_check_1,
                quality_attention_check_2,
                quality_attention_check_3,
                
                most_automatic_app_self_reported,
                least_automatic_app_self_reported,
                
                SRHI_phone_use_1,
                SRHI_phone_use_2,
                SRHI_phone_use_3,
                SRHI_phone_use_4,
                SRHI_phone_use_5,
                SRHI_phone_use_6,
                SRHI_phone_use_7,
                SRHI_phone_use_8,
                SRHI_phone_use_9,
                SRHI_phone_use_10,
                SRHI_phone_use_11,
                SRHI_phone_use_12,
                
                SRHI_social_media_use_1,
                SRHI_social_media_use_2,
                SRHI_social_media_use_3,
                SRHI_social_media_use_4,
                SRHI_social_media_use_5,
                SRHI_social_media_use_6,
                SRHI_social_media_use_7,
                SRHI_social_media_use_8,
                SRHI_social_media_use_9,
                SRHI_social_media_use_10,
                SRHI_social_media_use_11,
                SRHI_social_media_use_12,
                
                SRHI_most_automatic_app_1,
                SRHI_most_automatic_app_2,
                SRHI_most_automatic_app_3,
                SRHI_most_automatic_app_4,
                SRHI_most_automatic_app_5,
                SRHI_most_automatic_app_6,
                SRHI_most_automatic_app_7,
                SRHI_most_automatic_app_8,
                SRHI_most_automatic_app_9,
                SRHI_most_automatic_app_10,
                SRHI_most_automatic_app_11,
                SRHI_most_automatic_app_12,
                
                SRHI_least_automatic_app_1,
                SRHI_least_automatic_app_2,
                SRHI_least_automatic_app_3,
                SRHI_least_automatic_app_4,
                SRHI_least_automatic_app_5,
                SRHI_least_automatic_app_6,
                SRHI_least_automatic_app_7,
                SRHI_least_automatic_app_8,
                SRHI_least_automatic_app_9,
                SRHI_least_automatic_app_10,
                SRHI_least_automatic_app_11,
                SRHI_least_automatic_app_12,
                
                BISBAS_1,
                BISBAS_2,
                BISBAS_3,
                BISBAS_4,
                BISBAS_5,
                BISBAS_6,
                BISBAS_7,
                BISBAS_8,
                BISBAS_9,
                BISBAS_10,
                BISBAS_11,
                BISBAS_12,
                BISBAS_13,
                BISBAS_14,
                BISBAS_15,
                BISBAS_16,
                BISBAS_17,
                BISBAS_18,
                BISBAS_19,
                BISBAS_20,
                BISBAS_21,
                BISBAS_22,
                BISBAS_23,
                BISBAS_24,
                
                PHQ8_1,
                PHQ8_2,
                PHQ8_3,
                PHQ8_4,
                PHQ8_5,
                PHQ8_6,
                PHQ8_7,
                PHQ8_8,
                
                GAD7_1,
                GAD7_2,
                GAD7_3,
                GAD7_4,
                GAD7_5,
                GAD7_6,
                GAD7_7,

                OCI_R_1,
                OCI_R_2,
                OCI_R_3,
                OCI_R_4,
                OCI_R_5,
                OCI_R_6,
                OCI_R_7,
                OCI_R_8,
                OCI_R_9,
                OCI_R_10,
                OCI_R_11,
                OCI_R_12,
                OCI_R_13,
                OCI_R_14,
                OCI_R_15,
                OCI_R_16,
                OCI_R_17,
                OCI_R_18,
                
                ATQ_short_1,
                ATQ_short_2,
                ATQ_short_3,
                ATQ_short_4,
                ATQ_short_5,
                ATQ_short_6,
                ATQ_short_7,
                ATQ_short_8,
                ATQ_short_9,
                ATQ_short_10,
                
                This_week_did_you_use_any_social_media_sites_on_an_Internet_browser,
                If_so_which_sites_did_you_use,
                On_average_how_many_hours_did_you_spend_on_each_of_these_sites_via_the_browser_last_week,
                Without_looking_at_the_screen_time_on_your_phone_what_would_you_estimate_is_the_average_amount_of_hours_you_spent_on_your_phone_this_week,
                On_average_I_receive_notifications_on_my_phone_screen,
                Do_you_have_any_other_comments_about_your_phone_use,
                Do_you_have_any_other_comments_about_this_study_in_general
                ) 




##################################################################################
##################################################################################
##########                SCORING THE SCREENERS                        ###########
##################################################################################
##################################################################################

mh_data <- mh_data_clean

##################################################################################
#################.            THE 4 DIFFERENT SHRI            ####################
##################################################################################


# In accordance with the original SRHI publication, 
# Verplanken & Orbell 2003 'Reflections on Past Behaviour: A Self-Report Index of Habit Strength,
# scores on the Likert scale (here, 5 point scale) are recoded so that higher scores = stronger habits.
# for each SRHI, the lowest score possible is 12 * strongly disagree = 12, 
# and the highest score possible is 12 * strongly agree = 60

# all SRHI columns - for replacing the answers with numerical scores
SRHI_columns <- grep('SRHI', names(mh_data), value = TRUE)

# About the specific item - for adding up the scores later on
SRHI_phone_use_columns            <- grep('SRHI_phone_use', names(mh_data), value = TRUE)
SRHI_social_media_use_columns     <- grep('SRHI_social_media_use', names(mh_data), value = TRUE)
SRHI_most_automatic_app_columns   <- grep('SRHI_most', names(mh_data), value = TRUE)
SRHI_least_automatic_app_columns  <- grep('SRHI_least', names(mh_data), value = TRUE)

# Recode rules 
recode.rules.SRHI  <- function(x) {
  x <- gsub('Strongly disagree', 1, x)
  x <- gsub('Somwhat disagree', 2, x) # THERE IS A TYPO IN THE ORIGINAL QUESTIONNAIRE...
  x <- gsub('Neither agree nor disagree', 3, x)
  x <- gsub('Somewhat agree', 4, x)
  x <- gsub('Strongly agree', 5, x)
  return(x)
}

mh_data[SRHI_columns] <- lapply(mh_data[SRHI_columns], recode.rules.SRHI)



##################################################################################
#########################        BISBAS      ####################################
##################################################################################
# Link to website used for scoring info: https://www.psy.miami.edu/faculty/ccarver/bisbas.html

####### BISBAS INFO 

#####      Q1, Q6, Q11, Q17 ARE FILLERS 
#####.     Q2 AND Q22 ARE REVERSE SCORED
#####      BIS SCALE =  Q 2, 8, 13, 16, 19, 22, 24
#####      BAS DRIVE = 3, 9, 12, 21
#####      BAS FUN SEEKING = 5, 10, 15, 20
#####      BAS REWARD RESPONSIVENESS = 4, 7, 14, 18, 23


# identifying and renaming which bisbas questions need to be reversed 

mh_data <- mh_data %>%
  dplyr::rename("BISBAS_2_reverse" = "BISBAS_2") %>%
  dplyr::rename("BISBAS_22_reverse"=  "BISBAS_22")

recode.rules.BISBAS_normal  <- function(x) {
  x <- gsub('Very true for me', 4, x)
  x <- gsub( 'Somewhat true for me', 3, x)
  x <- gsub('Somewhat false for me', 2, x)
  x <- gsub('Very false for me', 1, x)
  return(x)
}

recode.rules.BISBAS_reverse  <- function(x) {
  x <- gsub('Very true for me', 1, x)
  x <- gsub( 'Somewhat true for me', 2, x)
  x <- gsub('Somewhat false for me', 3, x)
  x <- gsub('Very false for me', 4, x)
  return(x)
}

# specifying all bisbas columns
BISBAS_columns <- grep('BISBAS', names(mh_data), value = TRUE)

# specifying bis bas reverse columns 
BISBAS_reverse_columns <- grep('reverse', BISBAS_columns, value = TRUE)

# excluding reverse columns to specify the normal columns
BISBAS_normal_columns <- dplyr::setdiff(BISBAS_columns, BISBAS_reverse_columns)


# now recoding BIS BAS 
mh_data[BISBAS_normal_columns] <- lapply(mh_data[BISBAS_normal_columns], recode.rules.BISBAS_normal)
mh_data[BISBAS_reverse_columns] <- lapply(mh_data[BISBAS_reverse_columns], recode.rules.BISBAS_reverse)


# Naming the column names of the different BIS and BAS scale scores 
BIS_total_columns                 <- c('BISBAS_2_reverse', 'BISBAS_8','BISBAS_13', 'BISBAS_16', 'BISBAS_19','BISBAS_22_reverse', 'BISBAS_24')
BAS_drive_columns                 <- c('BISBAS_3', 'BISBAS_9', 'BISBAS_12', 'BISBAS_21')
BAS_fun_seeking_columns           <- c('BISBAS_5', 'BISBAS_10', 'BISBAS_15', 'BISBAS_20')
BAS_reward_responsiveness_columns <- c('BISBAS_4', 'BISBAS_7', 'BISBAS_14', 'BISBAS_18', 'BISBAS_23')
BAS_total_columns                 <- unique(c(BAS_drive_columns, BAS_fun_seeking_columns, BAS_reward_responsiveness_columns))



##################################################################################
#########################        PHQ-8          ##################################
##################################################################################

# In accordance with the original PHQ8 publication, 
# Kroenke et al 2009 'The PHQ-8 as a measure of current depression in the general population'
# scores are from 0 to 3 for each question, so the final score can range from 0 to 24. 
# Higher scores indicate higher depression.

recode.rules.PHQ  <- function(x) {
  x <- gsub('Not at all', 0, x)
  x <- gsub('Several days', 1, x)
  x <- gsub('More than half the days', 2, x)
  x <- gsub('Nearly every day', 3, x)
  return(x)
}


PHQ_columns <- grep('PHQ', names(mh_data), value = TRUE)
mh_data[PHQ_columns] <- lapply(mh_data[PHQ_columns], recode.rules.PHQ)



##################################################################################
#########################        GAD-7         ####################################
##################################################################################

recode.rules.GAD  <- function(x) {
  x <- gsub('not at all', 0, x)
  x <- gsub('several days', 1, x)
  x <- gsub('more than half the days', 2, x)
  x <- gsub('nearly every day', 3, x)
  return(x)
}

GAD_columns <- grep('GAD', names(mh_data), value = TRUE)
mh_data[GAD_columns] <- lapply(mh_data[GAD_columns], recode.rules.GAD)

##################################################################################
#########################        OCI-R      ####################################
##################################################################################

# Link to website used for scoring info : https://greenspacehealth.com/en-us/obsessive-compulsive-oci-r/#:~:text=Scores%20on%20the%20OCI%2DR,the%20likely%20presence%20of%20OCD.


recode.rules.OCI  <- function(x) {
  x <- gsub('Not at all', 0, x)
  x <- gsub('A little', 1, x)
  x <- gsub('Moderately', 2, x)
  x <- gsub('A lot', 3, x)
  x <- gsub('Extremely', 4, x)
  return(x)
}

OCI_columns <- grep('OCI', names(mh_data), value = TRUE)
mh_data[OCI_columns] <- lapply(mh_data[OCI_columns], recode.rules.OCI)


####################################################################################
############## The ATTC / ATQ SHORT FORM  ##########################################
####################################################################################

# link to website used for info about questions and scoring :
#. https://arc.psych.wisc.edu/self-report/attention-control-scale-attc/
# NOTE: We are using the short form. The only way to establish which questions are
# reverse coded is to cross reference between the short version and the normal version information on this website. 
# From doing this, in our short form version, q 1,2,3,5,6,8 are reverse coded.


#### Labelling the columns that need to be reverse coded

mh_data <- mh_data %>%
  dplyr::rename("ATQ_short_1_reverse" = "ATQ_short_1") %>%
  dplyr::rename("ATQ_short_2_reverse" = "ATQ_short_2") %>%
  dplyr::rename("ATQ_short_3_reverse" = "ATQ_short_3") %>%
  dplyr::rename("ATQ_short_5_reverse" = "ATQ_short_5") %>%
  dplyr::rename("ATQ_short_6_reverse" = "ATQ_short_6") %>%
  dplyr::rename("ATQ_short_8_reverse" = "ATQ_short_8") 

### grouping the ATQ columns into reverse and normal 

ATQ_columns_all  <- grep('ATQ', names(mh_data), value = TRUE)

# specifying ATQ reverse columns 
ATQ_reverse_columns <- grep('reverse', ATQ_columns_all, value = TRUE)


# excluding reverse columns to specify normal
ATQ_normal_columns <- dplyr::setdiff(ATQ_columns_all, ATQ_reverse_columns)


############# Specifying how the ATQ columns should be coded ######################
recode.rules.ATQ_short_normal  <- function(x) {
  x <- gsub('Almost never', 1, x)
  x <- gsub( 'Sometimes', 2, x)
  x <- gsub('Often', 3, x)
  x <- gsub('Always', 4, x)
  return(x)
}


recode.rules.ATQ_short_reverse  <- function(x) {
  x <- gsub('Almost never', 4, x)
  x <- gsub( 'Sometimes', 3, x)
  x <- gsub('Often', 2, x)
  x <- gsub('Always', 1, x)
  return(x)
}



########### Recoding THE ATQ_SHORT columns 
mh_data[ATQ_normal_columns]  <- lapply(mh_data[ATQ_normal_columns], recode.rules.ATQ_short_normal)
mh_data[ATQ_reverse_columns] <- lapply(mh_data[ATQ_reverse_columns], recode.rules.ATQ_short_reverse)


##################################################################################
############     Creating scores for each questionnaire    #######################
##################################################################################

mh_data_scored <- mh_data %>%
  dplyr::mutate(across(everything(), ~na_if(., ""))) %>%
  dplyr::slice(3:n()) %>% ## Getting rid of the top two rows that are info about what the question was 
  dplyr::mutate(across(all_of(PHQ_columns), as.numeric)) %>% # making the numbers numeric
  dplyr::mutate(across(all_of(GAD_columns), as.numeric)) %>%
  dplyr::mutate(across(all_of(OCI_columns), as.numeric)) %>%
  dplyr::mutate(across(all_of(BISBAS_columns), as.numeric))%>%
  dplyr::mutate(across(all_of(ATQ_columns_all), as.numeric))%>%
  dplyr::mutate(across(all_of(SRHI_columns), as.numeric))%>%
  dplyr::mutate(PHQ_8_score = rowSums(select(., all_of(PHQ_columns)), na.rm = TRUE)) %>% # Where the actual recoding starts
  dplyr::mutate(GAD_7_score = rowSums(select(., all_of(GAD_columns)), na.rm = TRUE))%>%
  dplyr::mutate(OCI_R_score = rowSums(select(., all_of(OCI_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BIS_total_score = rowSums(select(., all_of(BIS_total_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_total_score = rowSums(select(., all_of(BAS_total_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_fun_seeking_score = rowSums(select(., all_of(BAS_fun_seeking_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_drive_score = rowSums(select(., all_of(BAS_drive_columns)), na.rm = TRUE))%>%
  dplyr::mutate(BAS_reward_responsiveness_score = rowSums(select(., all_of(BAS_reward_responsiveness_columns)), na.rm = TRUE))%>%
  dplyr::mutate(ATQ_short_score = rowSums(select(., all_of(ATQ_columns_all)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_phone_use_score = rowSums(select(., all_of(SRHI_phone_use_columns)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_social_media_use_score = rowSums(select(., all_of(SRHI_social_media_use_columns)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_most_automatic_app_score = rowSums(select(., all_of(SRHI_most_automatic_app_columns)), na.rm = TRUE))%>%
  dplyr::mutate(SRHI_least_automatic_app_score = rowSums(select(., all_of(SRHI_least_automatic_app_columns)), na.rm = TRUE))



################################################################################
################# MANUALLY ADDING IN Z83 AS THEY ###############################
################# FORGOT TO PUT IN THEIR PPID    ###############################
################################################################################

mh_data_scored$ppid[mh_data_scored$RecordedDate == '2024-07-26 08:13:36'] <- 'Z83'

################################################################################
########### Only keeping the ppids we also have phone data for #################
########### AND RENAMING MH DATA                               #################
################################################################################

### only keeping ppids that we also have phone data for 

processed_qnr_data <- mh_data_scored %>%
  dplyr::filter(ppid %in% processed_app_data$ppid) # processed_app_data is the df created in the script 02-process_app_data.R, which should be run before this


################################################################################
########### Manually adding in the gender of the older z ps    #################
################################################################################

# for these participants there was no gender question in the main questionnaire,
# for later participants we added the gender question at the end
processed_qnr_data <- processed_qnr_data %>%
  mutate(gender = ifelse(ppid == "Z13", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z14", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z15", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z16", "Male",   gender))%>%
  mutate(gender = ifelse(ppid == "Z19", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z21", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z20", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z23", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z25", "Male",   gender))%>%
  mutate(gender = ifelse(ppid == "Z27", "Female", gender))%>%
  mutate(gender = ifelse(ppid == "Z28", "Prefer not to say", gender))


