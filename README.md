# ITACO AND QUESTIONNAIRE PROCESSING SCRIPTS

To make these scripts work, your directory needs to be set up as follows:

```{r}
PARENT_FOLDER
├── CICESE- Main - "".csv. # put the most up to date version of post data collection survey
├── itaco_scripts 
      ├── README.md
      ├── 01-main_processing_script.R
      ├── 02-process_app_data.R
      ├── 03-process_qnr_data.R
      ├── 04-make_summary_dfs.R
      └── itaco_scripts.Rproj
└── usable_participant_data #containing the folders for each individual participants iTACO data 
└── data_processed #where the final processed dataframes are saved after preprocessing. 

```

## 01-main_processing_script.R

This script calls the other scripts. If you run it it should load in and process all the iTACO data, then the mental health questionnaire data, and then create and save three different data frames all reflecting the data on different levels (e.g., the app-session level, the session level, and then the participant level). These data frames can all be added to depending on specific research questions you are looking at.

## 02-process_app_data.R

This script first loops through every participant folder in the usable_participant_data folder, and loads in every txt.file. It then gets rid of non user initiated events and processes all the rows into columns, and loads this processed data into the data frame called "**combined_data_frame**". This data frame is the most unprocessed version of the data (i.e., only contains the raw variables from the txt files: PPID, event, node info, app, date and time)

It then creates a more processed data frame of all the participant data, called **processed_qnr_data**. Details about this can be found in the data dictionary.

## 03-process_qnr_data.R

This script loads in the post data questionnaire data. It then:

-   relabels the columns to reflect what question/ questionnaire is actually being asked.

-   Recodes the answers to score all the clinical questionnaires numerically

-   Creates overall scores for each of the questionnaires

-   adds in missing information to the data frame (i.e., for people who forgot to enter their id)

-   removes incomplete rows / participants we don't have iTACO app data for.

This script retains data frames from various different stages of this process in case things need to be altered, but the final, 'complete' version of the mental health data is called **processed_mh_data**

## 04-make_summary_dfs.R

This script creates three additional data frames that reflect the different levels you might wish to look at the data at. More information about each of these data frames can be found in the data dictionary.
The dataframes are 
 - app_session_level_df
 - session_level_df
 - participant_level_df
 
It then saves these in a folder 'data_processed'.
It saves them with the date and time so that it doesn't overwrite any previous ones.
