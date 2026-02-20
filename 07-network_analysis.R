
################################################################################
############# APP-JOURNEY NETWORK ANALYSIS                        ##############
############# Feb 2025.                                           ##############
############# Georgia Turner, georgianjt@gmail.com                ##############
################################################################################

# This loads in the cleaned and preprocessed dataframes and looks at how similarity across
# phone use over time relates to habitual tendency.

rm(list = ls())
seed <- 1
set.seed(seed)

################## Setting up environment  #####################################

library(tidyverse)
library(here)
library(ggplot2)
library(lubridate) # for recoding the date
library(corrplot)  # for plotting cross correlations
library(igraph)
library(entropy)
library(ggdist)
library(BayesFactor)
library(gridExtra)
library(broom)
library(tibble)
library(dplyr)
library(flextable)
library(officer)


here_dir   <- here()
parent_dir <- dirname(here_dir)
setwd(here_dir)
source('00-plotting_functions.R')

save_directory <- stringi::stri_join(dirname(here()), "data_processed", "", sep = "/")


################################################################################
############                load in data            ############################
################################################################################

filepath                   <- str_c(parent_dir, "/data_processed/")
event_level_filename       <- "250909_1851_250908_1519_event_level_df.csv"
app_session_level_filename <- "250909_1852_250908_1523_appsession_level_df.csv"
session_level_filename     <- "250909_1852_250908_1523_session_level_df.csv"
participant_level_filename <- "250909_1852_250908_1520_participant_level_df.csv"

app_session_level_df         <- read_csv(str_c(filepath, app_session_level_filename))
session_level_df             <- read_csv(str_c(filepath, session_level_filename))
participant_level_df         <- read_csv(str_c(filepath, participant_level_filename))
event_level_df               <- read.csv(str_c(filepath, event_level_filename), stringsAsFactors = FALSE)
event_level_df$Date_and_Time <- as.POSIXct(event_level_df$Date_and_Time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC") # to make sure it correctly loads with millisecond resolution

# exclude participant Z83 who has been removed from participant_level_df because they did not complete their questionnaires
event_level_df       <- event_level_df %>% filter(ppid %in% participant_level_df$ppid) 
app_session_level_df <- app_session_level_df %>% filter(ppid %in% participant_level_df$ppid) 
session_level_df     <- session_level_df %>% filter(ppid %in% participant_level_df$ppid) 


################################################################################
############      Define functions to extract graph properties
################################################################################

####### global properties
get_global_props <- function(graph) {

  modularity_louvain              <- modularity(cluster_louvain(as_undirected(graph)))
  edge_density                    <- edge_density(graph)

  data.frame(modularity_louvain,
             edge_density)
}

####### local properties
get_local_props <- function(graph, node_name) {
  strength                        <- strength(graph)[node_name]
  in_strength                     <- strength(graph, mode = "in", weights = E(graph)$weight)[node_name]
  out_strength                    <- strength(graph, mode = "out", weights = E(graph)$weight)[node_name]
  diversity                       <- diversity(as.undirected(graph))[node_name]
  in_diversity                    <- scaled_shannon_diversity(graph, node_name, mode = "in")
  out_diversity                   <- scaled_shannon_diversity(graph, node_name, mode = "out")
  eigen_centrality                <- eigen_centrality(graph)$vector[node_name]
  coreness                        <- coreness(graph)[node_name]
  
  # calculate in diversity and out diversity another way to check
  # note: we use the above way (in_diversity) in the paper; this is just double checking that it doesn't differ using a different method which we don't ultimately use in the paper
  
  incoming_neighbors <- neighbors(graph, node_name, mode = "in")
  # Step 2: Create a subgraph with these neighbors plus the node_name itself
  sub_nodes <- c(node_name, V(graph)[incoming_neighbors]$name)
  # Step 3: Get the induced subgraph (still directed)
  subgraph_directed <- induced_subgraph(graph, vids = sub_nodes)
  # Step 4: Extract edges that go *to* node_name and their weights
  edges_to_node <- E(subgraph_directed)[.to(node_name)]
  weights       <- E(subgraph_directed)[.to(node_name)]$weight
  # Step 5: Build an undirected graph with the same nodes and edges with weights
  # Create an empty undirected graph with the same nodes
  in_mini_graph <- make_empty_graph(n = length(sub_nodes), directed = FALSE)
  in_mini_graph <- set_vertex_attr(in_mini_graph, "name", value = sub_nodes)
  # Add edges undirected with weights equal to the original in_weight
  # Edges are from each neighbor to node_name
  edge_list <- c()
  for (i in seq_along(incoming_neighbors)) {
    edge_list <- c(edge_list, sub_nodes[i + 1], node_name)
  }
  in_mini_graph <- add_edges(in_mini_graph, edge_list)
  # Assign weights to these edges
  E(in_mini_graph)$weight <- weights
  if (ecount(in_mini_graph) > 0) {
    in_diversity_georgia <- diversity(in_mini_graph)[node_name]
  } else {
    in_diversity_georgia <- NA  # or rep(NA, length(node_name)) depending on expected output
  }
  ###
  outcoming_neighbors <- neighbors(graph, node_name, mode = "out")
  # Step 2: Create a subgraph with these neighbors plus the node_name itself
  sub_nodes <- c(node_name, V(graph)[outcoming_neighbors]$name)
  # Step 3: Get the induced subgraph (still directed)
  subgraph_directed <- induced_subgraph(graph, vids = sub_nodes)
  # Step 4: Extract edges that go *to* node_name and their weights
  edges_from_node <- E(subgraph_directed)[.from(node_name)]
  weights <- E(subgraph_directed)[.from(node_name)]$weight
  # Step 5: Build an undirected graph with the same nodes and edges with weights
  # Create an empty undirected graph with the same nodes
  out_mini_graph <- make_empty_graph(n = length(sub_nodes), directed = FALSE)
  out_mini_graph <- set_vertex_attr(out_mini_graph, "name", value = sub_nodes)
  # Add edges undirected with weights equal to the original in_weight
  # Edges are from each neighbor to node_name
  edge_list <- c()
  for (i in seq_along(outcoming_neighbors)) {
    edge_list <- c(edge_list, sub_nodes[i + 1], node_name)
  }
  out_mini_graph <- add_edges(out_mini_graph, edge_list)
  # Assign weights to these edges
  E(out_mini_graph)$weight <- weights
  if (ecount(out_mini_graph) > 0) {
    out_diversity_georgia <- diversity(out_mini_graph)[node_name]
  } else {
    out_diversity_georgia <- 0  # or rep(NA, length(node_name)) depending on expected output
  }
  
  # collecet all variables
  data.frame(strength,
             in_strength,
             out_strength,
             diversity,
             in_diversity,
             out_diversity,
             in_diversity_georgia,
             out_diversity_georgia,
             eigen_centrality,
             coreness)
}

################################################################################
############      Make graphs.
################################################################################


# make list filter to participants who used both their most and least automatic app at least once, for the most and least automatic app analyses
participant_level_df_usedApps <- app_session_level_df %>% 
  right_join(select(participant_level_df, ppid, most_automatic_app_recoded, least_automatic_app_recoded), by = "ppid") %>% 
  group_by(ppid) %>% 
  filter(any(App == most_automatic_app_recoded) & any(App == least_automatic_app_recoded)) %>% 
  ungroup()

# get a vector of participant IDs 
ppids <- unique(participant_level_df$ppid)

# Loop through each participant
for (pp_index in seq_along(ppids)) {
  pp <- ppids[pp_index]  # Get the current participant ID
  
  print(str_c("Processing participant number ", pp_index, " out of ", length(ppids)))  # Print progress
  
  ################################################################################
  # get the data for this participant
  ################################################################################
  
  # Initialize the empty dataframe to record how many sessions each app occurred in, which will be normalising factor for graph weights
  n_sessions_with_apps <- tibble(
    appName = character(),
    n_sessions_with_app = numeric()
  )  
  
  # Filter the app-session-level data for the current participant
  pp_df <- filter(app_session_level_df, ppid == pp)
  
  ################################################################################
  # make the graph
  ################################################################################
  
  # Create a session x app matrix where we count the occurrences of apps per session
  pp_df_count <- pp_df %>%
    count(session_id, App) %>%
    spread(App, n, fill = 0)
  
  app_names <- colnames(pp_df_count)[-1]  # Excluding session_id column

  # store info about how many unique apps in that participant's graph
  participant_level_df$nApps[pp_index] <- length(app_names)
  
  # Calculate co-occurrence for this participant
  for (i in 1:length(app_names)) {
    appA               <- app_names[i]
    other_apps         <- setdiff(app_names, appA)

    # get normalising factors
    sessions_with_appA   <- filter(pp_df_count, get(appA) != 0)
    n_sessions_with_appA <- nrow(sessions_with_appA)
    n_sessions_with_apps <- bind_rows( # fill in to get the normalising factor we will then use for the other graphs too
      n_sessions_with_apps,
      tibble(appName = appA, n_sessions_with_app = n_sessions_with_appA)
    )
  }
  
  ########## make into graphs

  ################################################################################
  # 'orderA' : directed graph 
  # connection from A to B represents conditional likelihood that if A is 
  # used in a session, B is used at least once after A in that session.

  ordered_app_pairs <- pp_df %>%
    group_by(session_id) %>%
    arrange(app_session_id, .by_group = TRUE) %>%
    filter(n() > 1) %>%
    summarise(
      app_combinations = list(combn(App, 2, simplify = FALSE)),
      .groups = 'drop'
    ) %>%
    mutate(
      unique_combinations = map(app_combinations, function(pairs) {
        pairs %>%
          map_chr(~ paste(.x, collapse = "-")) %>%  # Convert each pair to a string
          unique() %>%                              # Remove duplicate strings
          map(~ strsplit(.x, "-")[[1]])             # Convert back to list format
      })
    ) %>%
    select(-app_combinations) %>%
    unnest(unique_combinations) %>%                # Unnest to get each pair as a row
    transmute(
      appA = map_chr(unique_combinations, ~ .x[1]),
      appB = map_chr(unique_combinations, ~ .x[2])
    ) %>%
    count(appA, appB) %>% 
    left_join(n_sessions_with_apps, by = c("appA" = "appName")) %>%  # Join on appA and appName
    rename(n_sessions_with_appA = n_sessions_with_app) %>%
    left_join(n_sessions_with_apps, by = c("appB" = "appName")) %>%  # Join on appB and appName
    rename(n_sessions_with_appB = n_sessions_with_app) %>%
    mutate(normalisedA = n / n_sessions_with_appA,
           normalisedB = n / n_sessions_with_appB) 
  

  # Get unique app names in sorted order
  ordered_app_names <- sort(unique(c(ordered_app_pairs$appA, ordered_app_pairs$appB)))
  
  # Expand app pairs to include all possible combinations
  ordered_appA_matrix <- expand.grid(appA = ordered_app_names, appB = ordered_app_names) %>%
    left_join(ordered_app_pairs, by = c("appA", "appB")) %>% 
    replace_na(list(normalisedA = 0)) %>% 
    group_by(appA, appB) %>% 
    summarise(normalisedA = sum(normalisedA), .groups = "drop") %>%  # Ensure unique appA, appB combinations with sum of 'n'
    pivot_wider(names_from = appB, values_from = normalisedA, values_fill = list(normalisedA = 0)) %>%
    as.data.frame() %>%  # Convert to a data.frame to ensure row names work
    column_to_rownames("appA")  # Set appA as row names
  
  ## add back any Apps which were not in the matrix, which are Apps that only ever appeared in one session without any other Apps and are therefore 
  # not 'connected' to any other Apps
  isolated_apps <- ""
  if (length(app_names) != length(ordered_app_names)) {
    isolated_apps <- setdiff(app_names, ordered_app_names)
    ordered_appA_matrix[isolated_apps, ] <- 0
    ordered_appA_matrix[, isolated_apps] <- 0
  }
  
  ########## make into graphs
  # A
  # Convert co-occurrence matrix to an edge list with weights
  orderedA_edges   <- which(ordered_appA_matrix > 0, arr.ind = TRUE)  # Get indices where co-occurrence > 0
  orderedA_weights <- ordered_appA_matrix[orderedA_edges]  # Get the corresponding weights (co-occurrence counts)
  # Create a directed graph from the edge list
  orderA_graph           <- graph_from_edgelist(as.matrix(orderedA_edges), directed = TRUE)
  # Assign edge weights
  E(orderA_graph)$weight <- orderedA_weights
  # Add node names to the graph
  V(orderA_graph)$name   <- ordered_app_names
  orderA_graph <- add_vertices(orderA_graph, 
                               nv   = length(isolated_apps), 
                               name = isolated_apps)
  

  ################################################################################
  # extract properties for the graphs 
  ################################################################################

  ####### global properties
  global_properties_orderA        <- get_global_props(orderA_graph)

  ####### local properties
  # Calculate most and least automatic apps for the current participant
  most_auto_app         <- filter(participant_level_df, ppid == pp)$most_automatic_app_recoded
  least_auto_app        <- filter(participant_level_df, ppid == pp)$least_automatic_app_recoded
  
  # if participant used their most and least auto app both at least once
  if (pp %in% unique(participant_level_df_usedApps$ppid)) {

    mostAutoNode_properties_orderA     <- get_local_props(orderA_graph, most_auto_app)
    leastAutoNode_properties_orderA    <- get_local_props(orderA_graph, least_auto_app)

  ################################################################################
  # save properties for the graphs 
  ################################################################################
    
    ####### local properties
    colnames_mostAutoNode_properties_orderA         <- paste0(colnames(mostAutoNode_properties_orderA), "_MostorderA")
    colnames_leastAutoNode_properties_orderA        <- paste0(colnames(leastAutoNode_properties_orderA), "_LeastorderA")

    # local properties
    participant_level_df[participant_level_df$ppid == pp, colnames_mostAutoNode_properties_orderA]  <- mostAutoNode_properties_orderA
    participant_level_df[participant_level_df$ppid == pp, colnames_leastAutoNode_properties_orderA] <- leastAutoNode_properties_orderA
    

  } 

  ####### global properties
  #initialise columns if necessary
  if (pp_index == 1) {
    colnames_global_properties_orderA         <- paste0(colnames(global_properties_orderA), "_orderA")
  }
  
  participant_level_df[participant_level_df$ppid == pp, colnames_global_properties_orderA]        <- global_properties_orderA

}

################################################################################
############       Plot results
################################################################################

## Define different dfs to test - full df and outliers
# to do so, we first get lists of ppids to exclude for different cases - either outliers or failed attention checks in questionnaires

# Get outliers for each qnr

outliers_list <- list(
  modularity_louvain_orderA = find_outliers(participant_level_df, "modularity_louvain_orderA")$outlier_ids,
  edge_density_orderA       = find_outliers(participant_level_df, "edge_density_orderA")$outlier_ids,
  PHQ_8_score               = find_outliers(participant_level_df, "PHQ_8_score")$outlier_ids,
  GAD_7_score               = find_outliers(participant_level_df, "GAD_7_score")$outlier_ids,
  OCI_R_score               = find_outliers(participant_level_df, "OCI_R_score")$outlier_ids,
  ATQ_short_score           = find_outliers(participant_level_df, "ATQ_short_score")$outlier_ids,
  SRHI_phone_use_score      = find_outliers(participant_level_df, "SRHI_phone_use_score")$outlier_ids,
  SRHI_social_media_use_score    = find_outliers(participant_level_df, "SRHI_social_media_use_score")$outlier_ids,
  SRHI_most_automatic_app_score  = find_outliers(participant_level_df, "SRHI_most_automatic_app_score")$outlier_ids,
  SRHI_least_automatic_app_score = find_outliers(participant_level_df, "SRHI_least_automatic_app_score")$outlier_ids,
  BIS_total_score           = find_outliers(participant_level_df, "BIS_total_score")$outlier_ids,
  BAS_total_score           = find_outliers(participant_level_df, "BAS_total_score")$outlier_ids,
  BAS_fun_seeking_score     = find_outliers(participant_level_df, "BAS_fun_seeking_score")$outlier_ids,
  BAS_drive_score           = find_outliers(participant_level_df, "BAS_drive_score")$outlier_ids,
  BAS_reward_responsiveness_score = find_outliers(participant_level_df, "BAS_reward_responsiveness_score")$outlier_ids
)

ppid_failed_attention <- c("Z73", "Z109", "M7" ,  "Z54" , "Z115" ,"Z141", "Z137","Z55") # participants who failed at least one attention check


## RQ1: GLOBAL NETWORK PROPERTIES

global_features <- list(
  "modularity_louvain_orderA",
  "edge_density_orderA"
)
qnr_outcome_vars <- list(
  PHQ       = "PHQ_8_score",
  GAD       = "GAD_7_score",
  OCD       = "OCI_R_score",
  ATC       = "ATQ_short_score",
  SRHIphone = "SRHI_phone_use_score",
  SRHIsm    = "SRHI_social_media_use_score"
)
variations <- list(
  "Raw",
  "NoOutliers",
  "NoAttFail",
  "TimeCtrl",
  "nAppsCtrl"
)

## Table 1: Relating modularity and density to questionnaire scores
#####################

all_results <- list()

for (global_feature in global_features) {
  
  for (qnr_outcome_var in qnr_outcome_vars) {
    
    ############## calculate relevant stats
    
    # extract variable names 
    global_network_feature <- global_feature[[1]]
    qnr_score              <- qnr_outcome_var[[1]]
    
    # Print progress
    print(str_c("Processing ", global_network_feature, " and ", qnr_score))
    
    # formula without control
    formula_NoCtrl <- as.formula(paste(qnr_outcome_var, "~", global_feature))
    
    # Raw 
    lm_mod_raw  <- lm(formula_NoCtrl, data = participant_level_df)
    bf_mod_raw  <- lmBF(formula_NoCtrl, data = participant_level_df)
    
    # NoOutlier
    lm_mod_NoOutlier  <- lm(formula_NoCtrl, 
                            data = filter(participant_level_df, !(ppid %in% outliers_list[[global_network_feature]])
                                          & !(ppid %in% outliers_list[[qnr_score]]) )
                            )
    bf_mod_NoOutlier <- lmBF(formula_NoCtrl, 
                             data = filter(participant_level_df, !(ppid %in% outliers_list[[global_network_feature]])
                                           & !(ppid %in% outliers_list[[qnr_score]]) )
    )
    
    # No Attention Fail
    lm_mod_NoAttFail <- lm(formula_NoCtrl, 
                            data = filter(participant_level_df, !(ppid %in% ppid_failed_attention) )
    )
    bf_mod_NoAttFail   <- lmBF(formula_NoCtrl, 
                             data = filter(participant_level_df, !(ppid %in% ppid_failed_attention) )
    )
    
    # TimeCtrl
    # note that some pps are excluded from time spent per days, leaving NA, as they either paused every day or paused every day that wasnt the first and last days
    # lmBF doesnt deal with these missing cases so create a df without them first
    participant_level_df_complete <- participant_level_df[complete.cases(participant_level_df[, "mean_timespent_per_day_hrs"]), ]
    
    formula_TimeCtrl <-  as.formula(paste(qnr_outcome_var, "~", global_feature, 
                                          "+", "mean_timespent_per_day_hrs"))
    formula_JustTime <- as.formula(paste(qnr_outcome_var, "~", "mean_timespent_per_day_hrs"))
    
    lm_mod_TimeCtrl <- lm(formula_TimeCtrl, data = participant_level_df)
    # the BF is comparing the model with controls to model just with the controls, as normal BF otherwise just compares all to null
    bf_mod_TimeCtrl <- lmBF(formula_TimeCtrl, data = participant_level_df_complete) / 
      lmBF(formula_JustTime, data = participant_level_df_complete) 
    
    # nAppsCtrl
    formula_nAppsCtrl <-  as.formula(paste(qnr_outcome_var, "~", global_feature, 
                                          "+", "nApps"))
    formula_JustnApps <- as.formula(paste(qnr_outcome_var, "~", "nApps"))
    
    lm_mod_nAppsCtrl <- lm(formula_nAppsCtrl, data = participant_level_df)
    bf_mod_nAppsCtrl <- lmBF(formula_nAppsCtrl, data = participant_level_df) /
      lmBF(formula_JustnApps, data = participant_level_df) 
    
    ############## make into row
    
    make_model_row <- function(lm_mod, bf_mod, model_type) {
      tibble(
        Outcome   = qnr_score,
        Predictor = global_network_feature,
        ModelType = model_type,
        Beta      = summary(lm_mod)$coefficients[global_network_feature, "Estimate"],
        StdError  = summary(lm_mod)$coefficients[global_network_feature, "Std. Error"],
        t_value   = summary(lm_mod)$coefficients[global_network_feature, "t value"],
        p_value   = summary(lm_mod)$coefficients[global_network_feature, "Pr(>|t|)"],
        R2        = summary(lm_mod)$r.squared,
        BF        = as.vector(bf_mod)
      )
    }
    
    row_Raw       <- make_model_row(lm_mod_raw, bf_mod_raw, "Raw")
    row_NoOutlier <- make_model_row(lm_mod_NoOutlier, bf_mod_NoOutlier, "NoOutlier")
    row_NoAttFail <- make_model_row(lm_mod_NoAttFail, bf_mod_NoAttFail, "NoAttFail")
    row_TimeCtrl  <- make_model_row(lm_mod_TimeCtrl, bf_mod_TimeCtrl, "TimeControlled")
    row_nAppsCtrl <- make_model_row(lm_mod_nAppsCtrl, bf_mod_nAppsCtrl, "nAppsControlled")
    
    all_results <- bind_rows(
      all_results,
      row_Raw, 
      row_NoOutlier, 
      row_NoAttFail,
      row_TimeCtrl,
      row_nAppsCtrl
    )

  }
  
}

# repeat for
  
########### Format into table

add_sig_stars <- function(p) {
  if (is.na(p)) {
    return("")
  } else if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("")
  }
}

add_BF_interpretation <- function(BF) {
  if (is.na(BF)) {
    return(NA)
  } else if (BF < 1/100) {
    return("Extreme: H0 > H1")
  } else if (BF >= 1/100 && BF < 1/30) {
    return("Very strong: H0 > H1")
  } else if (BF >= 1/30 && BF < 1/10) {
    return("Strong: H0 > H1")
  } else if (BF >= 1/10 && BF < 1/3) {
    return("Substantial: H0 > H1")
  } else if (BF >= 1/3 && BF < 1) {
    return("Anecdotal: H0 > H1")
  } else if (BF == 1) {
    return("No evidence")
  } else if (BF > 1 && BF < 3) {
    return("Anecdotal: H1 > H0")
  } else if (BF >= 3 && BF < 10) {
    return("Substantial: H1 > H0")
  } else if (BF >= 10 && BF < 30) {
    return("Strong: H1 > H0")
  } else if (BF >= 30 && BF < 100) {
    return("Very strong: H1 > H0")
  } else if (BF >= 100) {
    return("Extreme: H1 > H0")
  }
}

format_table <- function(results_df) {
  # Select columns
  df <- results_df %>%
    select(
      Outcome,
      ModelType,
      Beta,
      StdError,
      p_value,
      R2,
      BF
    )
  
  # Round numeric columns to 3 decimals
  df <- df %>%
    mutate(
      Beta     = round(Beta, 3),
      StdError = round(StdError, 3),
      p_value  = round(p_value, 3),
      R2       = round(R2, 3),
      BF       = round(BF, 3)
    )
  
  # Add significance stars column after p_value
  df <- df %>%
    mutate(sig = sapply(p_value, add_sig_stars),
           BF_interpretation = sapply(BF, add_BF_interpretation)
           ) %>%
    select(Outcome, ModelType, Beta, StdError, R2, p_value, sig, BF, BF_interpretation)

  return(df)
}

formatted_table <- format_table(all_results)
View(formatted_table)
# Save CSV

write.csv(formatted_table, str_c(parent_dir,"/output/Table1.csv"), row.names = FALSE)
export_results_to_word(formatted_table, str_c(parent_dir,"/output/Table1.docx"))

## Figure 2: Relating modularity and density to questionnaire scores
#####################

variables_mainfigs <- c(
  "PHQ_8_score", "GAD_7_score", "OCI_R_score", 
  "ATQ_short_score", "SRHI_phone_use_score", "SRHI_social_media_use_score"
)

color_palette_Fig2 <- c("pink", "pink", "pink", "#7e57c2", "#1f77b4", "#1f77b4")

Fig2a <- plot_measure_facet1row(participant_level_df, "modularity_louvain_orderA", variables_mainfigs, color_palette_Fig2)
Fig2b <- plot_measure_facet1row(participant_level_df, "edge_density_orderA", variables_mainfigs, color_palette_Fig2)
Fig2_layout_matrix <- rbind(
  c(1),
  c(NA),
  c(2))
# Arrange plots with added space
Fig2 <- grid.arrange(
  Fig2a, Fig2b, 
  layout_matrix = Fig2_layout_matrix,
  heights = c(1, 0.1, 1)
  
)

ggsave(str_c(parent_dir,"/output/figures/Fig2.png"), plot = Fig2, width = 17, height = 8, units = "in")

## RQ2: LOCAL NETWORK PROPERTIES

## Figure 5: Relating local properties to self-reported Most vs. Least Auto Apps
#####################

Fig5a <- plot_raincloud2vars(participant_level_df, "in_strength_MostorderA", "in_strength_LeastorderA")
Fig5b <- plot_raincloud2vars(participant_level_df, "out_strength_MostorderA", "out_strength_LeastorderA")
Fig5c <- plot_raincloud2vars(participant_level_df, "in_diversity_MostorderA", "in_diversity_LeastorderA")
Fig5d <- plot_raincloud2vars(participant_level_df, "out_diversity_MostorderA", "out_diversity_LeastorderA")
Fig5e <- plot_raincloud2vars(participant_level_df, "eigen_centrality_MostorderA", "eigen_centrality_LeastorderA")
Fig5f <- plot_raincloud2vars(participant_level_df, "coreness_MostorderA", "coreness_LeastorderA")

Fig5_layout_matrix <- rbind(
  c(1, 2),
  c(3, 4), 
  c(5, 6)
)
# Arrange plots with added space
Fig5 <- grid.arrange(
  Fig5a, Fig5b, Fig5c, Fig5d, Fig5e, Fig5f, 
  layout_matrix = Fig5_layout_matrix
)
ggsave(str_c(parent_dir,"/output/figures/Fig5.png"), plot = Fig5, width = 5, height = 8, units = "in")

## stats
#####################
participant_level_df_NoAttFail <- filter(participant_level_df, !(ppid %in% ppid_failed_attention))

participant_level_df_NoAttFail <- filter(participant_level_df, !(ppid %in% ppid_failed_attention))
#### For each, do frequentist and Bayes with and without attention fail participants excluded. First do it all for without exclusions, then with.

### strength

t.test(participant_level_df$in_strength_MostorderA,
       participant_level_df$in_strength_LeastorderA,
       paired = TRUE)
in_strength_df <- na.omit(data.frame(
  x = participant_level_df$in_strength_MostorderA,
  y = participant_level_df$in_strength_LeastorderA
))
bf_in_strength <- ttestBF(x = in_strength_df$x, y = in_strength_df$y, paired = TRUE)
print(bf_in_strength)

t.test(participant_level_df$out_strength_MostorderA,
       participant_level_df$out_strength_LeastorderA,
       paired = TRUE)
out_strength_df <- na.omit(data.frame(
  x = participant_level_df$out_strength_MostorderA,
  y = participant_level_df$out_strength_LeastorderA
))
bf_out_strength <- ttestBF(x = out_strength_df$x, y = out_strength_df$y, paired = TRUE)
print(bf_out_strength)

### no att fail
t.test(participant_level_df_NoAttFail$in_strength_MostorderA,
       participant_level_df_NoAttFail$in_strength_LeastorderA,
       paired = TRUE)
in_strength_df <- na.omit(data.frame(
  x = participant_level_df_NoAttFail$in_strength_MostorderA,
  y = participant_level_df_NoAttFail$in_strength_LeastorderA
))
bf_in_strength <- ttestBF(x = in_strength_df$x, y = in_strength_df$y, paired = TRUE)
print(bf_in_strength)

t.test(participant_level_df_NoAttFail$out_strength_MostorderA,
       participant_level_df_NoAttFail$out_strength_LeastorderA,
       paired = TRUE)
out_strength_df <- na.omit(data.frame(
  x = participant_level_df_NoAttFail$out_strength_MostorderA,
  y = participant_level_df_NoAttFail$out_strength_LeastorderA
))
bf_out_strength <- ttestBF(x = out_strength_df$x, y = out_strength_df$y, paired = TRUE)
print(bf_out_strength)

### diversity

t.test(participant_level_df$in_diversity_MostorderA,
       participant_level_df$in_diversity_LeastorderA,
       paired = TRUE)
in_diversity_df <- na.omit(data.frame(
  x = participant_level_df$in_diversity_MostorderA,
  y = participant_level_df$in_diversity_LeastorderA
))
bf_in_diversity <- ttestBF(x = in_diversity_df$x, y = in_diversity_df$y, paired = TRUE)
print(bf_in_diversity)

t.test(participant_level_df$out_diversity_MostorderA,
       participant_level_df$out_diversity_LeastorderA,
       paired = TRUE)
out_diversity_df <- na.omit(data.frame(
  x = participant_level_df$out_diversity_MostorderA,
  y = participant_level_df$out_diversity_LeastorderA
))
bf_out_diversity <- ttestBF(x = out_diversity_df$x, y = out_diversity_df$y, paired = TRUE)
print(bf_out_diversity)

### no att fail
t.test(participant_level_df_NoAttFail$in_diversity_MostorderA,
       participant_level_df_NoAttFail$in_diversity_LeastorderA,
       paired = TRUE)
in_diversity_df <- na.omit(data.frame(
  x = participant_level_df_NoAttFail$in_diversity_MostorderA,
  y = participant_level_df_NoAttFail$in_diversity_LeastorderA
))
bf_in_diversity <- ttestBF(x = in_diversity_df$x, y = in_diversity_df$y, paired = TRUE)
print(bf_in_diversity)

t.test(participant_level_df_NoAttFail$out_diversity_MostorderA,
       participant_level_df_NoAttFail$out_diversity_LeastorderA,
       paired = TRUE)
out_diversity_df <- na.omit(data.frame(
  x = participant_level_df_NoAttFail$out_diversity_MostorderA,
  y = participant_level_df_NoAttFail$out_diversity_LeastorderA
))
bf_out_diversity <- ttestBF(x = out_diversity_df$x, y = out_diversity_df$y, paired = TRUE)
print(bf_out_diversity)


### eigen centrality

t.test(participant_level_df$eigen_centrality_MostorderA,
       participant_level_df$eigen_centrality_LeastorderA,
       paired = TRUE)
eigen_df <- na.omit(data.frame(
  x = participant_level_df$eigen_centrality_MostorderA,
  y = participant_level_df$eigen_centrality_LeastorderA
))
bf_eigen_centrality <- ttestBF(x = eigen_df$x, y = eigen_df$y, paired = TRUE)
print(bf_eigen_centrality)

t.test(participant_level_df$coreness_MostorderA,
       participant_level_df$coreness_LeastorderA,
       paired = TRUE)
coreness_df <- na.omit(data.frame(
  x = participant_level_df$coreness_MostorderA,
  y = participant_level_df$coreness_LeastorderA
))
bf_coreness <- ttestBF(x = coreness_df$x, y = coreness_df$y, paired = TRUE)
print(bf_coreness)

### no att fail
t.test(participant_level_df_NoAttFail$eigen_centrality_MostorderA,
       participant_level_df_NoAttFail$eigen_centrality_LeastorderA,
       paired = TRUE)
eigen_df <- na.omit(data.frame(
  x = participant_level_df_NoAttFail$eigen_centrality_MostorderA,
  y = participant_level_df_NoAttFail$eigen_centrality_LeastorderA
))
bf_eigen_centrality <- ttestBF(x = eigen_df$x, y = eigen_df$y, paired = TRUE)
print(bf_eigen_centrality)

t.test(participant_level_df_NoAttFail$coreness_MostorderA,
       participant_level_df_NoAttFail$coreness_LeastorderA,
       paired = TRUE)
coreness_df <- na.omit(data.frame(
  x = participant_level_df_NoAttFail$coreness_MostorderA,
  y = participant_level_df_NoAttFail$coreness_LeastorderA
))
bf_coreness <- ttestBF(x = coreness_df$x, y = coreness_df$y, paired = TRUE)
print(bf_coreness)

## Figure S1: Relating modularity and density to BIS/BAS questionnaire scores
#####################

variables_BISBAS <- c("BIS_total_score",
                      "BAS_total_score", "BAS_fun_seeking_score", "BAS_drive_score",
                      "BAS_reward_responsiveness_score")

color_palette_FigS1 <- c("darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen" )

FigS1a <- plot_measure_facet1row(participant_level_df, "modularity_louvain_orderA", variables_BISBAS, color_palette_FigS1)
FigS1b <- plot_measure_facet1row(participant_level_df, "edge_density_orderA", variables_BISBAS, color_palette_FigS1)

FigS1_layout_matrix <- rbind(
  c(1),
  c(NA),
  c(2))

# Arrange plots with added space
FigS1 <- grid.arrange(
  FigS1a, FigS1b, 
  layout_matrix = FigS1_layout_matrix,
  heights = c(1, 0.2, 1)
)

ggsave(str_c(parent_dir, "/output/figures/FigS01.png"), plot = FigS1, width = 17, height = 8, units = "in")

## stats
#####################

# BIS BAS and modularity
lm_mod  <- lm(BIS_total_score ~ modularity_louvain_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BIS_total_score ~ modularity_louvain_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_total_score ~ modularity_louvain_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_total_score ~ modularity_louvain_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_fun_seeking_score ~ modularity_louvain_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_fun_seeking_score ~ modularity_louvain_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_drive_score ~ modularity_louvain_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_drive_score ~ modularity_louvain_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_reward_responsiveness_score ~ modularity_louvain_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_reward_responsiveness_score ~ modularity_louvain_orderA, data = participant_level_df)

# BIS BAS and density
lm_mod  <- lm(BIS_total_score ~ edge_density_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BIS_total_score ~ edge_density_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_total_score ~ edge_density_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_total_score ~ edge_density_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_fun_seeking_score ~ edge_density_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_fun_seeking_score ~ edge_density_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_drive_score ~ edge_density_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_drive_score ~ edge_density_orderA, data = participant_level_df)

lm_mod  <- lm(BAS_reward_responsiveness_score ~ edge_density_orderA, data = participant_level_df)
summary(lm_mod)
lmBF(BAS_reward_responsiveness_score ~ edge_density_orderA, data = participant_level_df)

