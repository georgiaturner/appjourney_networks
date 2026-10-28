
################################################################################
############# APP-JOURNEY NETWORK ANALYSIS                        ##############
############# Feb 2025.                                           ##############
############# Georgia Turner, georgianjt@gmail.com                ##############
################################################################################

# This loads in the cleaned and preprocessed dataframes, generates App-journey
# networks, and then performs statistical tests on these networks as well as outputting
# figures for the paper. 
# This code loads and processes two datasets - Cambridge/Mexico and ABCD EARS. 
# But it should be straightforward to adapt for other datasets.
# For the paper, it outputs figures 3, 4 and S1.

rm(list = ls())
seed <- 1
set.seed(seed)

################## Setting up environment  #####################################

library(tidyverse)
library(here)
library(ineq)
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
library(effectsize)


here_dir   <- here()
parent_dir <- dirname(here_dir)
#setwd(here_dir)
source('00-plotting_functions.R')

save_directory <- stringi::stri_join(dirname(here()), "data_processed", "", sep = "/")

################################################################################
############                load in data            ############################
################################################################################

filepath                   <- str_c(parent_dir, "/data_processed/")

# This contains separate data loading code for each of the different datasets:
# 1) The 'Cambridge/Mexico (CM) data': n=82 dataset collected by study authors, ages 18-22
# 2) The 'ABCD' data: n=565 data from the ABCD cohort EARS study, more info here https://www.nature.com/articles/s41598-024-68467-8

## Cambridge/Mexico (CM) data

CM_app_session_level_filename <- "250909_1852_250908_1523_appsession_level_df.csv"
CM_participant_level_filename <- "250909_1852_250908_1520_participant_level_df.csv"
CM_app_session_level_df       <- read_csv(str_c(filepath, CM_app_session_level_filename))
CM_participant_level_df       <- read_csv(str_c(filepath, CM_participant_level_filename))

# exclude participant Z83 who has been removed from participant_level_df because they did not complete their questionnaires
CM_app_session_level_df <- CM_app_session_level_df %>% filter(ppid %in% CM_participant_level_df$ppid) 


## ABCD data

ABCD_filename                 <- "261005_1216_abcd_appusage2_sevendays_qnr.csv"
ABCD_app_session_level_df     <- read_csv(str_c(filepath, ABCD_filename)) %>%
  rename(session_id       = phone_session_id, # previous name was phone_session_id, here we change to session_id
         ppid             = participant_id,
         App              = app) %>%
  mutate(app_session_id = row_number())

ABCD_mean_screen_time <- ABCD_app_session_level_df |>
  dplyr::summarize(
    sec_foreground = sum(sec_foreground, na.rm = TRUE),
    .by = c(ppid, dt)
  ) |>
  group_by(ppid) |>
  dplyr::summarize(
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
  

# Get categories for each App - this was separately loaded and constructed using the GooglePlay API in '05-further_cleaning.R',
# so that it has a name for every App category in the dataset (although only the CM dataset)
GooglePlay_CategoryMap_df    <- read_csv(str_c(filepath, '260514_1452_GooglePlayAppCategory_Map.csv'))


################################################################################
###### Define functions to extract graph properties and run network analysis
################################################################################

####### global properties
get_global_props <- function(graph) {

  ################ modularity
  modularity_louvain              <- modularity(cluster_louvain(as_undirected(graph)))
  
  ################ edge density 
  edge_density                    <- edge_density(graph)
  
  ################ average path length
  graph_pathlength                <- graph
  all_pairs                       <- t(combn(V(graph_pathlength)$name, 2))
  existing                        <- as.data.frame(get.edgelist(graph_pathlength))
  missing                         <- all_pairs[!paste(all_pairs[,1], all_pairs[,2]) %in%
                         paste(existing[,1], existing[,2]), ]
  graph_pathlength <- add_edges(
    graph_pathlength,
    t(missing),
    attr = list(weight = 0)
  )
  # create distances which are in opposite direction to weight, so higher weight (higher transition prob) = shorter distance
  E(graph_pathlength)$dist        <- 1 - (E(graph_pathlength)$weight) # 
  mean_pathlength                 <- mean_distance(graph_pathlength, directed = TRUE, weights = E(graph_pathlength)$dist)
  
  ################ global efficiency
  ## discount weights and just include if there's a distance or not, as in the classical global efficiency metric 
  global_efficiency               <- global_efficiency(graph, weight = NA)
  
  ################ eigenvector centrality distribution metrics: Gini and centralisation
  eigen_vector                <- eigen_centrality(graph)$vector
  eigen_gini                  <- Gini(eigen_vector)
  eigen_centr                 <- centr_eigen(graph, directed = TRUE, normalized = TRUE)$centralization
  
  ################ clustering
  global_clust                <- transitivity(graph, type="global", weights=E(graph)$weight)
   
  data.frame(
    modularity_louvain,
    edge_density,
    mean_pathlength,
    global_efficiency,
    eigen_gini,
    eigen_centr,
    global_clust
  )
}

####### local properties
get_local_props <- function(graph, node_name) {
  strength                        <- strength(graph)[node_name]
  in_strength                     <- strength(graph, mode = "in", weights = E(graph)$weight)[node_name]
  out_strength                    <- strength(graph, mode = "out", weights = E(graph)$weight)[node_name]
  diversity                       <- diversity(as_undirected(graph))[node_name]
  in_diversity                    <- scaled_shannon_diversity(graph, node_name, mode = "in")
  out_diversity                   <- scaled_shannon_diversity(graph, node_name, mode = "out")
  eigen_centrality                <- eigen_centrality(graph)$vector[node_name]
  coreness                        <- coreness(graph)[node_name]
  local_clust                     <- transitivity(as.undirected(graph), type = "weighted", weights =  E(as.undirected(graph))$weight)[node_name]
  betweenness                     <- betweenness(graph, directed = TRUE, weights = E(graph)$weight)[node_name]
    
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
  
  # collect all variables
  data.frame(strength,
             in_strength,
             out_strength,
             diversity,
             in_diversity,
             out_diversity,
             in_diversity_georgia,
             out_diversity_georgia,
             eigen_centrality,
             coreness,
             local_clust,
             betweenness
             )
}

# Name local property columns to later add to dataframe

local_prop_cols <- cols <- c(
  "strength",
  "in_strength",
  "out_strength",
  "diversity",
  "in_diversity",
  "out_diversity",
  "in_diversity_georgia",
  "out_diversity_georgia",
  "eigen_centrality",
  "coreness",
  "local_clust",
  "betweenness"
)


## Wrapper function to average the local properties for each category
get_category_props <- function(graph,
                               category_colNames,
                               category_name) {
  
  # Get apps in category
  apps_in_category <- GooglePlay_CategoryMap_df %>%
    filter(GooglePlay_category == category_name) %>%
    pull(App)
  
  # Match to nodes in graph
  nodes_in_category <- V(graph)$name[V(graph)$name %in% apps_in_category]
  
  # end function if no apps in that category
  if (length(nodes_in_category) == 0) {
    return(
      as.data.frame(setNames(as.list(rep(NA, length(cols) + 1)), c(cols, "n_nodes"))) %>%
        rename_with(
          .fn = function(x) paste0(x, "_", category_name)
        )
    )
  }
  
  # Compute local properties for each node
  props_df <- lapply(nodes_in_category, function(node) {
    get_local_props(graph, node)
  }) %>%
    bind_rows()
  
  # Compute mean
  summary_df <- props_df %>%
    summarise(
      across(
        everything(),
        ~ if (is.numeric(.x)) mean(.x, na.rm = TRUE) else NA_real_ ) 
      ) %>%
    mutate(n_nodes = length(nodes_in_category)
    ) %>%
    rename_with(
      ~ paste0(.x, "_", category_name)
    )
  return(summary_df)
}

########### Function to make networks
################################################################################

# Note: It takes about 8 mins to run this through the n = 82 and n = 565 datasets together in this code.

make_networks <- function(app_session_level_df,
                          participant_level_df,
                          app_category_map, 
                          ppids_local_measures   # Only in CM dataset: the subset of participants with their 'most' and 'least' automatic apps identified and used at least once for the local network analysis of those nodes
) {
  

  ppids_all <- participant_level_df$ppid
  
  # Initialise list of graphs for each participant
  participant_level_df$network_orderA <- vector(
    "list",
    nrow(participant_level_df)
  )  
  
  # Loop through each participant
  for (pp_index in seq_along(ppids_all)) {
    
    set.seed(1) # necessary because the modularity uses the louvain algorithm, which is stochastic, so need to set seet for reproducible results
    
    pp <- ppids_all[pp_index]  # Get the current participant ID
    
    print(str_c("Processing participant number ", pp_index, " out of ", length(ppids_all)))  # Print progress
    
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
    # make the network
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
    
    if (nrow(n_sessions_with_apps) > length(unique(n_sessions_with_apps$appName))) {
      stop("n_sessions_with_apps has duplicate Apps; check - this is likely because it's been added to multiple times since initialisation.")
    }
    
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
      rename(n_sessions_with_appA        = n_sessions_with_app) %>%
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
    # save network 
    ################################################################################
    
    participant_level_df$network_orderA[[pp_index]] <- orderA_graph

    ################################################################################
    # extract properties for the graphs 
    ################################################################################
    
    ####### global properties
    global_properties_orderA        <- get_global_props(orderA_graph)
    
    ####### local properties
    
    # Calculate most and least automatic apps for the current participant, for the CM dataset (as this is the only one where the q was asked)
    if (all(c("most_automatic_app_recoded", "least_automatic_app_recoded") %in% names(participant_level_df))) {
      most_auto_app  <- filter(participant_level_df, ppid == pp)$most_automatic_app_recoded
      least_auto_app <- filter(participant_level_df, ppid == pp)$least_automatic_app_recoded
    }
    
    # if participant used their most and least auto app both at least once
    if ( !missing(ppids_local_measures)) { # basically checking if there are the list of PPIDs who use their most and least auto app at least once. This exists for the CM but not the ABCD dataset.
      if ( pp %in% unique(ppids_local_measures$ppid)) {
        
        mostAutoNode_properties_orderA   <- get_local_props(orderA_graph, most_auto_app)
        leastAutoNode_properties_orderA  <- get_local_props(orderA_graph, least_auto_app)
        
        ################################################################################
        # save properties for the graphs 
        ################################################################################
        
        ####### local properties
        colnames_mostAutoNode_properties_orderA    <- paste0(local_prop_cols, "_MostorderA")
        colnames_leastAutoNode_properties_orderA   <- paste0(local_prop_cols, "_LeastorderA")
        
        # local properties
        participant_level_df[participant_level_df$ppid == pp, colnames_mostAutoNode_properties_orderA]  <- mostAutoNode_properties_orderA
        participant_level_df[participant_level_df$ppid == pp, colnames_leastAutoNode_properties_orderA] <- leastAutoNode_properties_orderA

      } 
      
    } 
    
    ####### global properties
    # initialise columns if necessary
    if (pp_index == 1) {
      colnames_global_properties_orderA         <- paste0(colnames(global_properties_orderA), "_orderA")
      colnames_Social                           <- c(paste0(local_prop_cols, "_Social"), str_c("n_nodes_Social"))
      colnames_Communication                    <- c(paste0(local_prop_cols, "_Communication"), str_c("n_nodes_Communication"))
      colnames_Productivity                     <- c(paste0(local_prop_cols, "_Productivity"), str_c("n_nodes_Productivity"))
      colnames_Education                        <- c(paste0(local_prop_cols, "_Education"), str_c("n_nodes_Education"))
      colnames_Entertainment                    <- c(paste0(local_prop_cols, "_Entertainment"), str_c("n_nodes_Entertainment"))
      colnames_Maps                             <- c(paste0(local_prop_cols, "_Maps & Navigation"), str_c("n_nodes_Maps & Navigation"))
      colnames_Music                            <- c(paste0(local_prop_cols, "_Music & Audio"), str_c("n_nodes_Music & Audio"))
      colnames_News                             <- c(paste0(local_prop_cols, "_News & Magazines"), str_c("n_nodes_News & Magazines"))
      colnames_Health                           <- c(paste0(local_prop_cols, "_Health & Fitness"), str_c("n_nodes_Health & Fitness"))
      colnames_Lifestyle                        <- c(paste0(local_prop_cols, "_Lifestyle"), str_c("n_nodes_Lifestyle"))
      colnames_Casino                           <- c(paste0(local_prop_cols, "_Casino"), str_c("n_nodes_Casino"))
    }
    
    participant_level_df[participant_level_df$ppid == pp, colnames_global_properties_orderA]        <- global_properties_orderA
    participant_level_df[participant_level_df$ppid == pp, colnames_Social]        <- get_category_props(graph = orderA_graph, category_colNames = colnames_Social, category_name = "Social") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Productivity]  <- get_category_props(graph = orderA_graph, category_colNames = colnames_Productivity, category_name = "Productivity") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Education]     <- get_category_props(graph = orderA_graph, category_colNames = colnames_Education, category_name = "Education") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Communication] <- get_category_props(graph = orderA_graph, category_colNames = colnames_Communication, category_name = "Communication") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Entertainment] <- get_category_props(graph = orderA_graph, category_colNames = colnames_Entertainment, category_name = "Entertainment") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Maps]          <- get_category_props(graph = orderA_graph, category_colNames = colnames_Maps, category_name = "Maps & Navigation") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Music]         <- get_category_props(graph = orderA_graph, category_colNames = colnames_Music, category_name = "Music & Audio") 
    participant_level_df[participant_level_df$ppid == pp, colnames_News]          <- get_category_props(graph = orderA_graph, category_colNames = colnames_News, category_name = "News & Magazines") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Health]        <- get_category_props(graph = orderA_graph, category_colNames = colnames_Health, category_name = "Health & Fitness") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Lifestyle]     <- get_category_props(graph = orderA_graph, category_colNames = colnames_Lifestyle, category_name = "Lifestyle") 
    participant_level_df[participant_level_df$ppid == pp, colnames_Casino]        <- get_category_props(graph = orderA_graph, category_colNames = colnames_Casino, category_name = "Casino") 
    
  }
  
  return(participant_level_df)
}

################################################################################
############       Extract networks
################################################################################

# Get list of PPIDs for which there is a most auto and least auto app used at least once in the CM dataset, for the analyses with these

CM_ppids_local_measures <- CM_app_session_level_df %>% 
    right_join(select(CM_participant_level_df, ppid, most_automatic_app_recoded, least_automatic_app_recoded), by = "ppid") %>% 
    group_by(ppid) %>% 
    filter(any(App == most_automatic_app_recoded) & any(App == least_automatic_app_recoded)) %>% 
    ungroup()

# Now do the networks on each dataset, CM then ABCD
# Note: If want to debug, I suggest just making a dataframe with a subset of particiants and doing the function on those

CM_network_stats <- make_networks(
  app_session_level_df = CM_app_session_level_df,
  participant_level_df = CM_participant_level_df,
  app_category_map     = GooglePlay_CategoryMap_df,
  ppids_local_measures = CM_ppids_local_measures
)


ABCD_network_stats <- make_networks(
  app_session_level_df = ABCD_app_session_level_df,
  participant_level_df = ABCD_participant_level_df,
  app_category_map     = GooglePlay_CategoryMap_df
  #ppids_local_measures = NA
)

################################################################################
############       Plot results
################################################################################

## Define different dfs to test - full df and outliers
# to do so, we first get lists of ppids to exclude for different cases - either outliers or failed attention checks in questionnaires

# Get outliers for each qnr

########## CM 

CM_outliers_list <- list(
  modularity_louvain_orderA = find_outliers(CM_network_stats, "modularity_louvain_orderA")$outlier_ids,
  edge_density_orderA       = find_outliers(CM_network_stats, "edge_density_orderA")$outlier_ids,
  PHQ_8_score               = find_outliers(CM_network_stats, "PHQ_8_score")$outlier_ids,
  GAD_7_score               = find_outliers(CM_network_stats, "GAD_7_score")$outlier_ids,
  OCI_R_score               = find_outliers(CM_network_stats, "OCI_R_score")$outlier_ids,
  ATQ_short_score           = find_outliers(CM_network_stats, "ATQ_short_score")$outlier_ids,
  SRHI_phone_use_score      = find_outliers(CM_network_stats, "SRHI_phone_use_score")$outlier_ids,
  SRHI_social_media_use_score    = find_outliers(CM_network_stats, "SRHI_social_media_use_score")$outlier_ids,
  SRHI_most_automatic_app_score  = find_outliers(CM_network_stats, "SRHI_most_automatic_app_score")$outlier_ids,
  SRHI_least_automatic_app_score = find_outliers(CM_network_stats, "SRHI_least_automatic_app_score")$outlier_ids,
  BIS_total_score                = find_outliers(CM_network_stats, "BIS_total_score")$outlier_ids,
  BAS_total_score                = find_outliers(CM_network_stats, "BAS_total_score")$outlier_ids,
  BAS_fun_seeking_score          = find_outliers(CM_network_stats, "BAS_fun_seeking_score")$outlier_ids,
  BAS_drive_score                = find_outliers(CM_network_stats, "BAS_drive_score")$outlier_ids,
  BAS_reward_responsiveness_score = find_outliers(CM_network_stats, "BAS_reward_responsiveness_score")$outlier_ids
)

CM_ppid_failed_attention <- c("Z73", "Z109", "M7" ,  "Z54" , "Z115" ,"Z141", "Z137","Z55") # participants who failed at least one attention check

########## ABCD
ABCD_outliers_list <- list(
  modularity_louvain_orderA = find_outliers(ABCD_network_stats, "modularity_louvain_orderA")$outlier_ids,
  edge_density_orderA       = find_outliers(ABCD_network_stats, "edge_density_orderA")$outlier_ids,
  dsm_dep                   = find_outliers(ABCD_network_stats, "mh_y_ysr__dsm__dep_tscore")$outlier_ids,
  dsm_adhd                  = find_outliers(ABCD_network_stats, "mh_y_ysr__dsm__adhd_tscore")$outlier_ids,
  dsm_anx                   = find_outliers(ABCD_network_stats, "mh_y_ysr__dsm__anx_tscore")$outlier_ids
)


## RQ1: GLOBAL NETWORK PROPERTIES

global_features <- list(
  "modularity_louvain_orderA",
  "edge_density_orderA"
#  "mean_pathlength_orderA",
#  "eigen_centr_orderA",
#  "global_clust_orderA"
) # Note that for the paper I currently only include modularity and edge density, but you could easily change this to include the other variables too.

CM_qnr_outcome_vars <- list(
  PHQ       = "PHQ_8_score",
  GAD       = "GAD_7_score",
  OCD       = "OCI_R_score",
  ATC       = "ATQ_short_score",
  SRHIphone = "SRHI_phone_use_score",
  SRHIsm    = "SRHI_social_media_use_score"
)

CM_variations <- list(
  "Raw",
  "NoOutliers", 
  "NoAttFail",
  "TimeCtrl",
  "nAppsCtrl"
)

ABCD_qnr_outcome_vars <- list(
  DSM_Depression = "mh_y_ysr__dsm__dep_tscore",
  DSM_ADHD       = "mh_y_ysr__dsm__adhd_tscore",
  DSM_Anxiety    = "mh_y_ysr__dsm__anx_tscore"
)

ABCD_variations <- list( # Same as CM but don't have the attention failures
  "Raw", 
  "NoOutliers", 
  "TimeCtrl", 
  "nAppsCtrl"
)

## Table 1: Relating global measures to questionnaire scores
#####################

## Make functions to make the table and format it, then run this separately for (i) CM dataset and (ii) ABCD dataset
## Note that the procedure for these datasest are the same EXCEPT that they have different MH questionnaires and also there are no 'attention fail' participants in ABCD so we dont have that as a control

############## function to make result row (used inside run_RQ1_global_models and also under RQ3 later on)

make_model_row <- function(lm_mod, bf_mod, model_type, outcome, predictor) {
  
  coefs <- summary(lm_mod)$coefficients
  mf    <- model.frame(lm_mod)
  
  # SDs of outcome (first column of model frame) and predictor
  sd_y <- sd(mf[[outcome]])
  sd_x <- sd(mf[[predictor]])
  
  B_unstd <- coefs[predictor, "Estimate"]
  
  tibble(
    Dataset   = dataset_name,
    Outcome   = outcome,
    Predictor = predictor,
    ModelType = model_type,
    B         = B_unstd,
    Beta      = B_unstd * sd_x / sd_y,   # standardised beta
    Beta_StdErr  = coefs[predictor, "Std. Error"] * sd_x / sd_y,  
    R2        = summary(lm_mod)$r.squared,
    p_value   = summary(lm_mod)$coefficients[
      predictor, "Pr(>|t|)"
    ],
    BF        = as.vector(bf_mod)
  )
}


run_RQ1_global_models <- function(
    network_stats,
    qnr_outcome_vars,
    outliers_list,
    global_features,
    dataset_name,
    ppid_failed_attention = NULL
) {
  
  # This function will create a table which repeats the analyses linking App-journey features to mental health with several different senstivity analyses,
  # 1) Excluding participants: it repeats the analyses excluding participants with outlier values on either variable, or for the CM dataset, the eight participants who failed atteniton checks
  # 2) Controls: it repeats analyses controlling for mean time spent on phone per day and number of Apps in a network 
  
  all_results <- list()

  
  # Whether this dataset has an attention-failure exclusion (CM does but ABCD doesn't)
  has_attention_fail <- !is.null(ppid_failed_attention)
  
  
  for (global_feature in global_features) { # network features e.g. modularity, density etc
    
    for (qnr_outcome_var in qnr_outcome_vars) { # questionnaires e.g. mental health 
      
      ############## extract variable names
      
      global_network_feature <- global_feature[[1]]
      qnr_score              <- qnr_outcome_var[[1]]
      
      print(str_c(
        "Processing ", dataset_name, ": ",
        global_network_feature, " and ", qnr_score
      ))
      
      ## First, make formulas for linear models 
      
      ############## formulas
      
      formula_NoCtrl <- as.formula(
        paste(qnr_score, "~", global_network_feature)
      )
      
      # For both time and nApps controls, for the lmBF, we have to get the ratio of the variable of interest with that control to the same model with just that control, so compute both formulae here
      # This means the BF question asks: How much more (or less) supported is the model containing the global network feature, after accounting for the contorl (nApps or time spent), compared with a model containing nApps alone?
      # This means we are asking "Does adding the global network feature provide evidence beyond nApps?" rather than just "Is the full model supported?" which would be what we would get with jsut one lmBF alone
      
      formula_TimeCtrl <- as.formula(
        paste(
          qnr_score, "~", global_network_feature, "+ mean_timespent_per_day_hrs"
        )
      )
      
      formula_JustTime <- as.formula(
        paste(qnr_score, "~ mean_timespent_per_day_hrs")
      )
      
      formula_nAppsCtrl <- as.formula(
        paste(qnr_score, "~", global_network_feature, "+ nApps")
      )
      
      formula_JustnApps <- as.formula(
        paste(qnr_score, "~ nApps")
      )
      
      ## Now run the linear models 
      
      ############## Raw
      
      # The lmBF only works with complete cases (i.e. no NA)
      # Therefore, before doing every stat we have to get only the complete cases participants for each value
      # We do this once for the exclusion analyses as they all use the same formula, then we do it for each of time and nApps controls as these introduce new variables with potentially new missing cases
      
      df_Raw_complete <- network_stats[
        complete.cases(
          network_stats[, c(
            all.vars(formula_NoCtrl)
          ), drop = FALSE]
        ),
      ]

      lm_mod_raw <- lm(
        formula_NoCtrl,
        data = df_Raw_complete
      )
      
      bf_mod_raw <- lmBF(
        formula_NoCtrl,
        data = df_Raw_complete
      )

      ############## NoOutlier
      
      df_NoOutlier <- filter(
        df_Raw_complete,
        !(ppid %in% outliers_list[[global_network_feature]]),
        !(ppid %in% outliers_list[[qnr_score]])
      )
      
      
      lm_mod_NoOutlier <- lm(
        formula_NoCtrl,
        data = df_NoOutlier
      )
      
      bf_mod_NoOutlier <- lmBF(
        formula_NoCtrl,
        data = df_NoOutlier
      )
      
      ############## No Attention Fail
      # Only run for CM
      
      if (has_attention_fail) {
        
        df_NoAttFail <- filter(
          df_Raw_complete,
          !(ppid %in% ppid_failed_attention)
        )
        
        lm_mod_NoAttFail <- lm(
          formula_NoCtrl,
          data = df_NoAttFail
        )
        
        bf_mod_NoAttFail <- lmBF(
          formula_NoCtrl,
          data = df_NoAttFail
        )
        
      }

      ############## TimeCtrl
      
      # lmBF doesn't deal with missing cases, so explicitly
      # remove participants missing mean_timespent_per_day_hrs
      
      df_TimeCtrl_complete <- network_stats[
        complete.cases(
          network_stats[, c(
            all.vars(formula_TimeCtrl),
            all.vars(formula_JustTime)
          ), drop = FALSE]
        ),
      ]
      
      
      lm_mod_TimeCtrl <- lm(
        formula_TimeCtrl,
        data = df_TimeCtrl_complete
      )
      
      bf_mod_TimeCtrl <- (
        lmBF(
          formula_TimeCtrl,
          data = df_TimeCtrl_complete
        ) /
          lmBF(
            formula_JustTime,
            data = df_TimeCtrl_complete
          )
      )
      
      
      ############## nAppsCtrl
      
      
      df_nAppsCtrl_complete <- network_stats[
        complete.cases(
          network_stats[, c(
            all.vars(formula_nAppsCtrl),
            all.vars(formula_JustnApps)
          ), drop = FALSE]
        ),
      ]
      
      
      lm_mod_nAppsCtrl <- lm(
        formula_nAppsCtrl,
        data = df_nAppsCtrl_complete
      )
      
      bf_mod_nAppsCtrl <- (
        lmBF(
          formula_nAppsCtrl,
          data = df_nAppsCtrl_complete
        ) /
          lmBF(
            formula_JustnApps,
            data = df_nAppsCtrl_complete
          )
      )
      
      
      ############## make rows
      
      row_Raw <- make_model_row(
        lm_mod_raw,
        bf_mod_raw,
        "Raw", 
        outcome   = qnr_score, 
        predictor = global_network_feature
      )
      
      row_NoOutlier <- make_model_row(
        lm_mod_NoOutlier,
        bf_mod_NoOutlier,
        "NoOutlier",
        outcome   = qnr_score, 
        predictor = global_network_feature
        
      )
      
      row_TimeCtrl <- make_model_row(
        lm_mod_TimeCtrl,
        bf_mod_TimeCtrl,
        "TimeControlled",
        outcome   = qnr_score, 
        predictor = global_network_feature
        
      )
      
      row_nAppsCtrl <- make_model_row(
        lm_mod_nAppsCtrl,
        bf_mod_nAppsCtrl,
        "nAppsControlled",
        outcome   = qnr_score, 
        predictor = global_network_feature
        
      )
      
      
      # Add NoAttFail only for datasets that have it
      
      if (has_attention_fail) {
        
        row_NoAttFail <- make_model_row(
          lm_mod_NoAttFail,
          bf_mod_NoAttFail,
          "NoAttFail",
          outcome   = qnr_score, 
          predictor = global_network_feature
          
        )
        
        all_results <- bind_rows(
          all_results,
          row_Raw,
          row_NoOutlier,
          row_NoAttFail,
          row_TimeCtrl,
          row_nAppsCtrl
        )
        
      } else {
        
        all_results <- bind_rows(
          all_results,
          row_Raw,
          row_NoOutlier,
          row_TimeCtrl,
          row_nAppsCtrl
        )
        
      }
      
    }
    
  }
  
  return(all_results)
}  

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
      B,
      Beta,
      Beta_StdErr,
      p_value,
      R2,
      BF
    )
  
  # Round numeric columns to 3 decimals
  df <- df %>%
    mutate(
      B           = round(B, 3), 
      Beta        = round(Beta, 3),
      Beta_StdErr = round(Beta_StdErr, 3),
      p_value  = round(p_value, 3),
      R2       = round(R2, 3),
      BF       = round(BF, 3)
    )
  
  # Add significance stars column after p_value
  df <- df %>%
    mutate(sig = sapply(p_value, add_sig_stars),
           BF_interpretation = sapply(BF, add_BF_interpretation)
           ) %>%
    select(Outcome, ModelType, B, Beta, Beta_StdErr, R2, p_value, sig, BF, BF_interpretation)

  return(df)
}

########### Run and get results for Table 1 & 2 / RQ 1, for CM then ABCD

CM_RQ1_results <- run_RQ1_global_models(
  network_stats         = CM_network_stats,
  qnr_outcome_vars      = CM_qnr_outcome_vars,
  outliers_list         = CM_outliers_list,
  global_features       = global_features,
  dataset_name          = "CM",
  ppid_failed_attention = CM_ppid_failed_attention
)

CM_formatted_table <- format_table(CM_RQ1_results)

ABCD_RQ1_results <- run_RQ1_global_models(
  network_stats         = ABCD_network_stats,
  qnr_outcome_vars      = ABCD_qnr_outcome_vars,
  outliers_list         = ABCD_outliers_list,
  global_features       = global_features,
  dataset_name          = "ABCD",
  ppid_failed_attention = NULL
)

ABCD_formatted_table <- format_table(ABCD_RQ1_results)


# Save CSV

write.csv(CM_formatted_table, str_c(parent_dir,"/output/CM_Table1.csv"), row.names = FALSE)
export_results_to_word(CM_formatted_table, str_c(parent_dir,"/output/CM_Table1.docx"))

write.csv(ABCD_formatted_table, str_c(parent_dir,"/output/ABCD_Table2.csv"), row.names = FALSE)
export_results_to_word(ABCD_formatted_table, str_c(parent_dir,"/output/ABCD_Table2.docx"))


## Figure 2: Relating modularity and density to questionnaire scores
#####################

mh_variables_CM <- c(
  "PHQ_8_score", "GAD_7_score", "OCI_R_score"
)
mh_variables_ABCD <- c(
  "mh_y_ysr__dsm__dep_tscore", "mh_y_ysr__dsm__anx_tscore", "mh_y_ysr__dsm__adhd_tscore"
)

color_palette_MH_CM_Fig2  <- c("pink","#1f77b4", "#7e57c2")
color_palette_MH_ABCDFig2 <- c("pink", "#1f77b4", "#26a69a")

Fig2a <- plot_measure_facet1row(CM_network_stats, "modularity_louvain_orderA", mh_variables_CM, color_palette_MH_CM_Fig2) + theme(panel.spacing.x = unit(1.5, "lines"))
Fig2b <- plot_measure_facet1row(ABCD_network_stats, "modularity_louvain_orderA", mh_variables_ABCD, color_palette_MH_ABCDFig2)+ theme(panel.spacing.x = unit(1.5, "lines"))
Fig2c <- plot_measure_facet1row(CM_network_stats, "edge_density_orderA", mh_variables_CM, color_palette_MH_CM_Fig2)+ theme(panel.spacing.x = unit(1.5, "lines"))
Fig2d <- plot_measure_facet1row(ABCD_network_stats, "edge_density_orderA", mh_variables_ABCD, color_palette_MH_ABCDFig2)+ theme(panel.spacing.x = unit(1.5, "lines"))

## Fig 
Fig2 <- cowplot::plot_grid(
  Fig2a, Fig2b, Fig2c, Fig2d,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1, 1, 1, 1)
)

ggsave(
  str_c(parent_dir, "/output/figures/Fig2.pdf"),
  plot = Fig2,
  width = 17,
  height = 20,
  units = "in"
)


## RQ2: LOCAL NETWORK PROPERTIES

## Figure 5: Relating local properties to self-reported Most vs. Least Auto Apps
#####################

Fig5a <- plot_raincloud2vars(CM_network_stats, "in_strength_MostorderA", "in_strength_LeastorderA")
Fig5b <- plot_raincloud2vars(CM_network_stats, "out_strength_MostorderA", "out_strength_LeastorderA")
Fig5c <- plot_raincloud2vars(CM_network_stats, "in_diversity_MostorderA", "in_diversity_LeastorderA")
Fig5d <- plot_raincloud2vars(CM_network_stats, "out_diversity_MostorderA", "out_diversity_LeastorderA")
Fig5e <- plot_raincloud2vars(CM_network_stats, "eigen_centrality_MostorderA", "eigen_centrality_LeastorderA")
Fig5f <- plot_raincloud2vars(CM_network_stats, "coreness_MostorderA", "coreness_LeastorderA")
Fig5g <- plot_raincloud2vars(CM_network_stats, "local_clust_MostorderA", "local_clust_LeastorderA")
Fig5h <- plot_raincloud2vars(CM_network_stats, "betweenness_MostorderA", "betweenness_LeastorderA")

Fig5_plots <- c(Fig5a, Fig5b, Fig5c, Fig5d, Fig5e, Fig5f, Fig5g, Fig5h)

Fig5 <- cowplot::plot_grid(
  plotlist = Fig5_plots,
  ncol = 4,
  align = "hv",
  axis = "lr"
)

ggsave(str_c(parent_dir,"/output/figures/Fig5.pdf"), plot = Fig5, width = 8, height = 4, units = "in")

## stats
#####################

# ============================================================
# DATASET - only CM as ABCD doesn't have the local properties of Most/Least Auto App
# ============================================================

CM_network_stats_NoAttFail <- CM_network_stats %>%
  filter(!(ppid %in% CM_ppid_failed_attention))

# ============================================================
# FUNCTION: paired t-test + Bayes factor
# ============================================================

run_paired_test <- function(data, metric) {
  
  x <- data[[paste0(metric, "_MostorderA")]]
  y <- data[[paste0(metric, "_LeastorderA")]]
  
  # Replace Inf / -Inf with NA
  x[!is.finite(x)] <- NA
  y[!is.finite(y)] <- NA
  
  # Keep complete paired observations
  df <- na.omit(data.frame(x = x, y = y))
  
  # Frequentist test
  t_test <- t.test(
    df$x,
    df$y,
    paired = TRUE
  )
  
  # Bayes factor
  bayes_factor <- ttestBF(
    x = df$x,
    y = df$y,
    paired = TRUE
  )
  
  # Print results
  cat("\n----------------------------------------\n")
  cat(metric, "\n")
  cat("----------------------------------------\n")
  print(t_test)
  print(bayes_factor)
  
  # Return results
  list(
    data = df,
    t_test = t_test,
    bayes_factor = bayes_factor
  )
}


# ============================================================
# METRICS
# ============================================================

metrics <- c(
  "in_strength",
  "out_strength",
  "in_diversity",
  "out_diversity",
  "eigen_centrality",
  "coreness",
  "local_clust",
  "betweenness"
)


# ============================================================
# RUN ALL TESTS
# ============================================================

# ----------------------------
# All participants
# ----------------------------

CM_results <- lapply(
  metrics,
  function(metric) run_paired_test(
    data = CM_network_stats,
    metric = metric
  )
)

names(CM_results) <- metrics


# ----------------------------
# Excluding attention failures
# ----------------------------

CM_results_NoAttFail <- lapply(
  metrics,
  function(metric) run_paired_test(
    data = CM_network_stats_NoAttFail,
    metric = metric
  )
)

names(CM_results_NoAttFail) <- metrics


# Extra visualisations (Fig 6 and 7; not currently in paper)

## Figure 6: Visualising networks, for high and low depression participants specifically
#####################

networks_all        <- CM_network_stats$network_orderA
names(networks_all) <- CM_network_stats$ppid

# filter to participants who we have the most and least auto apps for so that we can draw networks for them where we highlight these apps
networks_usedApps <- networks_all[
  names(networks_all) %in% unique(CM_ppids_local_measures$ppid)
]

# first, set colours for google categories of social/communication vs. productive
GooglePlay_CategoryMap_df$GooglePlay_category_altered <-
  ifelse(
    GooglePlay_CategoryMap_df$GooglePlay_category %in% c("Social", "Communication"),
    "Social/Communication",
    GooglePlay_CategoryMap_df$GooglePlay_category
  )

## choosing ppids to plot: take the four with the highest and four with the lowest scores on a given questionnaire and plot

n_toPlot <- 8
df_ranked <- CM_ppids_local_measures %>%
  group_by(ppid) %>% slice(1) %>% ungroup() %>%
  left_join(CM_network_stats %>% select(ppid, PHQ_8_score),
            by = "ppid") %>%
  arrange(PHQ_8_score)
ppids_toPlot <- unlist(list(
  df_ranked %>% slice_head(n = n_toPlot/2) %>% pull(ppid),
  df_ranked %>% slice_tail(n = n_toPlot/2) %>% pull(ppid)
))

# plot and save each of their graphs individually

ppid_APP_info <- data.frame(
  ppid           = character(),
  most_auto_app  = character(),
  least_auto_app = character(),
  highest_ec     = numeric(),
  lowest_ec      = numeric()
)

for (ppid_graph in seq_along(ppids_toPlot)) {
  
  ppid_plot <- ppids_toPlot[ppid_graph]
  phq       <- filter(df_ranked, ppid == ppid_plot)$PHQ_8_score
  
  print(str_c("Processing participant number ", ppid_plot))  # Print progress
  
  g <- networks_usedApps[[ppid_plot]]
  most_auto_app_ppid        <- filter(CM_network_stats, ppid == ppid_plot)$most_automatic_app_recoded
  least_auto_app_ppid       <- filter(CM_network_stats, ppid == ppid_plot)$least_automatic_app_recoded
  
  # prepare size to scale nodes by centrality
  ec        <- eigen_centrality(g, directed = TRUE, weights = E(g)$weight)$vector
  V(g)$size <- 3 + 25 * (ec / max(ec))
  
  # colour scheme
  
  vertex_df <- data.frame(
    node_name = V(g)$name
  ) %>%
    left_join(
      GooglePlay_CategoryMap_df,
      by = c("node_name" = "App")
    )
  V(g)$GooglePlay_category_altered <- vertex_df$GooglePlay_category_altered
  
  vertex_colors <- V(g)$color
  # Colour Social apps
  vertex_colors[V(g)$GooglePlay_category_altered == "Social/Communication"] <- "pink"
  # Colour Productivity apps
  vertex_colors[V(g)$GooglePlay_category_altered == "Productivity"] <- "lightblue"

  # now set frame colours for most vs. least automatic apps - will outline these in diff colours
  vertex_frame_color                                   <- rep("black", vcount(g))
  vertex_frame_color[V(g)$name == most_auto_app_ppid]  <- automatic_palette["Most Automatic"]
  vertex_frame_color[V(g)$name == least_auto_app_ppid] <- automatic_palette["Least Automatic"]
  
  # now set edge colours to be transparent
  edge_col <- adjustcolor("grey50", alpha.f = 0.2)
  
  # set frame width for most and least auto
  vertex_frame_width <- rep(0.5, vcount(g))  # small default
  vertex_frame_width[V(g)$name == most_auto_app_ppid]  <- 6
  vertex_frame_width[V(g)$name == least_auto_app_ppid] <- 6
  
  pdf(str_c(parent_dir, "/output/figures/graph_vis/", ppid_plot, "_PHQ8_", phq, "plot_vis.pdf"), width = 12, height = 12)  # square canvas
  
  plot(
    g,
    layout             = layout_with_fr(g),
    vertex.label       = NA,
    vertex.size        = V(g)$size,
    vertex.color       = vertex_colors,
    vertex.frame.color = vertex_frame_color,
    vertex.frame.width = vertex_frame_width,
    edge.color         = edge_col,
    edge.width         = E(g)$width,
    main               = ppid_plot
  )
  
  dev.off()  
  
  # record info
  ppid_APP_info <- rbind(
    ppid_APP_info,
    data.frame(
      ppid           = ppid_plot,
      most_auto_app  = most_auto_app_ppid,
      least_auto_app = least_auto_app_ppid,
      highest_ec     = names(ec[ec == max(ec)]),
      lowest_ec      = names(ec[ec == min(ec)])
    )
  )
}

## Figure 7: Comparing average local properties for different categories
#####################

for (v in local_prop_cols) {
  
  print(str_c("Processing ", v, " out of ", length(local_prop_cols)))  # Print progress
  
  g_CM <- plot_multi_rainclouds(
      CM_network_stats,
      base_var = v,
      title    = paste(v, "by Domain")
      )
  
  g_ABCD <- plot_multi_rainclouds(
    ABCD_network_stats,
    base_var = v,
    title    = paste(v, "by Domain")
  )
  
  
  ggsave(str_c(parent_dir, '/output/figures/category_properties/', v, '_catComparisonCM.pdf'), 
               plot   = g_CM, 
               width  = 10, 
               height = 4, 
               units  = "in"
               )
  
  ggsave(str_c(parent_dir, '/output/figures/category_properties/', v, '_catComparisonABCD.pdf'), 
         plot   = g_ABCD, 
         width  = 10, 
         height = 4, 
         units  = "in"
  )

}

#### Stats - compare Social and productivity for each feature

# Function to compare Social vs Productivity for one feature
run_prop_test <- function(cat, dataset) {
  
  x <- dataset[[paste0(cat, "_Social")]]
  y <- dataset[[paste0(cat, "_Productivity")]]
  
  # Remove Inf / -Inf
  x[!is.finite(x)] <- NA
  y[!is.finite(y)] <- NA
  
  # Keep complete paired cases
  df <- na.omit(data.frame(x = x, y = y))
  
  # Paired t-test
  t_result <- t.test(df$x, df$y, paired = TRUE)
  
  # Bayes factor
  bf_result <- ttestBF(
    x = df$x,
    y = df$y,
    paired = TRUE
  )
  
  # Print results
  cat("\n=========================\n")
  cat(toupper(cat), "\n")
  cat("=========================\n")
  print(t_result)
  print(bf_result)
  
  # Return results
  list(
    data = df,
    t_test = t_result,
    bayes_factor = bf_result
  )
}

local_props <- c( "in_strength", "out_strength", "in_diversity", "out_diversity", "eigen_centrality", "coreness", "local_clust", "betweenness" )

# CM 
CM_results <- lapply( local_props, run_prop_test, dataset = CM_network_stats ) 
names(CM_results) <- local_props # ABCD ABCD_results <- lapply( cats, run_cat_test, dataset = ABCD_network_stats )

# ABCD 
CM_results <- lapply( local_props, run_prop_test, dataset = CM_network_stats ) 
names(CM_results) <- local_props # ABCD ABCD_results <- lapply( cats, run_cat_test, dataset = ABCD_network_stats )



## RQ3: RELATING MENTAL HEALTH TO TIME SPENT

### General function to correlate time spent to MH questionnaires

standardised_lm_results <- function(data, outcome, predictor, dataset_name = NULL) {
  
  # Complete cases on just the two variables
  d      <- as.data.frame(data[complete.cases(data[, c(outcome, predictor)]),
                          c(outcome, predictor)])
  
  fml    <- reformulate(predictor, response = outcome)
  lm_mod <- lm(fml, data = d)
  # Bayes factor (BF10)
  bf_mod <- lmBF(fml, data = d)
  
  make_model_row( # This function is defined in the script above
    lm_mod     = lm_mod, 
    bf_mod     = bf_mod, 
    model_type = NA,
    outcome    = outcome, 
    predictor  = predictor
  )
  
}

### CM - with attention fail

CM_dep_time <- standardised_lm_results(
  data         = CM_network_stats,
  outcome      = "PHQ_8_score",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "CM"
)
CM_dep_time

CM_anx_time <- standardised_lm_results(
  data         = CM_network_stats,
  outcome      = "GAD_7_score",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "CM"
)
CM_anx_time

CM_ocd_time <- standardised_lm_results(
  data         = CM_network_stats,
  outcome      = "OCI_R_score",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "CM"
)
CM_ocd_time

### CM - with no attention fail

CM_dep_time <- standardised_lm_results(
  data         = CM_network_stats_NoAttFail,
  outcome      = "PHQ_8_score",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "CM"
)
CM_dep_time

CM_anx_time <- standardised_lm_results(
  data         = CM_network_stats_NoAttFail,
  outcome      = "GAD_7_score",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "CM"
)
CM_anx_time

CM_ocd_time <- standardised_lm_results(
  data         = CM_network_stats_NoAttFail,
  outcome      = "OCI_R_score",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "CM"
)
CM_ocd_time


### ABCD

ABCD_dep_time <- standardised_lm_results(
  data         = ABCD_network_stats,
  outcome      = "mh_y_ysr__dsm__dep_tscore",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "ABCD"
)
ABCD_dep_time

ABCD_anx_time <- standardised_lm_results(
  data         = ABCD_network_stats,
  outcome      = "mh_y_ysr__dsm__anx_tscore",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "ABCD"
)
ABCD_anx_time

ABCD_adhd_time <- standardised_lm_results(
  data         = ABCD_network_stats,
  outcome      = "mh_y_ysr__dsm__adhd_tscore",
  predictor    = "mean_timespent_per_day_hrs",
  dataset_name = "ABCD"
)
ABCD_adhd_time


## SUPPLEMENTARY

## Figure S1 is made in the '03-descriptives.R' script.

## Figure S2: Relating modularity and density to cognitive questionnaire scores in the CM dataset
#####################


###

## Figure S2: Relating modularity and density to cognitive questionnaire scores in the CM dataset
#####################

variables_cog_qnrs_row1 <- c(
  "BIS_total_score",
  "BAS_total_score"
  )

variables_cog_qnrs_row2 <- c(
  "BAS_fun_seeking_score",
  "BAS_drive_score",
  "BAS_reward_responsiveness_score"
)

variables_cog_qnrs_row3 <- c(
  "ATQ_short_score",
  "SRHI_phone_use_score",
  "SRHI_social_media_use_score"
)

color_palette_row1 <- c("#1B5E20", "#A8D5AA")
color_palette_row2 <- c("#A8D5AA", "#A8D5AA", "#A8D5AA")
color_palette_row3 <- c("#E67E22","#C65D5D", "#C65D5D")



## -------------------------
## Figure S2: Modularity
## -------------------------

FigS2a <- plot_measure_facet1row(
  CM_network_stats,
  "modularity_louvain_orderA",
  variables_cog_qnrs_row1,
  color_palette_row1
) +
  theme(panel.spacing.x = unit(1.5, "lines"))

FigS2b <- plot_measure_facet1row(
  CM_network_stats,
  "modularity_louvain_orderA",
  variables_cog_qnrs_row2,
  color_palette_row2
) +
  theme(panel.spacing.x = unit(1.5, "lines"))

FigS2c <- plot_measure_facet1row(
  CM_network_stats,
  "modularity_louvain_orderA",
  variables_cog_qnrs_row3,
  color_palette_row3
) +
  theme(panel.spacing.x = unit(1.5, "lines"))


FigS2 <- cowplot::plot_grid(
  FigS2a,
  FigS2b,
  FigS2c,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1, 1, 1)
)


ggsave(
  str_c(parent_dir, "/output/figures/FigS2.pdf"),
  plot = FigS2,
  width = 17,
  height = 20,
  units = "in"
)


## -------------------------
## Figure S3: Edge density
## -------------------------

FigS3a <- plot_measure_facet1row(
  CM_network_stats,
  "edge_density_orderA",
  variables_cog_qnrs_row1,
  color_palette_row1
) +
  theme(panel.spacing.x = unit(1.5, "lines"))

FigS3b <- plot_measure_facet1row(
  CM_network_stats,
  "edge_density_orderA",
  variables_cog_qnrs_row2,
  color_palette_row2
) +
  theme(panel.spacing.x = unit(1.5, "lines"))

FigS3c <- plot_measure_facet1row(
  CM_network_stats,
  "edge_density_orderA",
  variables_cog_qnrs_row3,
  color_palette_row3
) +
  theme(panel.spacing.x = unit(1.5, "lines"))


FigS3 <- cowplot::plot_grid(
  FigS3a,
  FigS3b,
  FigS3c,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(1, 1, 1)
)

ggsave(
  str_c(parent_dir, "/output/figures/FigS3.pdf"),
  plot = FigS3,
  width = 17,
  height = 20,
  units = "in"
)


## stats linking cognitive questionnaires to global network features
#####################

cog_qnr_outcomes <- c("BIS_total_score", "BAS_total_score",
                     "BAS_fun_seeking_score", "BAS_drive_score", "BAS_reward_responsiveness_score",
                     "ATQ_short_score", "SRHI_phone_use_score", "SRHI_social_media_use_score")

CM_cognitive_modularity <- purrr::map_dfr(
  cog_qnr_outcomes,
  ~ standardised_lm_results(CM_network_stats, .x, "modularity_louvain_orderA", "CM")
)
CM_cognitive_modularity

CM_cognitive_density <- purrr::map_dfr(
  cog_qnr_outcomes,
  ~ standardised_lm_results(CM_network_stats, .x, "edge_density_orderA", "CM")
)
CM_cognitive_density
