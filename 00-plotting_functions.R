
################################################################################
############# PLOTTING FUNCTIONS                                  ##############
############# Feb 2025.                                           ##############
############# Georgia Turner, georgianjt@gmail.com                ##############
################################################################################

# This defines functions to make plots, and is imported using the 'source' function
# to the relevant other scripts in this folder.
####################################################################################################
# define settings for all plots
# Define a reusable color palette
automatic_palette <- c("Most Automatic" = "#1b9e77", "Least Automatic" = "#d95f02")


# Define the function
plot_density_by_gender <- function(data, score_variable, clinical_cutoff = NULL) {  
  plot <- ggplot(data, aes(x = {{ score_variable }}, fill = gender)) +
    geom_density(alpha = 0.7, color = "black") +
    labs(title = paste("Distribution of", deparse(substitute(score_variable)), "by Gender"), x = deparse(substitute(score_variable))) +
    theme_classic() +
    facet_wrap(~gender, ncol = 1) +  # Stack plots vertically by gender
    
    # Add clinical cutoff if provided
    { 
      if (!is.null(clinical_cutoff)) {
        list(
          geom_vline(data = data.frame(gender = unique(data$gender), x = clinical_cutoff, label = "Clinical Cutoff"), 
                     aes(xintercept = x), color = "red", linetype = "dashed"),
          geom_text(data = data.frame(gender = unique(data$gender), x = clinical_cutoff, label = "Clinical Cutoff"), 
                    aes(x = x, y = Inf, label = label), vjust = 1, hjust = 0, color = "red")
        )
      } else {
        list()
      }
    } +
    
    geom_vline(data = data %>%
                 group_by(gender) %>%
                 dplyr::summarize(median = median({{ score_variable }})), 
               aes(xintercept = median), color = "black", linetype = "dashed") +
    geom_text(data = data %>%
                group_by(gender) %>%
                dplyr::summarize(median = median({{ score_variable }})) %>%
                mutate(label = "Median"), 
              aes(x = median, y = Inf, label = label), vjust = 1, hjust = 1, color = "black")
  
  return(plot)
}

# Define the function
plot_density <- function(data, score_variable, clinical_cutoff = NULL, fill_color = NULL) {  
  plot <- ggplot(data, aes(x = {{ score_variable }})) +
    geom_density(fill = if (!is.null(fill_color)) fill_color else "blue", 
                 alpha = 0.7, color = "black") +
    
    labs(title = paste("Distribution of", deparse(substitute(score_variable))), 
         x = deparse(substitute(score_variable))) +
    theme_minimal() +
    
    # Add clinical cutoff if provided
    {  
      if (!is.null(clinical_cutoff)) {
        list(
          geom_vline(aes(xintercept = clinical_cutoff), color = "red", linetype = "dashed"),
          geom_text(aes(x = clinical_cutoff, y = Inf, label = "Clinical Cutoff"), 
                    vjust = 1, hjust = 0, color = "red")
        )
      } else {
        list()
      }
    } +
    
    geom_vline(aes(xintercept = median({{ score_variable }})), 
               color = "black", linetype = "dashed") +
    geom_text(aes(x = median({{ score_variable }}), y = Inf, label = "Median"), 
              vjust = 1, hjust = 1, color = "black")
  
  return(plot)
}

# Function to plot all variables on a grid
plot_all_densities <- function(data) {
  automatic_palette <- c("Most Automatic" = "#1b9e77", "Least Automatic" = "#d95f02")
  
  plots <- list(
    plot_density(data, SRHI_phone_use_score),
    plot_density(data, SRHI_social_media_use_score),
    plot_density(data, SRHI_most_automatic_app_score, fill_color = automatic_palette["Most Automatic"]),
    plot_density(data, SRHI_least_automatic_app_score, fill_color = automatic_palette["Least Automatic"])
  )
  
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
}


plot_raincloud <- function(data, variable, label, fill_color = "#1f77b4") {
  ggplot(data, aes(x = "", y = !!rlang::ensym(variable), fill = label)) +
    stat_halfeye(
      adjust = 0.5,
      justification = -0.2,
      .width = 0,
      point_colour = NA
    ) +
    geom_boxplot(
      width = 0.12,
      outlier.shape = NA,
      alpha = 0.5
    ) +
    geom_jitter(
      width = 0.05,
      alpha = 0.3,
      size = 1
    ) +
    scale_fill_manual(values = setNames(fill_color, label)) +
    labs(title = label, y = NULL, x = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
}


plot_combined_rainclouds <- function(data) {
  # Define color palette
  automatic_palette <- c(
    "Phone Use" = "#aec7e8",
    "Social Media Use" = "#aec7e8",
    "Most Automatic" = "#1b9e77",
    "Least Automatic" = "#d95f02"
  )
  
  # Reshape data into long format
  data_long <- data %>%
    mutate(id = row_number()) %>%
    select(
      id,
      `Phone Use` = SRHI_phone_use_score,
      `Social Media Use` = SRHI_social_media_use_score,
      `Most Automatic` = SRHI_most_automatic_app_score,
      `Least Automatic` = SRHI_least_automatic_app_score
    ) %>%
    pivot_longer(
      -id,
      names_to = "Variable",
      values_to = "Score"
    )
  
  # Create raincloud plot
  ggplot(data_long, aes(x = Variable, y = Score, fill = Variable)) +
    stat_halfeye(
      adjust = 0.4,
      justification = -0.5,
      width = 0.4,
      point_colour = NA,
      interval_size = NA
    ) +
    geom_boxplot(
      width = 0.1,
      outlier.shape = NA,
      alpha = 0.5,
      aes(color = Variable)
      
    ) +
    geom_jitter(
      width = 0.08,
      alpha = 0.3,
      size = 3,
      aes(color = Variable),
      stroke = 0,
      shape = 16
    ) +
    scale_fill_manual(values = automatic_palette) +
    scale_color_manual(values = automatic_palette) +
    labs(
      title = "SRHI Raincloud Plots",
      x = NULL,
      y = "SRHI Score"
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

plot_statistic <- function(stat_type) {
  # Define the color palette
  automatic_palette <- c(
    "Least Automatic" = "#d95f02",
    "Most Automatic" = "#1b9e77"
  )
  
  # Define the columns and labels based on the stat_type
  if (stat_type == "mean") {
    stat_columns <- c("mean_duration_least", "mean_duration_most")
    stat_labels <- c("Least Automatic", "Most Automatic")
  } else if (stat_type == "median") {
    stat_columns <- c("median_duration_least", "median_duration_most")
    stat_labels <- c("Least Automatic", "Most Automatic")
  } else if (stat_type == "sd") {
    stat_columns <- c("sd_duration_least", "sd_duration_most")
    stat_labels <- c("Least Automatic", "Most Automatic")
  } else {
    stop("Invalid stat_type. Please choose from 'mean', 'median', or 'sd'.")
  }
  
  # Reshape the data to long format
  df_long <- automatic_app_appsessions_duration %>%
    pivot_longer(cols = stat_columns,
                 names_to = "Condition", 
                 values_to = "Duration") %>%
    mutate(Condition = factor(Condition, levels = stat_columns,
                              labels = stat_labels))
  
  # Compute means for the black dots/line
  means_df <- df_long %>%
    group_by(Condition) %>%
    summarise(mean_duration = mean(Duration, na.rm = TRUE), .groups = "drop")
  
  # Create raincloud plot with points and mean markers
  ggplot(df_long, aes(x = Condition, y = Duration, fill = Condition)) +
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
      aes(color = Condition)
    ) +
    
    geom_jitter(
      width = 0.08,
      alpha = 0.3,
      size = 2,
      aes(color = Condition),
      stroke = 0,
      shape = 16
    ) +
    geom_line(
      data = means_df,
      aes(x = Condition, y = mean_duration, group = 1),
      color = "black", linewidth = 1
    ) +
    
    geom_point(
      data = means_df,
      aes(x = Condition, y = mean_duration),
      color = "black", size = 4, shape = 21,
    ) +
    labs(
      title = paste("Raincloud Plot for", stat_type, "Duration"),
      x = "",
      y = "Duration"
    ) +
    scale_fill_manual(values = automatic_palette) +
    scale_color_manual(values = automatic_palette) +
    theme_classic() +
    theme(legend.position = "none")
}

plot_raincloud2vars <- function(df, col_most, col_least) {
  # Define the color palette
  automatic_palette <- c(
    "Least Automatic" = "#d95f02",
    "Most Automatic" = "#1b9e77"
  )
  
  # Filter complete cases for paired t-test
  df_complete <- df %>% filter(!is.na(.data[[col_most]]) & !is.na(.data[[col_least]]))
  
  # Convert to long format
  df_long <- df_complete %>%
    select(all_of(c(col_most, col_least))) %>%
    pivot_longer(cols = everything(), names_to = "Condition", values_to = "Score") %>%
    mutate(Condition = recode(Condition,
                              !!col_least := "Least Automatic",
                              !!col_most := "Most Automatic"))
  
  # Compute means for the black dots/line
  means_df <- df_long %>%
    group_by(Condition) %>%
    summarise(mean_score = mean(Score, na.rm = TRUE), .groups = "drop")
  
  # Perform paired t-test
  t_test <- t.test(df_complete[[col_most]], df_complete[[col_least]], paired = TRUE)
  p_value <- t_test$p.value
  n <- nrow(df_complete)
  
  # Determine significance level
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ "ns"
  )
  
  # Generate raincloud plot
  ggplot(df_long, aes(x = Condition, y = Score, fill = Condition)) +
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
      aes(color = Condition)
    ) +
    geom_jitter(
      width = 0.08,
      alpha = 0.3,
      size = 2,
      aes(color = Condition),
      stroke = 0,
      shape = 16
    ) +
    geom_line(
      data = means_df,
      aes(x = Condition, y = mean_score, group = 1),
      color = "black", linewidth = 0.65
    ) +
    geom_point(
      data = means_df,
      aes(x = Condition, y = mean_score),
      color = "black", size = 2, shape = 21
    ) +
    labs(
      title = paste(col_most, "vs.", col_least),
      x = "",
      y = "Score"
    ) +
    ggplot2::annotate("text", x = 1.5, y = max(df_long$Score, na.rm = TRUE) * 1.05, 
             label = paste0("Paired t-test: p = ", signif(p_value, 3), " (", significance, ")"), 
             size =1.5, hjust = 0.5) +
    ggplot2::annotate("text", x = 1.5, y = max(df_long$Score, na.rm = TRUE) * 1.1, 
             label = paste0("N = ", n), 
             size = 1.5, hjust = 0.5) +
    scale_fill_manual(values = automatic_palette) +
    scale_color_manual(values = automatic_palette) +
    theme_classic() +
    theme(legend.position = "none")
}

scaled_shannon_diversity <- function(graph, node, mode = c("in", "out")) {
  mode <- match.arg(mode)
  
  # Get directed incident edges
  edges <- incident(graph, node, mode = mode)
  
  # Degree check: if degree < 2, return 0 
  ki <- length(edges)
  if (ki < 2) return(0)
  
  # Get weights (default to 1 if missing)
  weights <- E(graph)[edges]$weight
  if (is.null(weights)) weights <- rep(1, ki)
  
  # Normalize weights to probabilities
  p <- weights / sum(weights)
  p <- p[p > 0]  # avoid log(0)
  
  # Shannon entropy
  H <- -sum(p * log(p))
  
  # Scaled by log(ki)
  D <- H / log(ki)
  return(D)
}




plot_measure_facet <- function(df, measure, variables) {  
  # Convert data to long format for facetting  
  df_long <- df %>%  
    select(ppid, all_of(measure), all_of(variables)) %>%  
    pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Score")  
  
  # Compute p-values for each variable  
  p_values <- df_long %>%  
    group_by(Variable) %>%  
    summarise(p_value = summary(lm(Score ~ .data[[measure]], data = cur_data()))$coefficients[2,4])  
  
  # Annotate significance  
  p_values <- p_values %>%  
    mutate(Significance = case_when(  
      p_value < 0.001 ~ "***",  
      p_value < 0.01  ~ "**",  
      p_value < 0.05  ~ "*",  
      TRUE            ~ "ns"  
    ))  
  
  # Merge significance labels with long data  
  df_long <- df_long %>%  
    left_join(p_values, by = "Variable")  
  
  # Generate the facet grid plot  
  ggplot(df_long, aes_string(x = measure, y = "Score")) +  
    geom_point(alpha = 0.6) +  
    geom_smooth(method = "lm", se = TRUE, color = "blue") +  
    facet_wrap(~ paste0(Variable, " (p = ", signif(p_value, 3), ") ", Significance), scales = "free_y") +  
    labs(  
      title = paste("Scatter Plots of", measure, "vs Psychological Variables"),  
      x = measure,  
      y = "Score"  
    ) +  
    theme_minimal()  
}

# Function to create violin plot with paired t-test and annotations
plot_violin <- function(df, col_most, col_least) {
  # Filter complete cases for paired t-test
  df_complete <- df %>% filter(!is.na(.data[[col_most]]) & !is.na(.data[[col_least]]))
  
  # Convert to long format
  df_long <- df_complete %>%
    select(all_of(c(col_most, col_least))) %>%
    pivot_longer(cols = everything(), names_to = "Condition", values_to = "Score")
  
  # Rename conditions to match the palette keys
  df_long$Condition <- recode(df_long$Condition, 
                              !!col_most := "Most Automatic",
                              !!col_least := "Least Automatic")
  
  # Perform paired t-test
  t_test <- t.test(df_complete[[col_most]], df_complete[[col_least]], paired = TRUE)
  p_value <- t_test$p.value
  n <- nrow(df_complete)
  
  # Determine significance level
  significance <- case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    TRUE            ~ "ns"
  )
  
  # Generate plot
  ggplot(df_long, aes(x = Condition, y = Score, fill = Condition)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, outlier.shape = NA) +
    theme_minimal() +
    labs(title = str_c(col_most, "\nvs.\n", col_least),
         x = "Condition") +
    scale_fill_manual(values = automatic_palette) +
    # Annotate with p-value, significance, and N
    ggplot2::annotate("text", x = 1.5, y = max(df_long$Score, na.rm = TRUE) * 1.05, 
             label = str_c("Paired t-test: p = ", signif(p_value, 3), " (", significance, ")"), 
             size = 5, hjust = 0.5) +
    ggplot2::annotate("text", x = 1.5, y = max(df_long$Score, na.rm = TRUE) * 1.1, 
             label = str_c("N = ", n), 
             size = 4, hjust = 0.5)
}


plot_violin_facet <- function(df, variable_pairs) {
  plots <- lapply(variable_pairs, function(pair) {
    col_most <- pair[1]
    col_least <- pair[2]
    plot_violin(df, col_most, col_least) + ggtitle(paste(col_most, "vs", col_least))
  })
  
  # Arrange plots with 3 per row
  plot_layout <- wrap_plots(plots, ncol = 3)
  return(plot_layout)
}


plot_measure_facet_mlr <- function(df, measure1, measure2, variables) {
  # Convert data to long format for facetting
  df_long <- df %>%
    select(ppid, all_of(measure1), all_of(measure2), all_of(variables)) %>%
    pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Score")
  
  # Compute p-values for each variable with multiple linear regression
  p_values <- df_long %>%
    group_by(Variable) %>%
    summarise(
      model = list(lm(Score ~ .data[[measure1]] + .data[[measure2]], data = cur_data())),
      .groups = 'drop'
    ) %>%
    mutate(
      p_value1 = map_dbl(model, ~ summary(.x)$coefficients[2, 4]),
      p_value2 = map_dbl(model, ~ summary(.x)$coefficients[3, 4]),
      Significance1 = case_when(
        p_value1 < 0.001 ~ "***",
        p_value1 < 0.01  ~ "**",
        p_value1 < 0.05  ~ "*",
        TRUE            ~ "ns"
      ),
      Significance2 = case_when(
        p_value2 < 0.001 ~ "***",
        p_value2 < 0.01  ~ "**",
        p_value2 < 0.05  ~ "*",
        TRUE            ~ "ns"
      )
    ) %>%
    select(Variable, p_value1, Significance1, p_value2, Significance2)
  
  # Merge significance labels with long data
  df_long <- df_long %>%
    left_join(p_values, by = "Variable")
  
  # Generate the facet grid plot
  ggplot(df_long, aes_string(x = measure1, y = "Score", color = measure2)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    facet_wrap(~ paste0(Variable, " (", measure1, " p = ", signif(p_value1, 3), " ", Significance1, ", ", measure2, " p = ", signif(p_value2, 3), " ", Significance2, ")"), scales = "free_y") +
    labs(
      title = paste("Scatter Plots of", measure1, "and", measure2, "vs Psychological Variables"),
      x = measure1,
      y = "Score",
      color = measure2
    ) +
    theme_minimal()
}

plot_measure_facet <- function(df, measure, variables) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(broom)  # For extracting p-values
  
  # Convert data to long format for facetting
  df_long <- df %>%
    select(ppid, all_of(measure), all_of(variables)) %>%
    pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Score") %>%
    mutate(Variable = factor(Variable, levels = variables))  # Preserve order of variables
  
  # Compute p-values and significance stars for each variable
  p_values <- df_long %>%
    group_by(Variable) %>%
    summarise(
      p_value = broom::tidy(lm(Score ~ get(measure), data = cur_data()))$p.value[2]
    ) %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        p_value < 0.1   ~ ".",
        TRUE            ~ "ns"
      ),
      label = paste0("p = ", signif(p_value, 3), " ", significance)
    )
  
  # Generate the facet grid plot
  ggplot(df_long, aes_string(x = measure, y = "Score")) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    facet_wrap(~ Variable, scales = "free_y") +
    labs(
      title = paste("Scatter Plots of", measure, "vs Psychological Variables"),
      x = measure,
      y = "Score"
    ) +
    theme_minimal() +
    geom_text(
      data = p_values,
      aes(x = Inf, y = Inf, label = label),
      hjust = 1.1, vjust = 1.5,
      inherit.aes = FALSE
    )
}

plot_measure_facet1row <- function(df, measure, variables, color_palette ) {
  # Convert data to long format for facetting
  df_long <- df %>%
    select(ppid, all_of(measure), all_of(variables)) %>%
    pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Score") %>%
    mutate(Variable = factor(Variable, levels = variables))  # preserve order
  
  # Compute p-values for each variable
  p_values <- df_long %>%
    group_by(Variable) %>%
    summarise(p_value = summary(lm(Score ~ .data[[measure]], data = cur_data()))$coefficients[2, 4])
  
  # Annotate significance
  p_values <- p_values %>%
    mutate(Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ))
  
  # Merge significance labels with long data
  df_long <- df_long %>%
    left_join(p_values, by = "Variable")
  
  # Create a new ordered factor for display labels
  df_long <- df_long %>%
    mutate(Label = factor(paste0(as.character(Variable), 
                                 " (p = ", signif(p_value, 3), ") ", Significance),
                          levels = paste0(variables, 
                                          " (p = ", signif(p_values$p_value, 3), ") ", 
                                          p_values$Significance)))
  
  names(color_palette) <- variables
  
  # Generate the facet grid plot with a single row
  ggplot(df_long, aes_string(x = measure, y = "Score")) +
    geom_point(aes(color = Variable, stroke = NA), alpha = 0.6) +
    geom_smooth(aes(color = Variable, fill = Variable), method = "lm", se = TRUE) +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    facet_wrap(~ Label, scales = "free_y", nrow = 1) +
    labs(
      title = paste("Scatter Plots of", measure, "vs Psychological Variables"),
      x = measure,
      y = "Score"
    ) +
    theme_classic()+ 
    theme(
      axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12)
      
    )
}


plot_combined_apps <- function(df,
                               most_name_col, most_cat_col,
                               least_name_col, least_cat_col,
                               title = "Top Most & Least Automatic Apps") {
  library(forcats)
  library(rlang)
  library(RColorBrewer)
  
  # Color-safe palette
  okabe_ito <- c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#999999", "#000000"
  )
  
  # Capture columns
  most_name  <- enquo(most_name_col)
  most_cat   <- enquo(most_cat_col)
  least_name <- enquo(least_name_col)
  least_cat  <- enquo(least_cat_col)
  
  # Prep data for each group
  most_df <- df %>%
    transmute(name = !!most_name, category = !!most_cat, group = "Most Automatic")
  
  least_df <- df %>%
    transmute(name = !!least_name, category = !!least_cat, group = "Least Automatic")
  
  combined_df <- bind_rows(most_df, least_df)
  
  # Count frequencies
  counted <- combined_df %>%
    count(group, name, category)
  
  # Get top 10 names per group
  top_names <- counted %>%
    group_by(group, name) %>%
    summarise(total = sum(n), .groups = "drop") %>%
    group_by(group) %>%
    slice_max(total, n = 10, with_ties = FALSE)
  
  # Mark 'Other'
  counted <- counted %>%
    left_join(top_names, by = c("group", "name")) %>%
    mutate(name_grouped = ifelse(is.na(total), "Other", name)) %>%
    select(group, name_grouped, category, n)
  
  # Combine across categories per name
  grouped <- counted %>%
    group_by(group, name_grouped, category) %>%
    summarise(n = sum(n), .groups = "drop")
  
  # Mark 'Other' categories explicitly
  grouped <- grouped %>%
    mutate(category = ifelse(name_grouped == "Other", "Other", category))
  
  # SPLIT by group and handle category ordering and releveling independently
  process_group <- function(data, descending = TRUE) {
    # Order name_grouped
    data <- data %>%
      mutate(name_grouped = fct_reorder(name_grouped, n, .desc = descending)) %>%
      mutate(name_grouped = fct_relevel(name_grouped, "Other", after = Inf))
    
    # Order categories
    top_cats <- data %>%
      filter(name_grouped != "Other") %>%
      group_by(category) %>%
      summarise(total = sum(n), .groups = "drop") %>%
      arrange(desc(total)) %>%
      pull(category)
    
    data <- data %>%
      mutate(category = factor(category, levels = c(top_cats, "Other")))
    
    return(list(data = data, cats = top_cats))
  }
  
  most_proc <- process_group(grouped %>% filter(group == "Most Automatic"), descending = TRUE)
  least_proc <- process_group(grouped %>% filter(group == "Least Automatic"), descending = TRUE)

  grouped_final <- bind_rows(most_proc$data, least_proc$data)
  # Summarize 'Other' by group
  summed_others <- grouped_final %>%
    filter(name_grouped == "Other", group %in% c("Most Automatic", "Least Automatic")) %>%
    group_by(group) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    mutate(name_grouped = "Other")
  # Remove the original 'Other' rows for these groups
  grouped_final <- grouped_final %>%
    filter(!(name_grouped == "Other" & group %in% c("Most Automatic", "Least Automatic"))) %>%
    bind_rows(summed_others) %>%
    arrange(group, name_grouped)

  grouped_final <- grouped_final %>%
    group_by(group) %>%
    mutate(
      sort_order = case_when(
        name_grouped == "Other" ~ Inf,  # So "Other" always goes last
        TRUE ~ -n  # Sort others by descending n
      )
    ) %>%
    arrange(group, sort_order) %>%
    select(-sort_order) %>%  # Drop helper column
    ungroup()
  # Set factor levels of name_grouped by their order in the dataframe, within each group
  grouped_final <- grouped_final %>%
    group_by(group) %>%
    mutate(name_grouped = factor(name_grouped, levels = unique(name_grouped))) %>%
    ungroup()
  letters_prefix <- LETTERS[seq_along(unique(grouped_final$name_grouped))]
  letters_prefix <- rev(letters_prefix)
  
  grouped_final <- grouped_final %>%
    group_by(group) %>%
    mutate(name_grouped = paste0(
      letters_prefix[match(name_grouped, unique(name_grouped))],
      "_",
      name_grouped
    ))

  # Build palette (combine unique categories across groups)
  all_cats <- union(most_proc$cats, least_proc$cats)
  n_cats <- length(all_cats)
  
  if (n_cats <= length(okabe_ito)) {
    palette <- okabe_ito[seq_len(n_cats)]
  } else {
    palette <- RColorBrewer::brewer.pal(min(n_cats, 12), "Set3")
  }
  
  names(palette) <- all_cats
  palette <- c(palette, Other = "grey70")
  
  # Plot
  ggplot(grouped_final, aes(x = name_grouped, y = n, fill = category)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title = title,
      x = "App Name",
      y = "Frequency",
      fill = "Category"
    ) +
    theme_classic() +
    scale_fill_manual(values = palette)
  ggplot(grouped_final, aes(x = name_grouped, y = n, fill = category)) +
    geom_col() +
    geom_text(aes(label = n), 
              position = position_stack(vjust = 0.5), 
              hjust = -0.1, 
              size = 3) +  # adjust size as needed
    coord_flip() +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title = title,
      x = "App Name",
      y = "Frequency",
      fill = "Category"
    ) +
    theme_classic() +
    scale_fill_manual(values = palette) +
    # Expand limits to make room for text labels
    expand_limits(y = max(grouped_final$n) * 1.1)
  ggplot(grouped_final, aes(x = name_grouped, y = n, fill = category)) +
    geom_col() +
    geom_text(
      aes(label = n),
      position = position_stack(vjust = 1),
      hjust = -0.5,
      size = 3.8
    ) +
    coord_flip() +
    facet_wrap(~ group, scales = "free_y") +
    labs(
      title = title,
      x = "App Name",
      y = "Frequency",
      fill = "Category"
    ) +
    theme_classic() +
    scale_fill_manual(values = palette) +
    expand_limits(y = max(grouped_final$n) * 1.15)
  
}

find_outliers <- function(data, variable_name, id_col = "ppid") {
  # Extract the variable as a vector
  variable <- data[[variable_name]]
  
  # Compute quartiles and IQR
  Q1 <- quantile(variable, 0.25, na.rm = TRUE)
  Q3 <- quantile(variable, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  # Compute bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identify outliers
  outliers <- variable < lower_bound | variable > upper_bound
  
  # Return IDs of outlier participants
  outlier_ids <- data[[id_col]][outliers]
  
  # Return as a list for reuse
  return(list(
    variable = variable_name,
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR_value,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    outlier_ids = outlier_ids
  ))
}


export_results_to_word <- function(formatted_df, filename = "Table2.docx") {
  
  # Make flextable
  ft <- flextable(formatted_df) |>
    autofit() |>
    theme_box() |>
    bg(i = seq(2, nrow(formatted_df), 2), bg = "#f9f9f9", part = "body") |>
    fontsize(size = 10, part = "all") |>
    fontsize(size = 12, part = "header") |>
    bold(part = "header") |>
    align(j = c("Beta", "StdError", "R2", "p_value", "BF"), align = "right", part = "all") |>
    align(j = c("Outcome", "ModelType", "sig", "BF_interpretation"), align = "center", part = "all")
  
  # Create Word doc
  doc <- read_docx() |>
    body_add_par("Table 2. Global network features predicting questionnaire scores", style = "heading 1") |>
    body_add_flextable(ft) |>
    body_add_par(
      "Note. Beta = standardized regression coefficient. Significance: * p < .05, ** p < .01, *** p < .001. BF = Bayes factor for predictor vs control-only model.",
      style = "Normal"
    )
  
  print(doc, target = filename)
  message("Word document saved as: ", filename)
}

