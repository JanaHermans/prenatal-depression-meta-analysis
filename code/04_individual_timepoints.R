################################################################################
# Title:    Meta-analyses inidividual depression time points
# Project:  ECCN project 
# Author:   Jana Hermans
# Date:     Created in February 2025
################################################################################
library(metafor)
library(meta)
library(grid)
library(ggplot2)
library(tidyverse)
library(gt)
library(patchwork)
library(dplyr)
library(ggpubr)
library(flextable)
library(officer)

projectdir <- '/Users/janahermans/Documents/PhD/02_project/'
setwd(file.path(projectdir,"results")) 
outdir <- paste0(projectdir,"results/tables")
#ABCD, EDEN, and PREDO have no prepreg depression data
#--------------------------------PRE: Data prep---------------------------------
cohort_levels <- c("Cohort", "ALSPAC", "DNBC", "Generation R", "NINFEA", "TOTAL")
outcomes <- c('int_', 'ext_', 'adhd_', 'asd_')
results_list <- list()
ALSPAC_samplesize <- readRDS('./ALSPAC_results/descriptives_ALSPAC.RData')

load_results <- function(path, outcome, cohort_name) {
  df <- read.csv(path)
  df <- df[df$outcome == outcome, ]
  df$cohort <- cohort_name
  df <- df[c("cohort", "Estimate", "Std..Error", "samplesize")]
  colnames(df) <- c("cohort", "preg_dep_betas", "ses", "N")
  if (cohort_name == 'alspac') {
    df$N <- ALSPAC_samplesize[[paste0("N_",outcome,"pc_CC")]]
  }
  return(df)
}

for (outcome in outcomes) {
  # Load ECCN results
  MAIN_results <- read.csv(paste0("ECCN/results_", outcome, "pre_indivi_ECCN.csv"))
  names(MAIN_results)[1] <- "cohort"
  colnames(MAIN_results)[colnames(MAIN_results) == "Estimate"] <- "preg_dep_betas"
  colnames(MAIN_results)[colnames(MAIN_results) == "Std..Error"] <- "ses"

  # Determine which cohorts to add
  if (outcome %in% c('int_', 'ext_', 'adhd_')) {
    MAIN_results <- rbind(MAIN_results,
                          load_results("ALSPAC_results/results_prepregind_model_ALSPAC.csv", outcome, "alspac"))
  } else if (outcome %in% c('fm_', 'gm_', 'lan_', 'nvi_', 'wm_')) {
    MAIN_results <- rbind(MAIN_results,
                          load_results("ALSPAC_results/results_prepregind_model_ALSPAC.csv", outcome, "alspac"))
  } else if (outcome == 'asd_') {
    MAIN_results <- MAIN_results
  }
  
  # Final formatting
  MAIN_results$preg_dep_betas <- as.numeric(MAIN_results$preg_dep_betas)
  MAIN_results$ses <- as.numeric(MAIN_results$ses)
  MAIN_results$cohort <- toupper(MAIN_results$cohort)
  if (outcome != "wm_") {
    MAIN_results$cohort[MAIN_results$cohort == "GENR"] <- "Generation R"
  }
  
  # Run meta-analysis
  m.model2 <- rma(yi = MAIN_results$preg_dep_betas, sei = MAIN_results$ses)
  coefs <- coef(summary(m.model2))
  
  # Add confidence intervals
  MAIN_results <- MAIN_results %>%
    mutate(conf.low = preg_dep_betas - 1.96 * ses,
           conf.high = preg_dep_betas + 1.96 * ses,
           estimate = preg_dep_betas)
  
  MAIN_results$cohort <- factor(MAIN_results$cohort,
                                levels = c("ALSPAC", "DNBC", "EDEN",
                                           "Generation R", "NINFEA"))
  
  # Add meta-analysis result
  MAIN_results$N <- as.character(MAIN_results$N)
  cohorts_to_sum <- nrow(MAIN_results)
  MAIN_results <- bind_rows(MAIN_results,
                            data.frame(cohort = "TOTAL",
                                       estimate = m.model2$beta,
                                       conf.low = m.model2$ci.lb,
                                       conf.high = m.model2$ci.ub,
                                       N = as.character(sum(
                                         as.numeric(
                                           MAIN_results$N[1:cohorts_to_sum])))
                            ))
  
  # Store results
  base <- gsub("_", "", outcome)
  results_list[[outcome]] <- MAIN_results
  results_list[[paste0(base, "_I2")]] <- round(m.model2$I2, 2)
  results_list[[paste0(base, "_tau2")]] <- round(m.model2$tau2, 2)
  results_list[[paste0(base, "_Q_pval")]] <- round(m.model2$QEp, 2)
  results_list[[paste0(base, "_pvalue")]] <- coefs$pval
}

#--------------------------PRE: Save results to table---------------------------
cohorts <- c("ALSPAC", "DNBC", "Generation R", "NINFEA", "TOTAL")

format_result <- function(df, cohorts) {
  df <- df %>%
    select(-ses) %>%
    mutate(
      N = N,
      `β` = round(estimate, 2),
      `95% CI` = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]")
    ) %>%
    select(Cohort = cohort, N, `β`, `95% CI`)
  
  # Ensure all cohorts are included
  master_cohorts <- data.frame(Cohort = cohorts)
  df <- left_join(master_cohorts, df, by = "Cohort")
  
  return(df)
}

combine_results_side_by_side <- function(results_list, outcome_names, cohorts) {
  formatted_dfs <- list()
  
  for (outcome in outcome_names) {
    if (!is.null(results_list[[outcome]])) {
      df <- format_result(results_list[[outcome]], cohorts)
      formatted_dfs[[outcome]] <- df
    }
  }
  
  # Combine side by side, excluding Cohort from all but the first
  combined_df <- do.call(cbind, lapply(formatted_dfs, function(x) x[, -1])) 
  combined_df <- cbind(formatted_dfs[[1]][, 1, drop = FALSE], combined_df)
  
  return(combined_df)
}

create_combined_flextable <- function(combined_df, outcome_names) {
  combined_df <- as.data.frame(combined_df)
  
  ft <- flextable(combined_df)
  
  ft <- set_header_labels(ft, values = setNames(rep("", ncol(combined_df)), names(combined_df)))
  
  colwidths <- c(1, rep(3, length(outcome_names)))  # Each outcome now has 3 columns
  ft <- add_header_row(ft, values = c("Cohort", outcome_names), colwidths = colwidths)
  
  ft <- ft %>%
    fontsize(size = 7.5, part = "all") %>%
    set_table_properties(layout = "autofit")
  
  last_row <- nrow(combined_df)
  ft <- bold(ft, i = last_row, part = "body")
  
  return(ft)
}

write_combined_results_to_word <- function(results_list, filename = "prepreg_ind_results_table.docx", cohorts) {
  outcome_names <- c("int_", "ext_", "adhd_", "asd_")
  combined_df <- combine_results_side_by_side(results_list, outcome_names, cohorts)
  ft <- create_combined_flextable(combined_df, outcome_names)
  doc <- read_docx() %>%
    body_add_flextable(ft)
  print(doc, target = filename)
}

write_combined_results_to_word(results_list, file.path(outdir, "prepreg_ind_model_combined_results.docx"), cohorts)

#--------------------------------PPD: Data prep---------------------------------
cohort_levels <- c("Cohort", "ABCD", "ALSPAC", "DNBC", "EDEN", "Generation R", "NINFEA", "PREDO", "TOTAL")
outcomes <- c('int_', 'ext_', 'adhd_', 'asd_')
results_list <- list()
ALSPAC_samplesize <- readRDS('./ALSPAC_results/descriptives_ALSPAC.RData')

# Helper function to load and format data
load_results <- function(path, outcome, cohort_name) {
  df <- read.csv(path)
  df <- df[df$outcome == outcome, ]
  df$cohort <- cohort_name
  df <- df[c("cohort", "Estimate", "Std..Error", "samplesize")]
  colnames(df) <- c("cohort", "preg_dep_betas", "ses", "N")
  if (cohort_name == 'alspac') {
    df$N <- ALSPAC_samplesize[[paste0("N_",outcome,"pc_CC")]]
  }
  return(df)
}

for (outcome in outcomes) {
  # Determine which cohorts to add
  if (outcome %in% c('int_', 'ext_', 'adhd_')) {
    # Load ECCN results
    MAIN_results <- read.csv(paste0("ECCN/results_", outcome, "ppd_indivi_ECCN.csv"))
    names(MAIN_results)[1] <- "cohort"
    colnames(MAIN_results)[colnames(MAIN_results) == "Estimate"] <- "preg_dep_betas"
    colnames(MAIN_results)[colnames(MAIN_results) == "Std..Error"] <- "ses"
    MAIN_results <- rbind(MAIN_results,
                          load_results("ALSPAC_results/results_ppdind_model_ALSPAC.csv", outcome, "alspac"),
                          load_results("PREDOresultsandscript/results_ppdind_model_PREDO.csv", outcome, "predo"))
  } else if (outcome %in% c('wm_')) {
    MAIN_results <- load_results("ALSPAC_results/results_ppdind_model_ALSPAC.csv", outcome, "alspac")
  } else if (outcome == 'asd_') {
    # Load ECCN results
    MAIN_results <- read.csv(paste0("ECCN/results_", outcome, "ppd_indivi_ECCN.csv"))
    names(MAIN_results)[1] <- "cohort"
    colnames(MAIN_results)[colnames(MAIN_results) == "Estimate"] <- "preg_dep_betas"
    colnames(MAIN_results)[colnames(MAIN_results) == "Std..Error"] <- "ses"
    MAIN_results <- rbind(MAIN_results,
                          load_results("PREDOresultsandscript/results_ppdind_model_PREDO.csv", outcome, "predo"))
  }
  
  # Final formatting
  MAIN_results$preg_dep_betas <- as.numeric(MAIN_results$preg_dep_betas)
  MAIN_results$ses <- as.numeric(MAIN_results$ses)
  MAIN_results$cohort <- toupper(MAIN_results$cohort)
  if (outcome != "wm_") {
    MAIN_results$cohort[MAIN_results$cohort == "GENR"] <- "Generation R"
  }
  
  # Run meta-analysis
  m.model2 <- rma(yi = MAIN_results$preg_dep_betas, sei = MAIN_results$ses)
  coefs <- coef(summary(m.model2))
  
  # Add confidence intervals
  MAIN_results <- MAIN_results %>%
    mutate(conf.low = preg_dep_betas - 1.96 * ses,
           conf.high = preg_dep_betas + 1.96 * ses,
           estimate = preg_dep_betas)
  
  MAIN_results$cohort <- factor(MAIN_results$cohort,
                                levels = c("ABCD", "ALSPAC", "DNBC", "EDEN",
                                           "Generation R", "NINFEA", "PREDO"))
  
  # Add meta-analysis result
  MAIN_results$N <- as.character(MAIN_results$N)
  cohorts_to_sum <- nrow(MAIN_results)
  MAIN_results <- bind_rows(MAIN_results,
                            data.frame(cohort = "TOTAL",
                                       estimate = m.model2$beta,
                                       conf.low = m.model2$ci.lb,
                                       conf.high = m.model2$ci.ub,
                                       N = as.character(sum(
                                         as.numeric(
                                           MAIN_results$N[1:cohorts_to_sum])))
                            ))
  
  # Store results
  base <- gsub("_", "", outcome)
  results_list[[outcome]] <- MAIN_results
  results_list[[paste0(base, "_I2")]] <- round(m.model2$I2, 2)
  results_list[[paste0(base, "_tau2")]] <- round(m.model2$tau2, 2)
  results_list[[paste0(base, "_Q_pval")]] <- round(m.model2$QEp, 2)
  results_list[[paste0(base, "_pvalue")]] <- coefs$pval
}
#--------------------------PPD: Save results to table---------------------------
cohorts <- c("ABCD", "ALSPAC", "DNBC", "EDEN", "Generation R", "NINFEA", "PREDO", "TOTAL")

format_result <- function(df, cohorts) {
  df <- df %>%
    select(-ses) %>%
    mutate(
      N = N,
      `β` = round(estimate, 2),
      `95% CI` = paste0("[", round(conf.low, 2), ", ", round(conf.high, 2), "]")
    ) %>%
    select(Cohort = cohort, N, `β`, `95% CI`)
  
  # Ensure all cohorts are included
  master_cohorts <- data.frame(Cohort = cohorts)
  df <- left_join(master_cohorts, df, by = "Cohort")
  
  return(df)
}

combine_results_side_by_side <- function(results_list, outcome_names, cohorts) {
  formatted_dfs <- list()
  
  for (outcome in outcome_names) {
    if (!is.null(results_list[[outcome]])) {
      df <- format_result(results_list[[outcome]], cohorts)
      formatted_dfs[[outcome]] <- df
    }
  }
  
  # Combine side by side, excluding Cohort from all but the first
  combined_df <- do.call(cbind, lapply(formatted_dfs, function(x) x[, -1])) 
  combined_df <- cbind(formatted_dfs[[1]][, 1, drop = FALSE], combined_df)
  
  return(combined_df)
}

create_combined_flextable <- function(combined_df, outcome_names) {
  combined_df <- as.data.frame(combined_df)
  
  ft <- flextable(combined_df)
  
  ft <- set_header_labels(ft, values = setNames(rep("", ncol(combined_df)), names(combined_df)))
  
  colwidths <- c(1, rep(3, length(outcome_names)))  # Each outcome now has 3 columns
  ft <- add_header_row(ft, values = c("Cohort", outcome_names), colwidths = colwidths)
  
  ft <- ft %>%
    fontsize(size = 7.5, part = "all") %>%
    set_table_properties(layout = "autofit")
  
  last_row <- nrow(combined_df)
  ft <- bold(ft, i = last_row, part = "body")
  
  return(ft)
}

write_combined_results_to_word <- function(results_list, filename = "ppd_ind_results_table.docx", cohorts) {
  outcome_names <- c("int_", "ext_", "adhd_", "asd_")
  combined_df <- combine_results_side_by_side(results_list, outcome_names, cohorts)
  ft <- create_combined_flextable(combined_df, outcome_names)
  doc <- read_docx() %>%
    body_add_flextable(ft)
  print(doc, target = filename)
}

write_combined_results_to_word(results_list, file.path(outdir, "ppd_ind_model_combined_results.docx"), cohorts)
