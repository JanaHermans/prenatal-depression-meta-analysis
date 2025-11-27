################################################################################
# Title:    Descriptives included participants
# Project:  ECCN project 
# Author:   Jana Hermans
# Date:     Created in June 2025
################################################################################
library(metafor)
library(meta)
library(grid)
library(ggplot2)
library(tidyverse)
library(gt)
library(patchwork)
library(dplyr)
library(cowplot)
library(ggpubr)
library(flextable)
library(officer)

projectdir <- '/Users/janahermans/Documents/PhD/02_project/'
setwd(file.path(projectdir,"results"))
outdir <- paste0(projectdir,"/results/tables")

# Load data
samples <- readRDS("./ECCN/samples_ECCN.RData")
flowchart <- readRDS("./ECCN/flowchart_ECCN.RData")

descriptives <- readRDS("./ECCN/descriptives_ECCN_included.RData")
descriptives_ALSPAC <- readRDS("./ALSPAC_results/descriptives_ALSPAC.RData")
descriptives_PREDO <- readRDS("./PREDOresultsandscript/descriptives_PREDO.RData")

cohorts <- sort(c("genr", "dnbc", "bib", "ninfea", "eden", "abcd"))
#--------------------------------CHARACTERISTICS--------------------------------
make_descriptives_table <- function(samples, descriptives, cohorts) {
  
  get_n_pct_total <- function(cohort) {
    n_val_name <- paste0("dimensions of df_complete_w in ", cohort)
    n_val <- descriptives[["study_sample_N"]][[n_val_name]][1]
    total_val_name <- paste0("dimensions of core_nonrep in ", cohort)
    total_val <- descriptives[["total_sample_N"]][[total_val_name]][1]
    pct <- if (total_val > 0) round(n_val / total_val * 100, 1) else NA
    sprintf("%d (%.1f)", n_val, pct)
  }
  
  get_count_pct <- function(data, cohort, codes = "1") {
    col <- data[, cohort, drop = FALSE]
    count <- sum(col[rownames(col) %in% codes, ], na.rm = TRUE)
    total <- as.numeric(descriptives$study_sample_N[[paste0("dimensions of df_complete_w in ", cohort)]][1])
    pct <- if (total > 0) round(count / total * 100, 1) else NA
    sprintf("%d (%.1f)", count, pct)
  }
  
  get_count_pct_with_missing <- function(data, cohort, codes = "1") {
    # Extract the named vector for the cohort
    vec <- data[, cohort]
    
    # Ensure names are treated as character (not factors)
    names_vec <- rownames(data)
    
    # Count for codes of interest
    count <- sum(vec[names_vec %in% codes], na.rm = TRUE)
    
    # Missing = value where rowname == "NA"
    n_missing <- if ("NA" %in% names_vec) vec["NA"] else 0
    n_missing <- ifelse(is.na(n_missing), 0, n_missing)
    
    # Available = everything except "NA"
    total <- sum(vec, na.rm = TRUE)
    n_available <- total - n_missing
    total_all <- n_available + n_missing
    
    # Percentages
    pct <- if (n_available > 0) round(count / n_available * 100, 1) else NA
    miss_pct <- if (total_all > 0) round(n_missing / total_all * 100, 1) else NA
    
    sprintf("%d (%.1f%%, missing: %.1f%%)", count, pct, miss_pct)
  }
  
  get_mean_sd <- function(var_list, cohort) {
    mean_val <- var_list$mean[[cohort]]
    sd_val <- var_list$SD[[cohort]]
    if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
    sprintf("%.1f (%.1f)", mean_val, sd_val)
  }
  
  # Define row structure with section headers and variables
  row_labels <- c(
    "n (% of original sample)",
    "Child characteristics",
    "Assigned sex, male, n (%)",
    "Maternal characteristics",
    "Maternal age at childbirth, years, mean (SD)",
    "Mother born abroad, yes, n (%)",
    "High maternal education level, n (%)",
    "Prenatal maternal depression, n (%)",
    "Postnatal maternal depression, n (%)",
    "Pre-pregnancy depression, n (%)",
    "Maternal depression at more than one time points, n (%)",
    "Any alcohol use in pregnancy, yes, n (%)",
    "Any smoking in pregnancy, yes, n (%)",
    "Pre-pregnancy BMI, mean (SD)"
  )
  
  results <- matrix(NA_character_, nrow = length(row_labels), ncol = length(cohorts),
                    dimnames = list(row_labels, cohorts))
  
  for (cohort in cohorts) {
    results["n (% of original sample)", cohort] <- get_n_pct_total(cohort)
    results["Assigned sex, male, n (%)", cohort] <- get_count_pct(descriptives$child_sex, cohort, "1")
    results["Maternal age at childbirth, years, mean (SD)", cohort] <- get_mean_sd(descriptives$agebirth_m_y, cohort)
    
    if (cohort %in% colnames(descriptives$cob_other_country_f)) {
      results["Mother born abroad, yes, n (%)", cohort] <- get_count_pct_with_missing(descriptives$cob_other_country_f, cohort, "0")
    }
    
    results["High maternal education level, n (%)", cohort] <- get_count_pct(descriptives$edu_m_.0, cohort, "1")
    
    results["Prenatal maternal depression, n (%)", cohort] <- get_count_pct(descriptives$preg_dep, cohort, "1")
    
    # Use the updated function with missing % for depression variables
    results["Postnatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing(descriptives$ppd, cohort, "1")
    
    if (cohort %in% colnames(descriptives$prepreg_dep)) {
      results["Pre-pregnancy depression, n (%)", cohort] <- get_count_pct_with_missing(descriptives$prepreg_dep, cohort, "1")
    }
    
    if (cohort %in% colnames(descriptives$cumul_dep_weighted)) {
      results["Maternal depression at more than one time points, n (%)", cohort] <- 
        get_count_pct_with_missing(descriptives$cumul_dep_weighted, cohort, c("2", "3"))
    }
    
    results["Any alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct(descriptives$preg_alc, cohort, "1")
    results["Any smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct(descriptives$preg_smk, cohort, "1")
    results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd(descriptives$prepreg_BMI, cohort)
  }
  
  as.data.frame(results, stringsAsFactors = FALSE)
}

# --- Generate and format table ---
cohorts <- sort(c("genr", "dnbc", "bib", "ninfea", "eden", "abcd"))
desc_table <- make_descriptives_table(samples, descriptives, cohorts)

library(flextable)
library(officer)

# Add rownames as a column
desc_table$Variable <- rownames(desc_table)
desc_table <- desc_table[, c("Variable", cohorts)]

# Build flextable
ft <- flextable(desc_table)

# Set font and size (this must come AFTER creating the flextable)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 7, part = "all")

# Highlight section headers
section_rows <- which(desc_table$Variable %in% c(
  "Child characteristics", "Maternal characteristics"
))
ft <- bold(ft, i = section_rows)
ft <- italic(ft, i = section_rows)

# Blank out section header cells
for (cohort in cohorts) {
  ft <- compose(ft, i = section_rows, j = cohort, value = as_paragraph(""))
}

# Shrink columns and padding
ft <- autofit(ft)
ft <- width(ft, width = 0.8)  # adjust this as needed
ft <- padding(ft, padding = 1, part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")

# Export to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = paste0(projectdir,"/results/tables/descriptives_included_table.docx"))

#------------------------------------OUTCOMES-----------------------------------
make_combined_table <- function(samples, descriptives, cohorts, prefixes) {
  
  # Helper to get n (%) with outcome
  get_n_pct <- function(prefix, cohort) {
    # Special cases: asd only genr, wm only bib
    if ((prefix == "asd" && cohort != "genr") || (prefix == "wm" && cohort != "bib")) {
      return(NA_character_)
    }
    
    n_name <- paste0("length of unique_id_", prefix, " in ", cohort)
    total_name <- paste0("dimensions of df_complete_w in ", cohort)
    
    n_val <- samples[[paste0("N_", prefix)]][[n_name]]
    if (length(n_val) > 1) n_val <- n_val[1]
    
    total_val <- descriptives$study_sample_N[[total_name]]
    if (length(total_val) > 1) total_val <- total_val[1]
    
    if (is.null(n_val) || is.null(total_val) || total_val == 0) {
      return(NA_character_)
    } else {
      pct <- round(100 * n_val / total_val, 1)
      return(sprintf("%d (%.1f)", n_val, pct))
    }
  }
  
  # Helper to get mean (SD)
  get_mean_sd <- function(var_list, cohort, prefix) {
    # Handle missing var_list
    if (is.null(var_list)) return(NA_character_)
    
    # Special logic for asd and wm variables without cohort indexing
    if ((prefix %in% c("asd", "wm"))) {
      # Only genr for asd, only bib for wm
      target_cohort <- ifelse(prefix == "asd", "genr", "bib")
      if (cohort != target_cohort) return(NA_character_)
      
      mean_val <- var_list$mean
      sd_val <- var_list$SD
      if (length(mean_val) > 1) mean_val <- mean_val[1]
      if (length(sd_val) > 1) sd_val <- sd_val[1]
      
      if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
      return(sprintf("%.1f (%.1f)", mean_val, sd_val))
    }
    
    # Normal case - cohort indexed
    if (!(cohort %in% names(var_list$mean)) || !(cohort %in% names(var_list$SD))) {
      return(NA_character_)
    }
    
    mean_val <- var_list$mean[[cohort]]
    sd_val <- var_list$SD[[cohort]]
    
    if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
    sprintf("%.1f (%.1f)", mean_val, sd_val)
  }
  
  all_rows <- list()
  
  for (prefix in prefixes) {
    n_pct_row <- sapply(cohorts, function(cohort) get_n_pct(prefix, cohort))
    
    age_var_name <- paste0(prefix, "_age")
    age_list <- descriptives[[age_var_name]]
    age_row <- sapply(cohorts, function(cohort) {
      get_mean_sd(age_list, cohort, prefix)
    })
    
    pc_var_name <- paste0(prefix, "_pc")
    pc_list <- descriptives[[pc_var_name]]
    pc_row <- sapply(cohorts, function(cohort) {
      get_mean_sd(pc_list, cohort, prefix)
    })
    
    prefix_label <- switch(prefix,
                           int = "Internalising",
                           ext = "Externalising",
                           adhd = "ADHD",
                           asd = "ASD",
                           fm = "FM",
                           gm = "GM",
                           lan = "LAN",
                           nvi = "NVI",
                           wm = "WM",
                           prefix)  # fallback to prefix if not matched
    
    rows_mat <- rbind(n_pct_row, age_row, pc_row)
    rownames(rows_mat) <- c(
      "n (%) with outcome",
      paste0("Age at ", tolower(prefix_label), " measurement, years, mean (SD)"),
      paste0(prefix_label, " percentile score, mean (SD)")
    )
    
    all_rows[[prefix]] <- rows_mat
  }
  
  combined_table <- do.call(rbind, all_rows)
  combined_table_df <- as.data.frame(combined_table, stringsAsFactors = FALSE)
  colnames(combined_table_df) <- cohorts
  
  return(combined_table_df)
}

cohorts <- sort(c("genr", "dnbc", "bib", "ninfea", "eden", "abcd"))
prefixes <- c("int", "ext", "adhd", "asd", "fm", "gm", "lan", "nvi", "wm")

combined_table <- make_combined_table(samples, descriptives, cohorts, prefixes)
print(combined_table)

# Suppose combined_table is your data.frame
ft <- flextable(combined_table)

doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = paste0(projectdir,"/results/tables/descriptives_outcomes_table.docx"))


