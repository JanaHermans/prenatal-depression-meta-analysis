################################################################################
# Title:    Descriptives excluded sample 
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

descriptives <- readRDS("./ECCN/descriptives_ECCN_excluded.RData")
descriptives_ALSPAC <- readRDS("./ALSPAC_results/descriptives_with_excl_ALSPAC.RData")
descriptives_PREDO <- readRDS("./PREDOresultsandscript/descriptives_with_excl_revisedPREDO.RData")

total_samplesize <- descriptives[["study_sample_N"]][2,]+descriptives[["study_sample_N"]][1,]

cohorts <- sort(c("genr", "dnbc", "bib", "ninfea", "eden", "abcd"))
#--------------------------------CHARACTERISTICS--------------------------------
make_descriptives_table <- function(samples, descriptives, cohorts) {
  
  # Function for sample size and % of total
  get_n_pct_total <- function(cohort) {
    n_val <- descriptives[["study_sample_N"]][1,][[cohort]]
    total_val <- total_samplesize[[cohort]]
    pct <- if (total_val > 0) round(n_val / total_val * 100, 1) else NA
    sprintf("%d (%.1f)", n_val, pct)
  }
  
  # Function for count and percent with missingness
  get_count_pct_with_missing <- function(data, cohort, codes = "1") {
    vec <- data[, cohort]
    names_vec <- rownames(data)
    
    count <- sum(vec[names_vec %in% codes], na.rm = TRUE)
    
    n_missing <- if ("NA" %in% names_vec) vec["NA"] else 0
    n_missing <- ifelse(is.na(n_missing), 0, n_missing)
    
    total <- sum(vec, na.rm = TRUE)
    n_available <- total - n_missing
    total_all <- n_available + n_missing
    
    pct <- if (n_available > 0) round(count / n_available * 100, 1) else NA
    miss_pct <- if (total_all > 0) round(n_missing / total_all * 100, 1) else NA
    
    sprintf("%d (%.1f%%, missing: %.1f%%)", count, pct, miss_pct)
  }
  
  # Function for mean and SD
  get_mean_sd <- function(var_list, cohort) {
    mean_val <- var_list$mean[[cohort]]
    sd_val <- var_list$SD[[cohort]]
    if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
    sprintf("%.1f (%.1f)", mean_val, sd_val)
  }
  
  # Labels
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
  
  # Results matrix
  results <- matrix(NA_character_, nrow = length(row_labels), ncol = length(cohorts),
                    dimnames = list(row_labels, cohorts))
  
  # Loop through cohorts
  for (cohort in cohorts) {
    results["n (% of original sample)", cohort] <- get_n_pct_total(cohort)
    
    results["Assigned sex, male, n (%)", cohort] <- get_count_pct_with_missing(descriptives$child_sex, cohort, "1")
    
    results["Maternal age at childbirth, years, mean (SD)", cohort] <- get_mean_sd(descriptives$agebirth_m_y, cohort)
    
    if (cohort %in% colnames(descriptives$cob_other_country_f)) {
      results["Mother born abroad, yes, n (%)", cohort] <- get_count_pct_with_missing(descriptives$cob_other_country_f, cohort, "0")
    }
    
    results["High maternal education level, n (%)", cohort] <- get_count_pct_with_missing(descriptives$edu_m_.0, cohort, "1")
    
    results["Prenatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing(descriptives$preg_dep, cohort, "1")
    
    results["Postnatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing(descriptives$ppd, cohort, "1")
    
    if (cohort %in% colnames(descriptives$prepreg_dep)) {
      results["Pre-pregnancy depression, n (%)", cohort] <- get_count_pct_with_missing(descriptives$prepreg_dep, cohort, "1")
    }
    
    if (cohort %in% colnames(descriptives$cumul_dep_weighted)) {
      results["Maternal depression at more than one time points, n (%)", cohort] <- 
        get_count_pct_with_missing(descriptives$cumul_dep_weighted, cohort, c("2", "3"))
    }
    
    results["Any alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing(descriptives$preg_alc, cohort, "1")
    
    results["Any smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing(descriptives$preg_smk, cohort, "1")
    
    results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd(descriptives$prepreg_BMI, cohort)
  }
  
  as.data.frame(results, stringsAsFactors = FALSE)
}

# Function to get count and percent with missingness from _NC descriptives
# Updated function: accept codes vector for "positive" values, calculate percentage properly
get_count_pct_with_missing_nc <- function(data, codes = "1") {
  vec <- data$distribution
  names_vec <- names(vec)
  
  # Count how many fall in the "positive" category
  count <- sum(vec[names_vec %in% codes], na.rm = TRUE)
  
  # Missing count (if provided)
  n_missing <- if ("missing" %in% names(data)) data$missing else 0
  n_missing <- ifelse(is.na(n_missing), 0, n_missing)
  
  # Total count of all known (non-missing) categories
  total_available <- sum(vec, na.rm = TRUE)
  
  # Calculate percentage of count relative to total available (excluding missing)
  pct <- if (total_available > 0) round(count / total_available * 100, 1) else NA
  miss_pct <- if ((total_available + n_missing) > 0) round(n_missing / (total_available + n_missing) * 100, 1) else NA
  
  sprintf("%d (%.1f%%, missing: %.1f%%)", count, pct, miss_pct)
}

# Function to format mean (SD) for _NC descriptives, with optional divisor
get_mean_sd_nc <- function(var_list, divide_factor = 1) {
  mean_val <- var_list$mean
  sd_val <- var_list$SD
  if (is.na(mean_val) || is.na(sd_val)) return(NA_character_)
  mean_val <- mean_val / divide_factor
  sd_val <- sd_val / divide_factor
  sprintf("%.2f (%.2f)", mean_val, sd_val)
}

# Main function to create descriptives table for ALSPAC and PREDO _NC data
make_descriptives_table_nc <- function(descriptives_list, cohort_names) {
  
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
  
  results <- matrix(NA_character_, nrow = length(row_labels), ncol = length(cohort_names),
                    dimnames = list(row_labels, cohort_names))
  
  for (i in seq_along(cohort_names)) {
    cohort <- cohort_names[i]
    descriptives <- descriptives_list[[i]]
    
    total_sample <- descriptives[["full_sample_N"]]
    study_sample <- descriptives[["study_sample_N_NC"]]
    pct <- if (!is.na(total_sample) && total_sample > 0) round(study_sample / total_sample * 100, 1) else NA
    results["n (% of original sample)", cohort] <- sprintf("%d (%.1f%%)", study_sample, pct)
    
    results["Assigned sex, male, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["child_sex_NC"]], "1")
    
    results["Maternal age at childbirth, years, mean (SD)", cohort] <- get_mean_sd_nc(descriptives[["agebirth_m_y_NC"]])
    
    # For country of birth, count those born outside country = "0"
    results["Mother born abroad, yes, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["cob_other_country_f_NC"]], "0")
    
    results["Prenatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["preg_dep_NC"]], "1")
    
    results["Postnatal maternal depression, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["ppd_NC"]], "1")
    
    results["Pre-pregnancy depression, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["prepreg_dep_NC"]], "1")
    
    results["Maternal depression at more than one time points, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["cumul_dep_NC"]], c("2", "3"))
    
    # PREDO specific fixes
    if (cohort == "predo") {
      # preg_alc labels switched - count "0" instead of "1"
      results["Any alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["preg_alc_NC"]], "0")
      # prepreg_BMI divided by 10000
      #results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd_nc(descriptives[["prepreg_BMI_NC"]], divide_factor = 10000)
      # education is reverse coded:
      results["High maternal education level, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["edu_m_0_NC"]], "3")
    } else {
      results["Any alcohol use in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["preg_alc_NC"]], "1")
      #results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd_nc(descriptives[["prepreg_BMI_NC"]])
      results["High maternal education level, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["edu_m_0_NC"]], "1")
    }
    
    results["Any smoking in pregnancy, yes, n (%)", cohort] <- get_count_pct_with_missing_nc(descriptives[["preg_smk_NC"]], "1")
    
    results["Pre-pregnancy BMI, mean (SD)", cohort] <- get_mean_sd_nc(descriptives[["prepreg_BMI_NC"]])
  }
  
  as.data.frame(results, stringsAsFactors = FALSE)
}


# --- Generate and format table ---
# Your main cohorts (excluding alspac and predo)
main_cohorts <- sort(c("genr", "dnbc", "bib", "ninfea", "eden", "abcd"))

# Your NC cohorts
nc_cohorts <- c("alspac", "predo")

# Generate tables
desc_table <- make_descriptives_table(samples, descriptives, main_cohorts)
desc_nc_table <- make_descriptives_table_nc(
  descriptives_list = list(descriptives_ALSPAC, descriptives_PREDO),
  cohort_names = nc_cohorts
)

# Add variable column from row names
desc_table$Variable <- rownames(desc_table)
desc_nc_table$Variable <- rownames(desc_nc_table)

# Define the exact desired row order
desired_row_order <- c(
  "n (% of original sample)",
  "Child characteristics",
  "Assigned sex, male, n (%)",
  "Maternal characteristics",
  "Maternal age at birth, years, mean (SD)",
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

# After merging:
combined_table <- merge(desc_table, desc_nc_table, by = "Variable", all = TRUE)

# Reorder rows to match desired order, keeping only variables present in the data:
existing_rows <- desired_row_order[desired_row_order %in% combined_table$Variable]

combined_table <- combined_table[match(existing_rows, combined_table$Variable), ]

# Then reorder columns (as before)
desired_col_order <- c("Variable", "abcd", "alspac", "bib", "dnbc", "eden", "genr", "ninfea", "predo")
existing_cols <- desired_col_order[desired_col_order %in% colnames(combined_table)]
combined_table <- combined_table[, existing_cols]

# Build flextable
library(flextable)
library(officer)

ft <- flextable(combined_table)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 7, part = "all")

# Highlight section headers
section_rows <- which(combined_table$Variable %in% c("Child characteristics", "Maternal characteristics"))
ft <- bold(ft, i = section_rows)
ft <- italic(ft, i = section_rows)

# Blank out section header cells
for (col_name in existing_cols[-1]) { # exclude Variable column
  ft <- compose(ft, i = section_rows, j = col_name, value = as_paragraph(""))
}

# Adjust layout
ft <- autofit(ft)
ft <- width(ft, width = 0.8)
ft <- padding(ft, padding = 1, part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")

# Save to Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = file.path(outdir, "descriptives_excluded_table.docx"))
