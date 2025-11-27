################################################################################
# Title:    Sensitivity analyses 
# Project:  ECCN project 
# Author:   Jana Hermans
# Date:     Created in February 2025
################################################################################
library(grid)
library(tidyverse)
library(gt)
library(dplyr)
library(dplyr)
library(flextable)
library(officer)

projectdir <- '/Users/janahermans/Documents/PhD/02_project/'
setwd(file.path(projectdir,"results")) 
outdir <- paste0(projectdir,"results/tables")

#---------------------------Teacher-reported outcomes---------------------------
TRF_results <- readRDS('./REV/REV_results_TRF_GenR.RData')
TRF_results <- TRF_results[TRF_results$outcome == 'adhd_',]
MR_results <- read.csv("ECCN/results_adhd_main_model_ECCN.csv")
MR_results <- MR_results %>%
  mutate(conf_low = preg_dep_betas - 1.96 * ses,
         conf_high = preg_dep_betas + 1.96 * ses)
MR_results <- MR_results[MR_results$cohort == 'genr',]

MR_table <- MR_results %>%
  mutate(
    Rater = 'Mother report',
    N = as.character(N),
    `β` = sprintf("%.2f", preg_dep_betas),
    `95% CI` = paste0("[", sprintf("%.2f", conf_low), ", ", sprintf("%.2f", conf_high), "]")
  ) %>%
  select(Rater, N, `β`, `95% CI`) 

TRF_table <- TRF_results %>%
  mutate(
    Rater = 'Teacher report',
    N = as.character(samplesize),
    `β` = sprintf("%.2f", Estimate),
    `95% CI` = paste0("[", sprintf("%.2f", conf_low), ", ", sprintf("%.2f", conf_high), "]")
    ) %>%
  select(Rater, N, `β`, `95% CI`) 
  
combined_df <- rbind(MR_table,TRF_table)

ft_TRF <- flextable(combined_df) %>%
  font(fontname = "Times New Roman", part = "all") %>%  # set font
  fontsize(size = 7.5, part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  add_header_row(
    values = c("", "ADHD symptoms"),
    colwidths = c(1, 3)
  ) %>%
  align(
    i = 1,        # first header row only
    j = 2:4,      # columns spanned by "ADHD symptoms"
    align = "center",
    part = "header"
  )

# Save table to Word file
#doc <- read_docx() %>%
#  body_add_flextable(ft)
#print(doc, target = file.path(outdir,"REV_TRF_table.docx"))

#-------------------Maternal depression - direct associations-------------------
# Process 1-year postnatal period
process_1y <- function(csv_path) {
  read.csv(csv_path) %>%
    filter(cohort == "genr") %>%
    mutate(
      conf.low  = preg_dep_betas - 1.96 * ses,
      conf.high = preg_dep_betas + 1.96 * ses,
      samplesize = N,
      Estimate = preg_dep_betas
    ) %>%
    select(samplesize, Estimate, conf.low, conf.high) %>%
    `rownames<-`("dep_bin_1y1")
}

# Process 3y/9y periods
process_3y9y <- function(rds_path) {
  rds <- readRDS(rds_path)
  out <- rbind(rds$dep3y, rds$dep9y)
  out %>% select(samplesize, Estimate, conf.low, conf.high)
}

# Combine 1y + 3y/9y for each outcome
combine_direct <- function(csv, rds) {
  rbind(process_1y(csv), process_3y9y(rds))
}

# Format output for table
format_result <- function(df) {
  df %>%
    mutate(
      N       = as.character(samplesize),
      `β`     = sprintf("%.2f", Estimate),
      `95% CI`= sprintf("[%.2f, %.2f]", conf.low, conf.high)
    ) %>%
    select(N, `β`, `95% CI`)
}

# Rename columns with prefix
rename_with_prefix <- function(df, prefix) {
  setNames(df, paste0(prefix, "_", names(df)))
}

int_dir  <- combine_direct(
  "./ECCN/results_int_ppd_indivi_ECCN.csv",
  "./REV/REV_results_direct_int_3y9y_GenR.RData"
)

ext_dir  <- combine_direct(
  "./ECCN/results_ext_ppd_indivi_ECCN.csv",
  "./REV/REV_results_direct_ext_3y9y_GenR.RData"
)

adhd_dir <- combine_direct(
  "./ECCN/results_adhd_ppd_indivi_ECCN.csv",
  "./REV/REV_results_direct_adhd_3y9y_GenR.RData"
)

asd_dir  <- combine_direct(
  "./ECCN/results_asd_ppd_indivi_ECCN.csv",
  "./REV/REV_results_direct_asd_3y9y_GenR.RData"
)

# Formatting for flextable 
timepoints <- as.data.frame(c('Postnatal period', 'Child age 3 years', 'Child age 9 years'))
colnames(timepoints) <- 'Time point'
int_t  <- rename_with_prefix(format_result(int_dir),  "int")
ext_t  <- rename_with_prefix(format_result(ext_dir),  "ext")
adhd_t <- rename_with_prefix(format_result(adhd_dir), "adhd")
asd_t  <- rename_with_prefix(format_result(asd_dir),  "asd")

combined_dir <- cbind(
  Time_point = timepoints,
  int_t, ext_t, adhd_t, asd_t
)

header_df <- data.frame(
  col_keys = colnames(combined_dir),
  
  h1 = c(
    "",
    rep("Internalising symptoms", 3),
    rep("Externalising symptoms", 3),
    rep("ADHD symptoms", 3),
    rep("ASD symptoms", 3)
  ),
  
  h2 = c("Time point", rep(c("N", "β", "95% CI"), 4)),
  stringsAsFactors = FALSE
)

ft_dir <- flextable(combined_dir) |> 
  set_header_df(mapping = header_df, key = "col_keys") |>
  merge_h(part = "header") |>
  align(i = 1, align = "center", part = "header") |>
  align(i = 2, align = "left",   part = "header") |>
  hline_top(part = "header", border = fp_border(color="black", width=1)) |>
  border_inner_h(part="header", border = fp_border(color="black", width=1)) |>
  hline_bottom(part = "header", border = fp_border(color="black", width=1)) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 7.5, part = "all") |>
  set_table_properties(layout = "autofit")

# Save table to Word file
#doc <- read_docx() %>%
#  body_add_flextable(ft)
#print(doc, target = file.path(outdir,"REV_direct_1y3y9y_table.docx"))

#-----------------------Maternal depression - Mediation 1y----------------------
load_med_df <- function(path, beta_col) {
  read.csv(path) %>%
    filter(cohort == "genr") %>%
    mutate(
      estimate = .data[[beta_col]],
      conf.low = estimate - 1.96 * ses,
      conf.high = estimate + 1.96 * ses
    ) %>%
    select(N, estimate, conf.low, conf.high)
}

load_pm_df <- function(path) {
  read.csv(path) %>%
    filter(cohort == "genr") %>%
    mutate(pm = tot_betas * 100) %>%
    mutate(pm = sprintf("%.1f", pm)) %>%
    select(pm)
}

format_result <- function(df) {
  df %>%
    mutate(
      estimate = as.numeric(estimate),
      conf.low = as.numeric(conf.low),
      conf.high = as.numeric(conf.high),
      N = as.character(N),
      `β` = sprintf("%.2f", estimate),
      `95% CI` = sprintf("[%.2f, %.2f]", conf.low, conf.high)
    ) %>%
    select(N, `β`, `95% CI`)
}

combine_direct_indirect <- function(dir_df, ind_df) {
  df_dir <- format_result(dir_df) %>%
    rename(N_direct = N, β_direct = `β`, CI_direct = `95% CI`)
  
  df_ind <- format_result(ind_df) %>%
    rename(N_indirect = N, β_indirect = `β`, CI_indirect = `95% CI`)
  
  cbind(df_dir, df_ind[, -1])  # Keep N only for direct
}

outcome_table <- function(outcome, paths) {
  
  dir_df <- load_med_df(paths$dir, "dir_betas")
  ind_df <- load_med_df(paths$ind, "ind_betas")
  pm_df  <- load_pm_df(paths$pm)
  
  combined <- combine_direct_indirect(dir_df, ind_df)
  combined$`Perc. Med.` <- paste0(pm_df$pm, "%")
  return(combined)
}

paths_list <- list(
  INT  = list(dir="ECCN/results_INT_dir_ppdmed_model_ECCN_5_10.csv",
              ind="ECCN/results_INT_ind_ppdmed_model_ECCN_5_10.csv",
              pm ="ECCN/results_INT_PM_ppdmed_model_ECCN_5_10.csv"),
  
  EXT  = list(dir="ECCN/results_EXT_dir_ppdmed_model_ECCN_5_10.csv",
              ind="ECCN/results_EXT_ind_ppdmed_model_ECCN_5_10.csv",
              pm ="ECCN/results_EXT_PM_ppdmed_model_ECCN_5_10.csv"),
  
  ADHD = list(dir="ECCN/results_ADHD_dir_ppdmed_model_ECCN_3_9.csv",
              ind="ECCN/results_ADHD_ind_ppdmed_model_ECCN_3_9.csv",
              pm ="ECCN/results_ADHD_PM_ppdmed_model_ECCN_3_9.csv"),
  
  ASD  = list(dir="ECCN/results_ASD_dir_ppdmed_model_ECCN_0_10.csv",
              ind="ECCN/results_ASD_ind_ppdmed_model_ECCN_0_10.csv",
              pm ="ECCN/results_ASD_PM_ppdmed_model_ECCN_0_10.csv")
)

all_med_1y <- bind_rows(
  lapply(names(paths_list),
         function(out) outcome_table(out, paths_list[[out]]))
)

outcomes <- as.data.frame(c("Internalising symptoms","Externalising symptoms",
                            "ADHD symptoms","ASD symptoms"))
colnames(outcomes) <- 'Outcome'
all_med_1y <- cbind(outcomes, all_med_1y)

header_df <- data.frame(
  col_keys = colnames(all_med_1y),
  
  h1 = c(
    "",
    "",
    rep("Direct Effect", 2),
    rep("Indirect Effect", 2),
    "Perc. Med."
  ),
  
  h2 = c("Outcome", "N", rep(c("β", "95% CI","β", "95% CI"), 1),""),
  stringsAsFactors = FALSE
)

ft_med_1y <- flextable(all_med_1y) |> 
  set_header_df(mapping = header_df, key = "col_keys") |>
  merge_h(part = "header") |>
  align(i = 1, align = "center", part = "header") |>
  align(i = 2, align = "left",   part = "header") |>
  hline_top(part = "header", border = fp_border(color="black", width=1)) |>
  border_inner_h(part="header", border = fp_border(color="black", width=1)) |>
  hline_bottom(part = "header", border = fp_border(color="black", width=1)) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 7.5, part = "all") |>
  set_table_properties(layout = "autofit")

# Save table to Word file
#doc <- read_docx() %>%
#  body_add_flextable(ft)
#print(doc, target = file.path(outdir,"REV_med_1y_table.docx"))

#---------------------Maternal depression - Mediation 3y/9y---------------------
format_mediation <- function(med_data) {
  # Direct effect
  dir_df <- as.data.frame(med_data$direct_effect) %>%
    mutate(
      β_direct = sprintf("%.2f", est),
      CI_direct = paste0("[", sprintf("%.2f", lo), ", ", sprintf("%.2f", up), "]")
    ) %>%
    select(β_direct, CI_direct)
  
  # Indirect effect
  ind_df <- as.data.frame(med_data$indirect_effect) %>%
    mutate(
      β_indirect = sprintf("%.2f", est),
      CI_indirect = paste0("[", sprintf("%.2f", lo), ", ", sprintf("%.2f", up), "]")
    ) %>%
    select(β_indirect, CI_indirect)
  
  # Combine
  combined_df <- cbind(dir_df, ind_df)
  
  # Proportion mediated
  combined_df$pm <- paste0(sprintf("%.1f", med_data$proportion_mediated), "%")
  
  # Add sample size
  combined_df <- cbind(N = as.character(med_data$samplesize), combined_df)
  
  return(combined_df)
}

# Read and format datasets
int_med_3y   <- readRDS("REV/REV_results_mediation_int_3y_GenR.Rdata")
ext_med_3y   <- readRDS("REV/REV_results_mediation_ext_3y_GenR.Rdata")
adhd_med_3y  <- readRDS("REV/REV_results_mediation_adhd_3y_GenR.Rdata")
asd_med_3y   <- readRDS("REV/REV_results_mediation_asd_3y_GenR.Rdata")

# Apply helper function
int_med_3y_table   <- format_mediation(int_med_3y)
ext_med_3y_table   <- format_mediation(ext_med_3y)
adhd_med_3y_table  <- format_mediation(adhd_med_3y)
asd_med_3y_table   <- format_mediation(asd_med_3y)

all_med_3y <- rbind(int_med_3y_table,ext_med_3y_table,adhd_med_3y_table,asd_med_3y_table)

outcomes <- as.data.frame(c("Internalising symptoms","Externalising symptoms",
                            "ADHD symptoms","ASD symptoms"))
colnames(outcomes) <- 'Outcome'
all_med_3y <- cbind(outcomes, all_med_3y)

header_df <- data.frame(
  col_keys = colnames(all_med_3y),
  
  h1 = c(
    "",
    "",
    rep("Direct Effect", 2),
    rep("Indirect Effect", 2),
    "Perc. Med."
  ),
  
  h2 = c("Outcome", "N", rep(c("β", "95% CI","β", "95% CI"), 1),""),
  stringsAsFactors = FALSE
)

ft_med_3y <- flextable(all_med_3y) |> 
  set_header_df(mapping = header_df, key = "col_keys") |>
  merge_h(part = "header") |>
  align(i = 1, align = "center", part = "header") |>
  align(i = 2, align = "left",   part = "header") |>
  hline_top(part = "header", border = fp_border(color="black", width=1)) |>
  border_inner_h(part="header", border = fp_border(color="black", width=1)) |>
  hline_bottom(part = "header", border = fp_border(color="black", width=1)) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 7.5, part = "all") |>
  set_table_properties(layout = "autofit")

# Save table to Word file
#doc <- read_docx() %>%
#  body_add_flextable(ft)
#print(doc, target = file.path(outdir,"REV_med_3y_table.docx"))

# Read and format datasets
int_med_9y   <- readRDS("REV/REV_results_mediation_int_9y_GenR.Rdata")
ext_med_9y   <- readRDS("REV/REV_results_mediation_ext_9y_GenR.Rdata")
adhd_med_9y  <- readRDS("REV/REV_results_mediation_adhd_9y_GenR.Rdata")
asd_med_9y   <- readRDS("REV/REV_results_mediation_asd_9y_GenR.Rdata")

# Apply helper function
int_med_9y_table   <- format_mediation(int_med_9y)
ext_med_9y_table   <- format_mediation(ext_med_9y)
adhd_med_9y_table  <- format_mediation(adhd_med_9y)
asd_med_9y_table   <- format_mediation(asd_med_9y)

all_med_9y <- rbind(int_med_9y_table,ext_med_9y_table,adhd_med_9y_table,asd_med_9y_table)

outcomes <- as.data.frame(c("Internalising symptoms","Externalising symptoms",
                            "ADHD symptoms","ASD symptoms"))
colnames(outcomes) <- 'Outcome'
all_med_9y <- cbind(outcomes, all_med_9y)

header_df <- data.frame(
  col_keys = colnames(all_med_9y),
  
  h1 = c(
    "",
    "",
    rep("Direct Effect", 2),
    rep("Indirect Effect", 2),
    "Perc. Med."
  ),
  
  h2 = c("Outcome", "N", rep(c("β", "95% CI","β", "95% CI"), 1),""),
  stringsAsFactors = FALSE
)

ft_med_9y <- flextable(all_med_9y) |> 
  set_header_df(mapping = header_df, key = "col_keys") |>
  merge_h(part = "header") |>
  align(i = 1, align = "center", part = "header") |>
  align(i = 2, align = "left",   part = "header") |>
  hline_top(part = "header", border = fp_border(color="black", width=1)) |>
  border_inner_h(part="header", border = fp_border(color="black", width=1)) |>
  hline_bottom(part = "header", border = fp_border(color="black", width=1)) |>
  font(fontname = "Times New Roman", part = "all") |>
  fontsize(size = 7.5, part = "all") |>
  set_table_properties(layout = "autofit")

# Save table to Word file
#doc <- read_docx() %>%
#  body_add_flextable(ft)
#print(doc, target = file.path(outdir,"REV_med_3y_table.docx"))
#--------------------------------Save all tables--------------------------------

# List of tables you want to save
tables_list <- list(
  "TRF"   = ft_TRF,
  "DIRECT"   = ft_dir,
  "MED 1y"  = ft_med_1y,
  "MED 3y"   = ft_med_3y,
  "MED 9y"   = ft_med_9y
)

doc <- read_docx()

for (name in names(tables_list)) {
  ft <- tables_list[[name]]  # already a flextable
  
  doc <- doc %>%
    body_add_par(name, style = "Normal") %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal") 
}

print(doc, target = file.path(outdir, "REV_med_all_3y_tables.docx"))

