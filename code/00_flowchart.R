################################################################################
# Title:    Flowchart ECCN project
# Author:   Jana Hermans
# Date:     Created in July 2025
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

projectdir <- '/Users/janahermans/Documents/PhD/02_project/'
setwd(file.path(projectdir,"results"))
outdir <- paste0(projectdir,"/results/tables")

# Load data
descriptives_excluded_ECCN <- readRDS("./ECCN/descriptives_ECCN_excluded.RData")
flowchart_ECCN <- readRDS("./ECCN/flowchart_ECCN.RData")
descriptives_included_ALSPAC <- readRDS("./ALSPAC_results/descriptives_ALSPAC.RData")
descriptives_included_PREDO <- readRDS("./PREDOresultsandscript/descriptives_PREDO.RData")
descriptives_excluded_ALSPAC <- readRDS("./ALSPAC_results/descriptives_with_excl_ALSPAC.RData")
descriptives_excluded_PREDO <- readRDS("./PREDOresultsandscript/descriptives_with_excl_PREDO.RData")

# Extract and simplify the lists
n_full <- descriptives_excluded_ECCN[["study_sample_N"]][1,]+descriptives_excluded_ECCN[["study_sample_N"]][2,]
n_exp <- sapply(flowchart_ECCN$N_EXP, `[`, 1)
n_exp_out <- sapply(flowchart_ECCN$N_EXP_OUT, `[`, 1)
n_exp_out_cov <- sapply(flowchart_ECCN$N_EXP_OUT_COV, `[`, 1)

# Combine into a data frame
df <- rbind(
  N_full = n_full,
  N_EXP = n_exp,
  N_EXP_OUT = n_exp_out,
  N_EXP_OUT_COV = n_exp_out_cov
)

# Make sure it's a data.frame (optional)
df <- as.data.frame(df)

# Clean up column names if needed
colnames(df) <- gsub("length of unique.newobj in ", "", colnames(df))
colnames(df) <- gsub("dimensions of df_complete_w in ", "", colnames(df))

# View result
print(df)
# Example: select only "genr", "eden", "abcd"
flowchart <- df[, c("abcd", "bib", "dnbc", "eden", "genr", "ninfea")]

n_exp_ALSPAC <- descriptives_excluded_ALSPAC[["full_sample_N"]]-descriptives_excluded_ALSPAC$preg_dep_NC$missing
# Combine into a data frame
df_ALSPAC <- rbind(
  N_full = descriptives_excluded_ALSPAC[["full_sample_N"]],
  N_EXP = n_exp_ALSPAC,
  N_EXP_OUT = descriptives_included_ALSPAC$study_sample_N,
  N_EXP_OUT_COV = descriptives_included_ALSPAC$study_sample_N_CC
)
colnames(df_ALSPAC) <- 'alspac'

n_exp_PREDO <- descriptives_excluded_PREDO[["full_sample_N"]]-descriptives_excluded_PREDO$preg_dep_NC$missing
# Combine into a data frame
df_PREDO <- rbind(
  N_full = descriptives_excluded_PREDO[["full_sample_N"]],
  N_EXP = n_exp_PREDO,
  N_EXP_OUT = descriptives_included_PREDO$study_sample_N,
  N_EXP_OUT_COV = descriptives_included_PREDO$study_sample_N_CC
)
colnames(df_PREDO) <- 'predo'

full_flowchart <- cbind(flowchart, df_ALSPAC, df_PREDO)
full_flowchart <- full_flowchart[, c("abcd", "alspac", "bib", "dnbc", "eden", 
                                     "genr", "ninfea", "predo")] #alphabetical order


