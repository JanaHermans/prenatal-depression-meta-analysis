################################################################################
# Title:    Meta-analyses mediation models with postnatal depression + Figure
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
library(webshot2)
library(flextable)
library(officer)

projectdir <- '/Users/janahermans/Documents/PhD/02_project/'
setwd(paste0(projectdir,"results/"))
outputdir <- paste0(projectdir,"results/plots")
outdirtable <- paste0(projectdir,"results/tables")
#---------------------------------INTERNALISING---------------------------------
# DIRECT EFFECT
int_med_ALSPAC <- readRDS("ALSPAC_results/results_mediation_int_ALSPAC.Rdata")
int_med_PREDO <- readRDS("PREDOresultsandscript/results_mediation_int_PREDO.Rdata")
int_med_ECCN_dir <- as.data.frame(read.csv("ECCN/results_INT_dir_ppdmed_model_ECCN_5_10.csv"))
int_med_ECCN_dir <- int_med_ECCN_dir[,2:5]
int_med_ALSPAC_dir <- c(int_med_ALSPAC[["direct_effect"]]$est, int_med_ALSPAC[["direct_effect"]]$se, int_med_ALSPAC$samplesize,'alspac')
int_med_PREDO_dir <- c(int_med_PREDO[["direct_effect"]]$est, int_med_PREDO[["direct_effect"]]$se, int_med_PREDO$samplesize,'predo')
int_med_DIR <- rbind(int_med_ECCN_dir,int_med_ALSPAC_dir)
int_med_DIR <- rbind(int_med_DIR,int_med_PREDO_dir)

int_med_DIR$dir_betas <- as.numeric(int_med_DIR$dir_betas)
int_med_DIR$ses <- as.numeric(int_med_DIR$ses)

dir.model.int <- rma(yi = int_med_DIR$dir_betas, sei = int_med_DIR$ses)

# INDIRECT EFFECT
int_med_ECCN_ind <- as.data.frame(read.csv("ECCN/results_INT_ind_ppdmed_model_ECCN_5_10.csv"))
int_med_ECCN_ind <- int_med_ECCN_ind[,2:5]
int_med_ALSPAC_ind <- c(int_med_ALSPAC[["indirect_effect"]]$est, int_med_ALSPAC[["indirect_effect"]]$se, int_med_ALSPAC$samplesize,'alspac')
int_med_PREDO_ind <- c(int_med_PREDO[["indirect_effect"]]$est, int_med_PREDO[["indirect_effect"]]$se, int_med_PREDO$samplesize,'predo')
int_med_IND <- rbind(int_med_ECCN_ind,int_med_ALSPAC_ind)
int_med_IND <- rbind(int_med_IND,int_med_PREDO_ind)

int_med_IND$ind_betas <- as.numeric(int_med_IND$ind_betas)
int_med_IND$ses <- as.numeric(int_med_IND$ses)

ind.model.int <- rma(yi = int_med_IND$ind_betas, sei = int_med_IND$ses)

# TOTAL EFFECT
int_med_ECCN_tot <- as.data.frame(read.csv("ECCN/results_INT_tot_ppdmed_model_ECCN_5_10.csv"))
int_med_ECCN_tot <- int_med_ECCN_tot[,2:5]
int_med_ALSPAC_tot <- c(int_med_ALSPAC[["total_effect"]]$est, int_med_ALSPAC[["total_effect"]]$se, int_med_ALSPAC$samplesize,'alspac')
int_med_PREDO_tot <- c(int_med_PREDO[["total_effect"]]$est, int_med_PREDO[["total_effect"]]$se, int_med_PREDO$samplesize,'predo')
int_med_TOT <- rbind(int_med_ECCN_tot,int_med_ALSPAC_tot)
int_med_TOT <- rbind(int_med_TOT,int_med_PREDO_tot)

int_med_TOT$tot_betas <- as.numeric(int_med_TOT$tot_betas)
int_med_TOT$ses <- as.numeric(int_med_TOT$ses)

tot.model.int <- rma(yi = int_med_TOT$tot_betas, sei = int_med_TOT$ses)

# PROPORTION MEDIATED
int_med_ALSPAC_PM <- c(int_med_ALSPAC$proportion_mediated,'alspac')
int_med_PREDO_PM <- c(int_med_PREDO$proportion_mediated,'predo')
int_med_ECCN_PM <- as.data.frame(read.csv("ECCN/results_INT_PM_ppdmed_model_ECCN_5_10.csv"))
int_med_ECCN_PM <- select(int_med_ECCN_PM, c(tot_betas,cohort))
int_med_ECCN_PM$tot_betas <- int_med_ECCN_PM$tot_betas*100
int_med_PM <- rbind(int_med_ECCN_PM,int_med_ALSPAC_PM)
int_med_PM <- rbind(int_med_PM,int_med_PREDO_PM)
int_med_PM[[2]] <- toupper(as.character(int_med_PM[[2]])) # Convert second column to uppercase
int_med_PM[(nrow(int_med_IND)+1),] <- c((as.numeric(ind.model.int$b)/as.numeric(tot.model.int$b))*100,"TOTAL")
int_med_PM$cohort[int_med_PM$cohort == "GENR"] <- "Generation R"
colnames(int_med_PM)[colnames(int_med_PM)=='tot_betas'] <- "pm"
int_med_PM$pm <- sprintf("%.1f", as.numeric(as.character(int_med_PM$pm))) # Round first column to 1 decimal place

# Results table
int_med_DIR$conf.high <- as.numeric(int_med_DIR$dir_betas) + as.numeric(int_med_DIR$ses)*1.96
int_med_DIR$conf.low <- as.numeric(int_med_DIR$dir_betas) - as.numeric(int_med_DIR$ses)*1.96
int_table_dir <- int_med_DIR[c("dir_betas","conf.low","conf.high","N","cohort")]
int_table_dir$cohort <- toupper(int_table_dir$cohort)
int_table_dir$cohort[int_table_dir$cohort == "GENR"] <- "Generation R"
colnames(int_table_dir)[colnames(int_table_dir)=='dir_betas'] <- "estimate"
int_table_dir[(nrow(int_med_DIR)+1),] <- c(as.numeric(dir.model.int$b), dir.model.int$ci.lb, dir.model.int$ci.ub, sum(as.numeric(int_med_DIR$N)), "TOTAL")

int_med_IND$conf.high <- as.numeric(int_med_IND$ind_betas) + as.numeric(int_med_IND$ses)*1.96
int_med_IND$conf.low <- as.numeric(int_med_IND$ind_betas) - as.numeric(int_med_IND$ses)*1.96
int_table_ind <- int_med_IND[c("ind_betas","conf.low","conf.high","N","cohort")]
int_table_ind$cohort <- toupper(int_table_ind$cohort)
int_table_ind$cohort[int_table_ind$cohort == "GENR"] <- "Generation R"
colnames(int_table_ind)[colnames(int_table_ind)=='ind_betas'] <- "estimate"
int_table_ind[(nrow(int_med_IND)+1),] <- c(as.numeric(ind.model.int$b), ind.model.int$ci.lb, ind.model.int$ci.ub, sum(as.numeric(int_med_IND$N)), "TOTAL")
#---------------------------------EXTERNALISING---------------------------------
# DIRECT EFFECT
ext_med_ALSPAC <- readRDS("ALSPAC_results/results_mediation_ext_ALSPAC.Rdata")
ext_med_PREDO <- readRDS("PREDOresultsandscript/results_mediation_ext_PREDO.Rdata")
ext_med_ECCN_dir <- as.data.frame(read.csv("ECCN/results_EXT_dir_ppdmed_model_ECCN_5_10.csv"))
ext_med_ECCN_dir <- ext_med_ECCN_dir[,2:5]
ext_med_ALSPAC_dir <- c(ext_med_ALSPAC[["direct_effect"]]$est, ext_med_ALSPAC[["direct_effect"]]$se, 
                        ext_med_ALSPAC[["samplesize"]],'alspac')
ext_med_PREDO_dir <- c(ext_med_PREDO[["direct_effect"]]$est, ext_med_PREDO[["direct_effect"]]$se,
                       ext_med_PREDO[["samplesize"]],'predo')
ext_med_DIR <- rbind(ext_med_ECCN_dir,ext_med_ALSPAC_dir)
ext_med_DIR <- rbind(ext_med_DIR,ext_med_PREDO_dir)

ext_med_DIR$dir_betas <- as.numeric(ext_med_DIR$dir_betas)
ext_med_DIR$ses <- as.numeric(ext_med_DIR$ses)

dir.model.ext <- rma(yi = ext_med_DIR$dir_betas, sei = ext_med_DIR$ses)

# INDIRECT EFFECT
ext_med_ECCN_ind <- as.data.frame(read.csv("ECCN/results_EXT_ind_ppdmed_model_ECCN_5_10.csv"))
ext_med_ECCN_ind <- ext_med_ECCN_ind[,2:5]
ext_med_ALSPAC_ind <- c(ext_med_ALSPAC[["indirect_effect"]]$est, ext_med_ALSPAC[["indirect_effect"]]$se,
                        ext_med_ALSPAC[["samplesize"]],'alspac')
ext_med_PREDO_ind <- c(ext_med_PREDO[["indirect_effect"]]$est, ext_med_PREDO[["indirect_effect"]]$se,
                       ext_med_PREDO[["samplesize"]],'predo')
ext_med_IND <- rbind(ext_med_ECCN_ind,ext_med_ALSPAC_ind)
ext_med_IND <- rbind(ext_med_IND,ext_med_PREDO_ind)

ext_med_IND$ind_betas <- as.numeric(ext_med_IND$ind_betas)
ext_med_IND$ses <- as.numeric(ext_med_IND$ses)

ind.model.ext <- rma(yi = ext_med_IND$ind_betas, sei = ext_med_IND$ses)

# TOTAL EFFECT
ext_med_ECCN_tot <- as.data.frame(read.csv("ECCN/results_EXT_tot_ppdmed_model_ECCN_5_10.csv"))
ext_med_ECCN_tot <- ext_med_ECCN_tot[,2:5]
ext_med_ALSPAC_tot <- c(ext_med_ALSPAC[["total_effect"]]$est, ext_med_ALSPAC[["total_effect"]]$se, ext_med_ALSPAC$samplesize,'alspac')
ext_med_PREDO_tot <- c(ext_med_PREDO[["total_effect"]]$est, ext_med_PREDO[["total_effect"]]$se, ext_med_PREDO$samplesize,'predo')
ext_med_TOT <- rbind(ext_med_ECCN_tot,ext_med_ALSPAC_tot)
ext_med_TOT <- rbind(ext_med_TOT,ext_med_PREDO_tot)

ext_med_TOT$tot_betas <- as.numeric(ext_med_TOT$tot_betas)
ext_med_TOT$ses <- as.numeric(ext_med_TOT$ses)

tot.model.ext <- rma(yi = ext_med_TOT$tot_betas, sei = ext_med_TOT$ses)

# PROPORTION MEDIATED
ext_med_ALSPAC_PM <- c(ext_med_ALSPAC$proportion_mediated,'alspac')
ext_med_PREDO_PM <- c(ext_med_PREDO$proportion_mediated,'predo')
ext_med_ECCN_PM <- as.data.frame(read.csv("ECCN/results_EXT_PM_ppdmed_model_ECCN_5_10.csv"))
ext_med_ECCN_PM <- select(ext_med_ECCN_PM, c(tot_betas,cohort))
ext_med_ECCN_PM$tot_betas <- ext_med_ECCN_PM$tot_betas*100
ext_med_PM <- rbind(ext_med_ECCN_PM,ext_med_ALSPAC_PM)
ext_med_PM <- rbind(ext_med_PM,ext_med_PREDO_PM)
ext_med_PM[[2]] <- toupper(as.character(ext_med_PM[[2]])) # Convert second column to uppercase
ext_med_PM[(nrow(ext_med_IND)+1),] <- c((as.numeric(ind.model.ext$b)/as.numeric(tot.model.ext$b))*100,"TOTAL")
ext_med_PM$cohort[ext_med_PM$cohort == "GENR"] <- "Generation R"
colnames(ext_med_PM)[colnames(ext_med_PM)=='tot_betas'] <- "pm"
ext_med_PM$pm <- sprintf("%.1f", as.numeric(as.character(ext_med_PM$pm))) # Round first column to 1 decimal place

# Results table
ext_med_DIR$conf.high <- as.numeric(ext_med_DIR$dir_betas) + as.numeric(ext_med_DIR$ses)*1.96
ext_med_DIR$conf.low <- as.numeric(ext_med_DIR$dir_betas) - as.numeric(ext_med_DIR$ses)*1.96
ext_table_dir <- ext_med_DIR[c("dir_betas","conf.low","conf.high","N","cohort")]
ext_table_dir$cohort <- toupper(ext_table_dir$cohort)
ext_table_dir$cohort[ext_table_dir$cohort == "GENR"] <- "Generation R"
colnames(ext_table_dir)[colnames(ext_table_dir)=='dir_betas'] <- "estimate"
ext_table_dir[(nrow(ext_med_DIR)+1),] <- c(as.numeric(dir.model.ext$b), dir.model.ext$ci.lb, dir.model.ext$ci.ub, sum(as.numeric(ext_med_DIR$N)), "TOTAL")

ext_med_IND$conf.high <- as.numeric(ext_med_IND$ind_betas) + as.numeric(ext_med_IND$ses)*1.96
ext_med_IND$conf.low <- as.numeric(ext_med_IND$ind_betas) - as.numeric(ext_med_IND$ses)*1.96
ext_table_ind <- ext_med_IND[c("ind_betas","conf.low","conf.high","N","cohort")]
ext_table_ind$cohort <- toupper(ext_table_ind$cohort)
ext_table_ind$cohort[ext_table_ind$cohort == "GENR"] <- "Generation R"
colnames(ext_table_ind)[colnames(ext_table_ind)=='ind_betas'] <- "estimate"
ext_table_ind[(nrow(ext_med_IND)+1),] <- c(as.numeric(ind.model.ext$b), ind.model.ext$ci.lb, ind.model.ext$ci.ub, sum(as.numeric(ext_med_IND$N)), "TOTAL")
#--------------------------------------ADHD-------------------------------------
# DIRECT EFFECT
adhd_med_ALSPAC <- readRDS("ALSPAC_results/results_mediation_adhd_ALSPAC.Rdata")
adhd_med_PREDO <- readRDS("PREDOresultsandscript/results_mediation_adhd_PREDO.Rdata")
adhd_med_ECCN_dir <- as.data.frame(read.csv("ECCN/results_adhd_dir_ppdmed_model_ECCN_3_9.csv"))
adhd_med_ECCN_dir <- adhd_med_ECCN_dir[,2:5]
adhd_med_ALSPAC_dir <- c(adhd_med_ALSPAC[["direct_effect"]]$est, adhd_med_ALSPAC[["direct_effect"]]$se,
                         adhd_med_ALSPAC[["samplesize"]],'alspac')
adhd_med_PREDO_dir <- c(adhd_med_PREDO[["direct_effect"]]$est, adhd_med_PREDO[["direct_effect"]]$se,
                        adhd_med_PREDO[["samplesize"]],'predo')
adhd_med_DIR <- rbind(adhd_med_ECCN_dir,adhd_med_ALSPAC_dir)
adhd_med_DIR <- rbind(adhd_med_DIR,adhd_med_PREDO_dir)

adhd_med_DIR$dir_betas <- as.numeric(adhd_med_DIR$dir_betas)
adhd_med_DIR$ses <- as.numeric(adhd_med_DIR$ses)
adhd_med_DIR <- adhd_med_DIR[!grepl("bib", adhd_med_DIR$cohort), ] # make sure BiB is not included
adhd_med_DIR

dir.model.adhd <- rma(yi = adhd_med_DIR$dir_betas, sei = adhd_med_DIR$ses)

# INDIRECT EFFECT
adhd_med_ECCN_ind <- as.data.frame(read.csv("ECCN/results_adhd_ind_ppdmed_model_ECCN_3_9.csv"))
adhd_med_ECCN_ind <- adhd_med_ECCN_ind[,2:5]
adhd_med_ALSPAC_ind <- c(adhd_med_ALSPAC[["indirect_effect"]]$est, adhd_med_ALSPAC[["indirect_effect"]]$se,
                         adhd_med_ALSPAC[["samplesize"]],'alspac')
adhd_med_PREDO_ind <- c(adhd_med_PREDO[["indirect_effect"]]$est, adhd_med_PREDO[["indirect_effect"]]$se,
                        adhd_med_PREDO[["samplesize"]],'predo')
adhd_med_IND <- rbind(adhd_med_ECCN_ind,adhd_med_ALSPAC_ind)
adhd_med_IND <- rbind(adhd_med_IND,adhd_med_PREDO_ind)

adhd_med_IND$ind_betas <- as.numeric(adhd_med_IND$ind_betas)
adhd_med_IND$ses <- as.numeric(adhd_med_IND$ses)
adhd_med_IND <- adhd_med_IND[!grepl("bib", adhd_med_IND$cohort), ] # make sure BiB is not included
adhd_med_IND

ind.model.adhd <- rma(yi = adhd_med_IND$ind_betas, sei = adhd_med_IND$ses)

# TOTAL EFFECT
adhd_med_ECCN_tot <- as.data.frame(read.csv("ECCN/results_ADHD_tot_ppdmed_model_ECCN_3_9.csv"))
adhd_med_ECCN_tot <- adhd_med_ECCN_tot[,2:5]
adhd_med_ALSPAC_tot <- c(adhd_med_ALSPAC[["total_effect"]]$est, adhd_med_ALSPAC[["total_effect"]]$se, adhd_med_ALSPAC$samplesize,'alspac')
adhd_med_PREDO_tot <- c(adhd_med_PREDO[["total_effect"]]$est, adhd_med_PREDO[["total_effect"]]$se, adhd_med_PREDO$samplesize,'predo')
adhd_med_TOT <- rbind(adhd_med_ECCN_tot,adhd_med_ALSPAC_tot)
adhd_med_TOT <- rbind(adhd_med_TOT,adhd_med_PREDO_tot)

adhd_med_TOT$tot_betas <- as.numeric(adhd_med_TOT$tot_betas)
adhd_med_TOT$ses <- as.numeric(adhd_med_TOT$ses)

tot.model.adhd <- rma(yi = adhd_med_TOT$tot_betas, sei = adhd_med_TOT$ses)

# PROPORTION MEDIATED
adhd_med_ALSPAC_PM <- c(adhd_med_ALSPAC$proportion_mediated,'alspac')
adhd_med_PREDO_PM <- c(adhd_med_PREDO$proportion_mediated,'predo')
adhd_med_ECCN_PM <- as.data.frame(read.csv("ECCN/results_ADHD_PM_ppdmed_model_ECCN_3_9.csv"))
adhd_med_ECCN_PM <- select(adhd_med_ECCN_PM, c(tot_betas,cohort))
adhd_med_ECCN_PM$tot_betas <- adhd_med_ECCN_PM$tot_betas*100
adhd_med_PM <- rbind(adhd_med_ECCN_PM,adhd_med_ALSPAC_PM)
adhd_med_PM <- rbind(adhd_med_PM,adhd_med_PREDO_PM)
adhd_med_PM[[2]] <- toupper(as.character(adhd_med_PM[[2]])) # Convert second column to uppercase
adhd_med_PM[(nrow(adhd_med_IND)+1),] <- c((as.numeric(ind.model.adhd$b)/as.numeric(tot.model.adhd$b))*100,"TOTAL")
adhd_med_PM$cohort[adhd_med_PM$cohort == "GENR"] <- "Generation R"
colnames(adhd_med_PM)[colnames(adhd_med_PM)=='tot_betas'] <- "pm"
adhd_med_PM$pm <- sprintf("%.1f", as.numeric(as.character(adhd_med_PM$pm))) # Round first column to 1 decimal place

# Results table
adhd_med_DIR$conf.high <- as.numeric(adhd_med_DIR$dir_betas) + as.numeric(adhd_med_DIR$ses)*1.96
adhd_med_DIR$conf.low <- as.numeric(adhd_med_DIR$dir_betas) - as.numeric(adhd_med_DIR$ses)*1.96
adhd_table_dir <- adhd_med_DIR[c("dir_betas","conf.low","conf.high","N","cohort")]
adhd_table_dir$cohort <- toupper(adhd_table_dir$cohort)
adhd_table_dir$cohort[adhd_table_dir$cohort == "GENR"] <- "Generation R"
colnames(adhd_table_dir)[colnames(adhd_table_dir)=='dir_betas'] <- "estimate"
adhd_table_dir[(nrow(adhd_med_DIR)+1),] <- c(as.numeric(dir.model.adhd$b), dir.model.adhd$ci.lb, dir.model.adhd$ci.ub, sum(as.numeric(adhd_med_DIR$N)), "TOTAL")

adhd_med_IND$conf.high <- as.numeric(adhd_med_IND$ind_betas) + as.numeric(adhd_med_IND$ses)*1.96
adhd_med_IND$conf.low <- as.numeric(adhd_med_IND$ind_betas) - as.numeric(adhd_med_IND$ses)*1.96
adhd_table_ind <- adhd_med_IND[c("ind_betas","conf.low","conf.high","N","cohort")]
adhd_table_ind$cohort <- toupper(adhd_table_ind$cohort)
adhd_table_ind$cohort[adhd_table_ind$cohort == "GENR"] <- "Generation R"
colnames(adhd_table_ind)[colnames(adhd_table_ind)=='ind_betas'] <- "estimate"
adhd_table_ind[(nrow(adhd_med_IND)+1),] <- c(as.numeric(ind.model.adhd$b), ind.model.adhd$ci.lb, ind.model.adhd$ci.ub, sum(as.numeric(adhd_med_IND$N)), "TOTAL")
#--------------------------------------ASD--------------------------------------
# DIRECT EFFECT
asd_med_PREDO <- readRDS("PREDOresultsandscript/results_mediation_asd_PREDO.Rdata")
asd_med_ECCN_dir <- as.data.frame(read.csv("ECCN/results_ASD_dir_ppdmed_model_ECCN_0_10.csv"))
asd_med_ECCN_dir <- asd_med_ECCN_dir[,2:5]
asd_med_PREDO_dir <- c(asd_med_PREDO[["direct_effect"]]$est, asd_med_PREDO[["direct_effect"]]$se,
                       asd_med_PREDO[["samplesize"]],'predo')
asd_med_DIR <- rbind(asd_med_ECCN_dir,asd_med_PREDO_dir)

asd_med_DIR$dir_betas <- as.numeric(asd_med_DIR$dir_betas)
asd_med_DIR$ses <- as.numeric(asd_med_DIR$ses)

dir.model.asd <- rma(yi = asd_med_DIR$dir_betas, sei = asd_med_DIR$ses)

# INDIRECT EFFECT
asd_med_ECCN_ind <- as.data.frame(read.csv("ECCN/results_ASD_ind_ppdmed_model_ECCN_0_10.csv"))
asd_med_ECCN_ind <- asd_med_ECCN_ind[,2:5]
asd_med_PREDO_ind <- c(asd_med_PREDO[["indirect_effect"]]$est, asd_med_PREDO[["indirect_effect"]]$se,
                       asd_med_PREDO[["samplesize"]],'predo')
asd_med_IND <- rbind(asd_med_ECCN_ind,asd_med_PREDO_ind)

asd_med_IND$ind_betas <- as.numeric(asd_med_IND$ind_betas)
asd_med_IND$ses <- as.numeric(asd_med_IND$ses)

ind.model.asd <- rma(yi = asd_med_IND$ind_betas, sei = asd_med_IND$ses)

# TOTAL EFFECT
asd_med_ECCN_tot <- as.data.frame(read.csv("ECCN/results_ASD_tot_ppdmed_model_ECCN_0_10.csv"))
asd_med_ECCN_tot <- asd_med_ECCN_tot[,2:5]
asd_med_PREDO_tot <- c(asd_med_PREDO[["total_effect"]]$est, asd_med_PREDO[["total_effect"]]$se, asd_med_PREDO$samplesize,'predo')
asd_med_TOT <- rbind(asd_med_ECCN_tot,asd_med_PREDO_tot)

asd_med_TOT$tot_betas <- as.numeric(asd_med_TOT$tot_betas)
asd_med_TOT$ses <- as.numeric(asd_med_TOT$ses)

tot.model.asd <- rma(yi = asd_med_TOT$tot_betas, sei = asd_med_TOT$ses)

# PROPORTION MEDIATED
asd_med_PREDO_PM <- c(asd_med_PREDO$proportion_mediated,'predo')
asd_med_ECCN_PM <- as.data.frame(read.csv("ECCN/results_ASD_PM_ppdmed_model_ECCN_0_10.csv"))
asd_med_ECCN_PM <- select(asd_med_ECCN_PM, c(tot_betas,cohort))
asd_med_ECCN_PM$tot_betas <- asd_med_ECCN_PM$tot_betas*100
asd_med_PM <- rbind(asd_med_ECCN_PM,asd_med_PREDO_PM)
asd_med_PM[[2]] <- toupper(as.character(asd_med_PM[[2]])) # Convert second column to uppercase
asd_med_PM[(nrow(asd_med_IND)+1),] <- c((as.numeric(ind.model.asd$b)/as.numeric(tot.model.asd$b))*100,"TOTAL")
asd_med_PM$cohort[asd_med_PM$cohort == "GENR"] <- "Generation R"
colnames(asd_med_PM)[colnames(asd_med_PM)=='tot_betas'] <- "pm"
asd_med_PM$pm <- sprintf("%.1f", as.numeric(as.character(asd_med_PM$pm))) # Round first column to 1 decimal place

# Results table
asd_med_DIR$conf.high <- as.numeric(asd_med_DIR$dir_betas) + as.numeric(asd_med_DIR$ses)*1.96
asd_med_DIR$conf.low <- as.numeric(asd_med_DIR$dir_betas) - as.numeric(asd_med_DIR$ses)*1.96
asd_table_dir <- asd_med_DIR[c("dir_betas","conf.low","conf.high","N","cohort")]
asd_table_dir$cohort <- toupper(asd_table_dir$cohort)
asd_table_dir$cohort[asd_table_dir$cohort == "GENR"] <- "Generation R"
colnames(asd_table_dir)[colnames(asd_table_dir)=='dir_betas'] <- "estimate"
asd_table_dir[(nrow(asd_med_DIR)+1),] <- c(as.numeric(dir.model.asd$b), dir.model.asd$ci.lb, dir.model.asd$ci.ub, sum(as.numeric(asd_med_DIR$N)), "TOTAL")

asd_med_IND$conf.high <- as.numeric(asd_med_IND$ind_betas) + as.numeric(asd_med_IND$ses)*1.96
asd_med_IND$conf.low <- as.numeric(asd_med_IND$ind_betas) - as.numeric(asd_med_IND$ses)*1.96
asd_table_ind <- asd_med_IND[c("ind_betas","conf.low","conf.high","N","cohort")]
asd_table_ind$cohort <- toupper(asd_table_ind$cohort)
asd_table_ind$cohort[asd_table_ind$cohort == "GENR"] <- "Generation R"
colnames(asd_table_ind)[colnames(asd_table_ind)=='ind_betas'] <- "estimate"
asd_table_ind[(nrow(asd_med_IND)+1),] <- c(as.numeric(ind.model.asd$b), ind.model.asd$ci.lb, ind.model.asd$ci.ub, sum(as.numeric(asd_med_IND$N)), "TOTAL")
#----------------------------------------PLOT-----------------------------------
# Define mediation plot function
mediation_plot <- function(lab_y, dir.model, ind.model, 
                           height = .5, width = 1.5, graph_label = NA,
                           node_text_size = 10, edge_text_size = 9,
                           node_color = "black", edge_color = "black",
                           node_text_color = "black", edge_text_color = "black",
                           ranksep = .2, minlen = 3) {
  
  require(glue)
  require(DiagrammeR)
  
  # Check if effect is statistically significant (CI does NOT include 0)
  direct_sig <- dir.model$ci.lb[1] > 0 | dir.model$ci.ub[1] < 0
  indirect_sig <- ind.model$ci.lb[1] > 0 | ind.model$ci.ub[1] < 0
  
  direct_label <- paste0(
    "Direct: ",
    sprintf("%.2f", dir.model$beta[1]), " [",
    sprintf("%.2f", dir.model$ci.lb[1]), ", ",
    sprintf("%.2f", dir.model$ci.ub[1]), "]"
  )
  if (direct_sig) {
    direct_label <- paste0("<B><I><FONT COLOR=\"#6e9fd5\">", direct_label, "</FONT></I></B>")
  } else {
    direct_label <- paste0("<I>", direct_label, "</I>")
  }
  
  indirect_label <- paste0(
    "Indirect: ",
    sprintf("%.2f", ind.model$beta[1]), " [",
    sprintf("%.2f", ind.model$ci.lb[1]), ", ",
    sprintf("%.2f", ind.model$ci.ub[1]), "]"
  )
  if (indirect_sig) {
    indirect_label <- paste0("<B><I><FONT COLOR=\"#6e9fd5\">", indirect_label, "</FONT></I></B>")
  } else {
    indirect_label <- paste0("<I>", indirect_label, "</I>")
  }
  
  # Combine labels for the edge
  coef_xy_html <- paste0("<", direct_label, "<BR/>", indirect_label, ">")
  
  # Data for graph
  med_data <- data.frame(
    lab_x   = "Prenatal maternal\\ndepression",
    lab_m   = "Postnatal maternal\\ndepression",
    lab_y   = lab_y,
    coef_xm = "",
    coef_my = "",
    coef_xy = coef_xy_html
  )
  
  # Styling settings
  med_data$height  <- height
  med_data$width   <- width
  med_data$ranksep <- ranksep
  med_data$minlen  <- minlen
  med_data$node_text_size  <- node_text_size
  med_data$edge_text_size  <- edge_text_size
  med_data$node_color   <- node_color
  med_data$edge_color   <- edge_color
  med_data$node_text_color   <- node_text_color
  med_data$edge_text_color   <- edge_text_color
  med_data$graph_label <- ifelse(is.na(graph_label), "", paste0("label = \"", graph_label, "\""))
  
  # Create diagram with GraphViz and HTML-style label
  diagram_out <- glue::glue_data(med_data,
                                 "digraph flowchart {
      fontname = Helvetica
      graph [ranksep = <<ranksep>>, labelloc = \"t\", labeljust = \"c\", <<graph_label>>]

      node [fontname = Helvetica, shape = rectangle, fixedsize = TRUE, width = <<width>>,
      height = <<height>>, fontsize = <<node_text_size>>, color = <<node_color>>, fontcolor = <<node_text_color>>]
        mm [label = '<<lab_m>>']
        xx [label = '<<lab_x>>']
        yy [label = '<<lab_y>>']

      edge [minlen = <<minlen>>, fontname = Helvetica, fontsize = <<edge_text_size>>, 
            color = <<edge_color>>, fontcolor = <<edge_text_color>>, arrowsize = 0.5]
        mm -> yy [label = '<<coef_my>>', color = <<edge_color>>, fontcolor = <<edge_text_color>>];
        xx -> mm [label = '<<coef_xm>>', color = <<edge_color>>, fontcolor = <<edge_text_color>>];
        xx -> yy [label = <<coef_xy>>, fontname = \"Helvetica\", color = <<edge_color>>, fontcolor = <<edge_text_color>>];

      { rank = same; mm }
      { rank = same; xx; yy }
    }", .open = "<<", .close = ">>")
  
  DiagrammeR::grViz(diagram_out)
}

save_mediation_plot_as_jpeg <- function(plot, filename, vwidth = 1050, vheight = 600, zoom = 2) {
  library(htmlwidgets)
  library(webshot2)
  
  # Ensure filename is clean (no extension)
  filename <- tools::file_path_sans_ext(filename)
  
  # Save as HTML
  html_file <- paste0(filename, ".html")
  saveWidget(plot, file = html_file, selfcontained = TRUE)
  
  # Save as JPEG
  jpeg_file <- paste0(filename, ".jpeg")
  webshot2::webshot(url = html_file, file = jpeg_file,
                    vwidth = vwidth, vheight = vheight, zoom = zoom)
}

int_plot <- mediation_plot(lab_y = "Internalising\\nsymptoms", dir.model = dir.model.int,
               ind.model = ind.model.int)

save_mediation_plot_as_jpeg(int_plot, file.path(outputdir, "mediation", "mediation_INT"))

ext_plot <- mediation_plot(lab_y = "Externalising\\nsymptoms", dir.model = dir.model.ext,
                           ind.model = ind.model.ext)

save_mediation_plot_as_jpeg(ext_plot, file.path(outputdir, "mediation", "mediation_EXT"))

adhd_plot <- mediation_plot(lab_y = "ADHD\\nsymptoms", dir.model = dir.model.adhd,
                           ind.model = ind.model.adhd)

save_mediation_plot_as_jpeg(adhd_plot, file.path(outputdir, "mediation", "mediation_ADHD"))

asd_plot <- mediation_plot(lab_y = "ASD\\nsymptoms", dir.model = dir.model.asd,
                            ind.model = ind.model.asd)

save_mediation_plot_as_jpeg(asd_plot, file.path(outputdir, "mediation", "mediation_ASD"))

library(magick)
library(cowplot)

# Read images
img1 <- magick::image_read(file.path(outputdir, "mediation", "mediation_INT.jpeg"))
img2 <- magick::image_read(file.path(outputdir, "mediation", "mediation_EXT.jpeg"))
img3 <- magick::image_read(file.path(outputdir, "mediation", "mediation_ADHD.jpeg"))
img4 <- magick::image_read(file.path(outputdir, "mediation", "mediation_ASD.jpeg"))

# Wrap each image in a ggdraw() and add a subtitle with draw_label()
p1 <- ggdraw() +
  draw_image(img1) +
  draw_label("Internalising symptoms", x = 0.5, y = 0.9, hjust = 0.5, 
             fontface = "bold", size = 18, color = "#2b5d94")

p2 <- ggdraw() +
  draw_image(img2) +
  draw_label("Externalising symptoms", x = 0.5, y = 0.9, hjust = 0.5, 
             fontface = "bold", size = 18, color = "#2b5d94")

p3 <- ggdraw() +
  draw_image(img3) +
  draw_label("ADHD symptoms", x = 0.5, y = 0.9, hjust = 0.5, fontface = "bold", 
             size = 18, color = "#2b5d94")

p4 <- ggdraw() +
  draw_image(img4) +
  draw_label("ASD symptoms", x = 0.5, y = 0.9, hjust = 0.5, fontface = "bold", 
             size = 18, color = "#2b5d94")

# Arrange side by side (or nrow = 2 for vertical)
combined_plot <- plot_grid(p1, p2, p3, p4, 
                           labels = c("a", "b", "c", 'd'), ncol = 2)

# Save the combined plot
ggsave("plots/combined_mediation_plot_08092025.jpeg", combined_plot, width = 14, height = 8)


#------------------------------------Table--------------------------------------
cohorts <- c("ABCD", "ALSPAC", "DNBC", "EDEN", "Generation R",
             "PREDO", "TOTAL")

format_result <- function(df, cohorts) {
  df <- df %>%
    mutate(
      estimate = as.numeric(estimate),
      conf.low = as.numeric(conf.low),
      conf.high = as.numeric(conf.high),
      N = as.character(N),
      `β` = sprintf("%.2f", estimate),
      `95% CI` = paste0("[", sprintf("%.2f", conf.low), ", ", sprintf("%.2f", conf.high), "]")
    ) %>%
    select(Cohort = cohort, N, `β`, `95% CI`)
  
  # Merge with master cohort list to ensure all cohorts are present
  master_cohorts <- data.frame(Cohort = cohorts)
  df <- left_join(master_cohorts, df, by = "Cohort")
  
  return(df)
}

# Combine Direct + Indirect side by side
combine_direct_indirect_wide <- function(direct_df, indirect_df, cohorts) {
  df_direct <- format_result(direct_df, cohorts) %>%
    rename(N_direct = N, β_direct = `β`, CI_direct = `95% CI`)
  
  df_indirect <- format_result(indirect_df, cohorts) %>%
    rename(N_indirect = N, β_indirect = `β`, CI_indirect = `95% CI`)
  
  combined_df <- cbind(df_direct, df_indirect[, -1])  # Exclude Cohort after first
  return(combined_df)
}

add_proportion_mediated <- function(combined_df, pm_df) {
  # Ensure column names match
  pm_df <- pm_df %>% rename(Cohort = cohort, `Proportion Mediated` = pm)
  
  # Convert to numeric
  pm_df <- pm_df %>%
    mutate(`Proportion Mediated` = as.numeric(`Proportion Mediated`))
  
  # Join by Cohort
  combined_df <- left_join(combined_df, pm_df, by = "Cohort")
  
  # Format as percentage with 1 decimal
  combined_df <- combined_df %>%
    mutate(`Proportion Mediated` = paste0(sprintf("%.1f", `Proportion Mediated`), "%"))
  
  return(combined_df)
}

# Create flextable with top-row grouped header
create_direct_indirect_flextable <- function(combined_df) {
  ft <- flextable(combined_df)
  
  # Clear base header labels
  ft <- set_header_labels(ft, values = setNames(rep("", ncol(combined_df)), names(combined_df)))
  
  # Add top header row: Direct vs Indirect
  ft <- add_header_row(
    ft,
    values = c("Cohort", "Direct Effect", "Indirect Effect", "Perc. Med."),
    colwidths = c(1, 3, 3, 1)
  )
  
  ft <- ft %>%
    fontsize(size = 7.5, part = "all") %>%
    set_table_properties(layout = "autofit")
  
  # Optionally bold last row (like totals)
  last_row <- nrow(combined_df)
  ft <- bold(ft, i = last_row, part = "body")
  
  return(ft)
}

# Write to Word
write_direct_indirect_to_word <- function(direct_df, indirect_df, pm_df, cohorts,
                                          filename = "direct_indirect_results.docx") {
  #combined_df <- combine_direct_indirect_wide(direct_df, indirect_df, cohorts)
  #ft <- create_direct_indirect_flextable(combined_df)
  combined_df <- combine_direct_indirect_wide(direct_df, indirect_df, cohorts)
  combined_df <- add_proportion_mediated(combined_df, pm_df)
  ft <- create_direct_indirect_flextable(combined_df)
  
  doc <- read_docx() %>%
    body_add_flextable(ft)
  
  print(doc, target = filename)
}

write_direct_indirect_to_word(int_table_dir, int_table_ind, int_med_PM, cohorts,
                               filename = file.path(outdirtable, "ppd_int_table.docx"))

write_direct_indirect_to_word(ext_table_dir, ext_table_ind, ext_med_PM, cohorts,
                              filename = file.path(outdirtable, "ppd_ext_table.docx"))

write_direct_indirect_to_word(adhd_table_dir, adhd_table_ind, adhd_med_PM, cohorts,
                              filename = file.path(outdirtable, "ppd_adhd_table.docx"))

write_direct_indirect_to_word(asd_table_dir, asd_table_ind, asd_med_PM, cohorts,
                              filename = file.path(outdirtable, "ppd_asd_table.docx"))
#----------------------------Print results (in-text)----------------------------

format_multiple_rma_results <- function(...) {
  models <- list(...)
  model_names <- as.list(substitute(list(...)))[-1]
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    name <- deparse(model_names[[i]])
    est <- as.numeric(model$b)
    lower <- model$ci.lb
    upper <- model$ci.ub
    
    cat(sprintf("%s: β = %.2f, 95%% CI [%.2f, %.2f];\n", name, est, lower, upper))
  }
}

format_multiple_rma_results(dir.model.int, dir.model.ext, dir.model.adhd, 
                            dir.model.asd)

format_multiple_rma_results(ind.model.int, ind.model.ext, ind.model.adhd, 
                            ind.model.asd)


print_custom <- function(df) {
  df_name <- deparse(substitute(df))
  cat("Dataframe:", df_name, "\n")
  
  for (i in seq_len(nrow(df))) {
    col2_val <- df[i, 2]
    col1_val <- df[i, 1]
    cat(sprintf("%s  %s%%\n", col2_val, col1_val))
  }
}

print_custom(int_med_PM)
print_custom(ext_med_PM)
print_custom(adhd_med_PM)
print_custom(asd_med_PM)
