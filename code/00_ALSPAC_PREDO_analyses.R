################################################################################
# Title:    Analyses ALSPAC and PREDO
# Author:   Jana Hermans
# Date:     Created November 2024, last modification on July 2025
# Purpose:  Analysis scripts for ALSPAC and PREDO
# Paper:    Prenatal maternal depression and child developmental, cognitive, and 
#            behavioural outcomes: an individual participant data meta-analysis.
################################################################################
if (!require("lme4")) {
  install.packages("lme4")
  library(lme4)
}
if (!require("mediation")) {
  install.packages("mediation")
  library(mediation)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("car")) {
  install.packages("car")
  library(car)
}
#--------------------------------1) User input----------------------------------
# For these analyses, we need the following variables (if available) 
# (for PREDO only: equivalent variables based on harmonisation protocols):

# "child_id", "sex", "agebirth_m_y", "cob_m", "eusilc_income", "prepreg_weight", 
# "height_m", "ppd", "preg_alc", "preg_dep", "preg_dep_std_continuous",
# "prepreg_dep", "preg_smk", "edu_m_.0", "int_pc_", "int_age_", "ext_pc_", "ext_age_", 
# "adhd_pc_", "adhd_age_", "asd_pc_", "asd_age_",  "fm_pc_", "fm_age_", "gm_pc_", 
# "gm_age_", "nvi_pc_", "nvi_age_", "lan_pc_", "lan_age_", "wm_pc_", "wm_age_"

# All outcomes are percentile scores and the age variables are age at measurement 
# of the outcome in years.

# Sex of the child, country of birth, prenatal smoking, prenatal alcohol use,
# and maternal education are factors

# 1) PLEASE SPECIFY:

# Please adjust the input to this dataframe according to what is available in 
# your cohort. You can specify "1" for available and "0" for unavailable. You
# can specify "yes" for repeated and "no" for non-repeated outcomes.

available_data <- as.data.frame(c('int_', 'ext_', 'adhd_', 'asd_', 'fm_', 'gm_', 'lan_', 'nvi_', 'wm_'))
colnames(available_data) <- "outcome"

#ADJUST here
available_data$available <- c("1", "1", "1", "1", "1", "1", "1", "1", "1")  
available_data$repeated <- c("yes", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes") 
cohort_id <- 'XXX' #replace with 'ALSPAC' or 'PREDO'
outdir <- "/your/output/directory/" # note that you need a '/' at the end

# Convert variables to correct variable types
cohort_data$sex <- as.factor(cohort_data$sex)
cohort_data$edu_m_.0 <- as.factor(cohort_data$edu_m_.0)
cohort_data$cob_m <- as.factor(cohort_data$cob_m)
cohort_data$agebirth_m_y <- as.numeric(cohort_data$agebirth_m_y)
cohort_data$height_m <- as.numeric(cohort_data$height_m)
cohort_data$prepreg_weight <- as.numeric(cohort_data$prepreg_weight)
cohort_data$prepreg_dep <- as.factor(cohort_data$prepreg_dep)
cohort_data$preg_dep <- as.factor(cohort_data$preg_dep)
cohort_data$preg_dep_std_continuous <- as.numeric(cohort_data$preg_dep_std_continuous)
cohort_data$ppd <- as.factor(cohort_data$ppd)
cohort_data$preg_alc <- as.factor(cohort_data$preg_alc)
cohort_data$preg_smk <- as.factor(cohort_data$preg_smk)
cohort_data$int_pc_ <- as.numeric(cohort_data$int_pc_)
cohort_data$int_age_ <- as.numeric(cohort_data$int_age_)
cohort_data$ext_pc_ <- as.numeric(cohort_data$ext_pc_)
cohort_data$ext_age_ <- as.numeric(cohort_data$ext_age_)
cohort_data$adhd_pc_ <- as.numeric(cohort_data$adhd_pc_)
cohort_data$adhd_age_ <- as.numeric(cohort_data$adhd_age_)
cohort_data$asd_pc_ <- as.numeric(cohort_data$asd_pc_)
cohort_data$asd_age_ <- as.numeric(cohort_data$asd_age_)

#2) PLEASE NOTE: 

# All data need to be in long format for the analyses, with all necessary variables 
# in the same dataframe. I.e. lines should be repeated until all repeated outcomes 
# are noted on a single line for each participants. If you need help merging these 
# properly, please let me know!

# Data should look like this format:

#   child_id sex edu_m_.0 cob_m agebirth_m_y eusilc_income prepreg_weight height_m preg_dep preg_dep_std_continuous prepreg_dep ppd preg_alc
#1     20969   1        2     0           37      7.515341       55.80802 171.7185        0              3.47163163           1   0        0
#2     21423   2        2     1           39      7.471516       70.19968 156.8268        1              3.24341862           0   1        1
#3     15512   1        3     2           34      7.558585       63.49994 159.2559        1              2.02988654           1   0        1
#4     13924   1        3     1           35      7.353551       66.59620 165.7788        0              0.18722729           1   0        1
#5     19098   2        1     1           27      7.212370       58.23292 169.1802        1              3.50763657           1   0        1
#6     12073   1        2     0           33      7.585304       56.24872 157.0316        0              2.44262827           0   1        1

# From this point onwards no user input is needed

#------------------------------2a) Add prepreg_BMI-------------------------------
# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
cohort_data$prepreg_BMI <- (cohort_data$prepreg_weight/((cohort_data$height_m/100)^2))
cohort_data$prepreg_BMI <- as.numeric(cohort_data$prepreg_BMI)

#------------------------------2b) Add COB variable------------------------------
# Similar to previous work, we will distinguish between born in the country of the
# cohort (= 1) and born in other country (= 0).

# Current distinction of cob_m is: 0 = born in country of cohort, 1 = born in EU country,
# 2 = born in other country.

if ('cob_m' %in% names(cohort_data)) {
  cohort_data$cob_other_country_f <- ifelse(cohort_data$cob_m == 0, 1, 0)
  cohort_data$cob_other_country_f <- as.factor(cohort_data$cob_other_country_f)
  # Check
  table(cohort_data$cob_m)
  table(cohort_data$cob_other_country_f)
}
#--------------------------2c) Add cumul_dep variable-------------------------
cohort_data$prepreg_dep_toweigh <- as.numeric(as.character(cohort_data$prepreg_dep))
cohort_data$preg_dep_toweigh <- as.numeric(as.character(cohort_data$preg_dep))
cohort_data$ppd_toweigh <- as.numeric(as.character(cohort_data$ppd))

cohort_data$prepreg_dep_toweigh[is.na(cohort_data$prepreg_dep_toweigh)] <- 10
cohort_data$preg_dep_toweigh[is.na(cohort_data$preg_dep_toweigh)] <- 10
cohort_data$ppd_toweigh[is.na(cohort_data$ppd_toweigh)] <- 10

cohort_data$cumul_dep <- (as.numeric(as.character(cohort_data$prepreg_dep_toweigh)) + 
                              as.numeric(as.character(cohort_data$preg_dep_toweigh)) + 
                              as.numeric(as.character(cohort_data$ppd_toweigh)))

# we created a weighted score, allowing up to 1 missing time point
cohort_data$cumul_dep[cohort_data$cumul_dep == 10] <- 0
cohort_data$cumul_dep[cohort_data$cumul_dep == 11] <- 1.5
cohort_data$cumul_dep[cohort_data$cumul_dep == 12] <- 3
cohort_data$cumul_dep[cohort_data$cumul_dep == 20] <- NA
cohort_data$cumul_dep[cohort_data$cumul_dep == 21] <- NA
cohort_data$cumul_dep[cohort_data$cumul_dep == 30] <- NA

cohort_data$cumul_dep <- as.numeric(cohort_data$cumul_dep)

#-----------------------3) Load mediation output function-----------------------
extract_mediation_summary <- function (x) { 
  
  clp <- 100 * x$conf.level
  isLinear.y <- ((class(x$model.y)[1] %in% c("lm", "rq")) || 
                   (inherits(x$model.y, "glm") && x$model.y$family$family == 
                      "gaussian" && x$model.y$family$link == "identity") || 
                   (inherits(x$model.y, "survreg") && x$model.y$dist == 
                      "gaussian"))
  
  printone <- !x$INT && isLinear.y
  
  if (printone) {
    
    smat <- c(x$d1, x$d1.ci, x$d1.p)
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    
    rownames(smat) <- c("ACME", "ADE", "Total Effect", "Prop. Mediated")
    
  } else {
    smat <- c(x$d0, x$d0.ci, x$d0.p)
    smat <- rbind(smat, c(x$d1, x$d1.ci, x$d1.p))
    smat <- rbind(smat, c(x$z0, x$z0.ci, x$z0.p))
    smat <- rbind(smat, c(x$z1, x$z1.ci, x$z1.p))
    smat <- rbind(smat, c(x$tau.coef, x$tau.ci, x$tau.p))
    smat <- rbind(smat, c(x$n0, x$n0.ci, x$n0.p))
    smat <- rbind(smat, c(x$n1, x$n1.ci, x$n1.p))
    smat <- rbind(smat, c(x$d.avg, x$d.avg.ci, x$d.avg.p))
    smat <- rbind(smat, c(x$z.avg, x$z.avg.ci, x$z.avg.p))
    smat <- rbind(smat, c(x$n.avg, x$n.avg.ci, x$n.avg.p))
    
    rownames(smat) <- c("ACME (control)", "ACME (treated)", 
                        "ADE (control)", "ADE (treated)", "Total Effect", 
                        "Prop. Mediated (control)", "Prop. Mediated (treated)", 
                        "ACME (average)", "ADE (average)", "Prop. Mediated (average)")
    
  }
  
  colnames(smat) <- c("Estimate", paste(clp, "% CI Lower", sep = ""), 
                      paste(clp, "% CI Upper", sep = ""), "p-value")
  smat
  
}

#----------------------------4) Check descriptives------------------------------
# Select the study sample from cohort data. Inclusion criteria:
# 1) Data on exposure
# 2) At least one observation for any of the nine outcomes 
# (i.e. participants with no outcome data at all will be excluded)

mental_health <- c()
for (i in 1:4) 
{
  if (available_data[i,2] == "1") {
    outcome <- available_data[i,1]
    mental_health <- append(mental_health,outcome)
  }
}

cognition <- c()
for (i in 5:9) 
{
  if (available_data[i,2] == "1") {
    outcome <- available_data[i,1]
    cognition <- append(cognition,outcome)
  }
}
columns_to_check <- c(mental_health, cognition)
columns_to_check <- unlist(lapply(columns_to_check, function(col) paste0(col, "pc_")))

# Filter rows with a non-NA value in the exposure column 
study_sample <- cohort_data[!is.na(cohort_data['preg_dep']), ]
# Filter rows with at least one non-NA value in the outcome columns
study_sample <- study_sample[rowSums(!is.na(study_sample[columns_to_check])) > 0, ]
study_sample_unique_cases <- study_sample[!duplicated(study_sample$child_id),]

# Filter rows with a non-NA value in the exposure column 
covar_list <- c('sex','edu_m_.0','agebirth_m_y','preg_alc','preg_smk','prepreg_BMI')
COMPLETE_COVAR_sample <- cohort_data[!is.na(cohort_data['preg_dep']), ]
for (var in covar_list) {
  COMPLETE_COVAR_sample <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample[var]), ]
}
# Filter rows with at least one non-NA value in the outcome columns
COMPLETE_COVAR_sample <- COMPLETE_COVAR_sample[rowSums(!is.na(COMPLETE_COVAR_sample[columns_to_check])) > 0, ]
COMPLETE_COVAR_sample_unique_cases <- COMPLETE_COVAR_sample[!duplicated(COMPLETE_COVAR_sample$child_id),]

# Create dataset of excluded subjects to compare descriptives
excluded_sample <- cohort_data[!(cohort_data$child_id %in% COMPLETE_COVAR_sample_unique_cases$child_id), ]
excluded_sample_unique_cases <- excluded_sample[!duplicated(excluded_sample$child_id),]

# Create descriptives to save
descriptives <- list(
  full_sample_N = nrow(cohort_data[!duplicated(cohort_data$child_id),]),
  study_sample_N = nrow(study_sample_unique_cases),
  study_sample_N_CC = nrow(COMPLETE_COVAR_sample_unique_cases),
  study_sample_N_NC = nrow(excluded_sample_unique_cases),
  child_sex = list(distribution = table(study_sample_unique_cases$sex), 
                   missing = sum(is.na(study_sample_unique_cases$sex))),
  child_sex_CC = list(distribution = table(COMPLETE_COVAR_sample_unique_cases$sex), 
                      missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$sex))),
  child_sex_NC = list(distribution = table(excluded_sample_unique_cases$sex), 
                      missing = sum(is.na(excluded_sample_unique_cases$sex))),
  agebirth_m_y = list(mean = mean(study_sample_unique_cases$agebirth_m_y, na.rm = T), 
                      SD = sd(study_sample_unique_cases$agebirth_m_y, na.rm = T),
                      missing = sum(is.na(study_sample_unique_cases$agebirth_m_y))),
  agebirth_m_y_CC = list(mean = mean(COMPLETE_COVAR_sample_unique_cases$agebirth_m_y, na.rm = T), 
                         SD = sd(COMPLETE_COVAR_sample_unique_cases$agebirth_m_y, na.rm = T),
                         missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$agebirth_m_y))),
  agebirth_m_y_NC = list(mean = mean(excluded_sample_unique_cases$agebirth_m_y, na.rm = T), 
                         SD = sd(excluded_sample_unique_cases$agebirth_m_y, na.rm = T),
                         missing = sum(is.na(excluded_sample_unique_cases$agebirth_m_y))),
  edu_m_.0 = list(distribution = table(study_sample_unique_cases$edu_m_.0), 
                  missing = sum(is.na(study_sample_unique_cases$edu_m_.0))),
  edu_m_.0_CC = list(distribution = table(COMPLETE_COVAR_sample_unique_cases$edu_m_.0), 
                     missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$edu_m_.0))),
  edu_m_.0_NC = list(distribution = table(excluded_sample_unique_cases$edu_m_.0), 
                     missing = sum(is.na(excluded_sample_unique_cases$edu_m_.0))),
  prepreg_BMI_CC = list(mean = mean(study_sample_unique_cases$prepreg_BMI, na.rm = T), 
                        SD = sd(study_sample_unique_cases$prepreg_BMI, na.rm = T),
                        missing = sum(is.na(study_sample_unique_cases$prepreg_BMI))),
  prepreg_BMI_CC = list(mean = mean(COMPLETE_COVAR_sample_unique_cases$prepreg_BMI, na.rm = T), 
                        SD = sd(COMPLETE_COVAR_sample_unique_cases$prepreg_BMI, na.rm = T),
                        missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$prepreg_BMI))),
  prepreg_BMI_NC = list(mean = mean(excluded_sample_unique_cases$prepreg_BMI, na.rm = T), 
                        SD = sd(excluded_sample_unique_cases$prepreg_BMI, na.rm = T),
                        missing = sum(is.na(excluded_sample_unique_cases$prepreg_BMI))),
  preg_dep = list(distribution = table(study_sample_unique_cases$preg_dep), 
                  missing = sum(is.na(study_sample_unique_cases$preg_dep))),
  preg_dep_CC = list(distribution = table(COMPLETE_COVAR_sample_unique_cases$preg_dep), 
                     missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$preg_dep))),
  preg_dep_NC = list(distribution = table(excluded_sample_unique_cases$preg_dep), 
                     missing = sum(is.na(excluded_sample_unique_cases$preg_dep))),
  preg_alc = list(distribution = table(study_sample_unique_cases$preg_alc), 
                  missing = sum(is.na(study_sample_unique_cases$preg_alc))),
  preg_alc_CC = list(distribution = table(COMPLETE_COVAR_sample_unique_cases$preg_alc), 
                     missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$preg_alc))),
  preg_alc_NC = list(distribution = table(excluded_sample_unique_cases$preg_alc), 
                     missing = sum(is.na(excluded_sample_unique_cases$preg_alc))),
  preg_smk = list(distribution = table(study_sample_unique_cases$preg_smk), 
                  missing = sum(is.na(study_sample_unique_cases$preg_smk))),
  preg_smk_CC = list(distribution = table(COMPLETE_COVAR_sample_unique_cases$preg_smk), 
                     missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$preg_smk))),
  preg_smk_NC = list(distribution = table(excluded_sample_unique_cases$preg_smk), 
                     missing = sum(is.na(excluded_sample_unique_cases$preg_smk)))
)

if ('cob_other_country_f' %in% names(study_sample_unique_cases)) {
  descriptives$cob_other_country_f = list(
    distribution = table(study_sample_unique_cases$cob_other_country_f), 
    missing = sum(is.na(study_sample_unique_cases$cob_other_country_f)))
  descriptives$cob_other_country_f_CC = list(
    distribution = table(COMPLETE_COVAR_sample_unique_cases$cob_other_country_f), 
    missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$cob_other_country_f)))
  descriptives$cob_other_country_f_NC = list(
    distribution = table(excluded_sample_unique_cases$cob_other_country_f), 
    missing = sum(is.na(excluded_sample_unique_cases$cob_other_country_f)))
}
if ('preg_dep_std_continuous' %in% names(study_sample_unique_cases)) {
  descriptives$preg_dep_std_continuous = 
    list(mean = mean(study_sample_unique_cases$preg_dep_std_continuous, na.rm = T),
         SD = sd(study_sample_unique_cases$preg_dep_std_continuous, na.rm = T),
         missing = sum(is.na(study_sample_unique_cases$preg_dep_std_continuous)))
  descriptives$preg_dep_std_continuous_CC = 
    list(mean = mean(COMPLETE_COVAR_sample_unique_cases$preg_dep_std_continuous, na.rm = T),
         SD = sd(COMPLETE_COVAR_sample_unique_cases$preg_dep_std_continuous, na.rm = T),
         missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$preg_dep_std_continuous)))
  descriptives$preg_dep_std_continuous_NC = 
    list(mean = mean(excluded_sample_unique_cases$preg_dep_std_continuous, na.rm = T),
         SD = sd(excluded_sample_unique_cases$preg_dep_std_continuous, na.rm = T),
         missing = sum(is.na(excluded_sample_unique_cases$preg_dep_std_continuous)))
}
if ('ppd' %in% names(study_sample_unique_cases)) {
  descriptives$ppd = list(
    distribution = table(study_sample_unique_cases$ppd), 
    missing = sum(is.na(study_sample_unique_cases$ppd)))
  descriptives$ppd_CC = list(
    distribution = table(COMPLETE_COVAR_sample_unique_cases$ppd), 
    missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$ppd)))
  descriptives$ppd_NC = list(
    distribution = table(excluded_sample_unique_cases$ppd), 
    missing = sum(is.na(excluded_sample_unique_cases$ppd)))
}
if ('prepreg_dep' %in% names(study_sample_unique_cases)) {
  descriptives$prepreg_dep = list(
    distribution = table(study_sample_unique_cases$prepreg_dep), 
    missing = sum(is.na(study_sample_unique_cases$prepreg_dep)))
  descriptives$prepreg_dep_CC = list(
    distribution = table(COMPLETE_COVAR_sample_unique_cases$prepreg_dep), 
    missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$prepreg_dep)))
  descriptives$prepreg_dep_NC = list(
    distribution = table(excluded_sample_unique_cases$prepreg_dep), 
    missing = sum(is.na(excluded_sample_unique_cases$prepreg_dep)))
}
if ('cumul_dep' %in% names(study_sample_unique_cases)) {
  descriptives$cumul_dep = list(
    distribution = table(study_sample_unique_cases$cumul_dep), 
    missing = sum(is.na(study_sample_unique_cases$cumul_dep)))
  descriptives$cumul_dep_CC = list(
    distribution = table(COMPLETE_COVAR_sample_unique_cases$cumul_dep), 
    missing = sum(is.na(COMPLETE_COVAR_sample_unique_cases$cumul_dep)))
  descriptives$cumul_dep_NC = list(
    distribution = table(excluded_sample_unique_cases$cumul_dep), 
    missing = sum(is.na(excluded_sample_unique_cases$cumul_dep)))
}

saveRDS(descriptives, file=paste0(outdir, "descriptives_with_excl_", cohort_id, ".RData"))

if ('int_pc_' %in% names(study_sample)) {
  int_sample <- study_sample[!is.na(study_sample['int_pc_']), ]
  int_sample <- int_sample[!duplicated(int_sample$child_id),]
  descriptives$N_int_pc_ = nrow(int_sample)
  descriptives$int_pc_ = list(mean = mean(study_sample$int_pc_, na.rm = T), 
                              SD = sd(study_sample$int_pc_, na.rm = T))
  descriptives$int_age_ = list(mean = mean(study_sample$int_age_, na.rm = T), 
                               SD = sd(study_sample$int_age_, na.rm = T))
  int_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['int_pc_']), ]
  int_sample_CC <- int_sample_CC[!duplicated(int_sample_CC$child_id),]
  descriptives$N_int_pc_CC = nrow(int_sample_CC)
  descriptives$int_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$int_pc_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$int_pc_, na.rm = T))
  descriptives$int_age_CC = list(mean = mean(COMPLETE_COVAR_sample$int_age_, na.rm = T), 
                               SD = sd(COMPLETE_COVAR_sample$int_age_, na.rm = T))
}
if ('ext_pc_' %in% names(study_sample)) {
  ext_sample <- study_sample[!is.na(study_sample['ext_pc_']), ]
  ext_sample <- ext_sample[!duplicated(ext_sample$child_id),]
  descriptives$N_ext_pc_ = nrow(ext_sample)
  descriptives$ext_pc_ = list(mean = mean(study_sample$ext_pc_, na.rm = T), 
                              SD = sd(study_sample$ext_pc_, na.rm = T))
  descriptives$ext_age_ = list(mean = mean(study_sample$ext_age_, na.rm = T), 
                               SD = sd(study_sample$ext_age_, na.rm = T))
  ext_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['ext_pc_']), ]
  ext_sample_CC <- ext_sample_CC[!duplicated(ext_sample_CC$child_id),]
  descriptives$N_ext_pc_CC = nrow(ext_sample_CC)
  descriptives$ext_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$ext_pc_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$ext_pc_, na.rm = T))
  descriptives$ext_age_CC = list(mean = mean(COMPLETE_COVAR_sample$ext_age_, na.rm = T), 
                               SD = sd(COMPLETE_COVAR_sample$ext_age_, na.rm = T))
}
if ('adhd_pc_' %in% names(study_sample)) {
  adhd_sample <- study_sample[!is.na(study_sample['adhd_pc_']), ]
  adhd_sample <- adhd_sample[!duplicated(adhd_sample$child_id),]
  descriptives$N_adhd_pc_ = nrow(adhd_sample)
  descriptives$adhd_pc_ = list(mean = mean(study_sample$adhd_pc_, na.rm = T), 
                               SD = sd(study_sample$adhd_pc_, na.rm = T))
  descriptives$adhd_age_ = list(mean = mean(study_sample$adhd_age_, na.rm = T), 
                                SD = sd(study_sample$adhd_age_, na.rm = T))
  adhd_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['adhd_pc_']), ]
  adhd_sample_CC <- adhd_sample_CC[!duplicated(adhd_sample_CC$child_id),]
  descriptives$N_adhd_pc_CC = nrow(adhd_sample_CC)
  descriptives$adhd_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$adhd_pc_, na.rm = T), 
                               SD = sd(COMPLETE_COVAR_sample$adhd_pc_, na.rm = T))
  descriptives$adhd_age_CC = list(mean = mean(COMPLETE_COVAR_sample$adhd_age_, na.rm = T), 
                                SD = sd(COMPLETE_COVAR_sample$adhd_age_, na.rm = T))
}
if ('asd_pc_' %in% names(study_sample)) {
  asd_sample <- study_sample[!is.na(study_sample['asd_pc_']), ]
  asd_sample <- asd_sample[!duplicated(asd_sample$child_id),]
  descriptives$N_asd_pc_ = nrow(asd_sample)
  descriptives$asd_pc_ = list(mean = mean(study_sample$asd_pc_, na.rm = T), 
                              SD = sd(study_sample$asd_pc_, na.rm = T))
  descriptives$asd_age_ = list(mean = mean(study_sample$asd_age_, na.rm = T), 
                               SD = sd(study_sample$asd_age_, na.rm = T))
  asd_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['asd_pc_']), ]
  asd_sample_CC <- asd_sample_CC[!duplicated(asd_sample_CC$child_id),]
  descriptives$N_asd_pc_CC = nrow(asd_sample_CC)
  descriptives$asd_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$asd_pc_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$asd_pc_, na.rm = T))
  descriptives$asd_age_CC = list(mean = mean(COMPLETE_COVAR_sample$asd_age_, na.rm = T), 
                               SD = sd(COMPLETE_COVAR_sample$asd_age_, na.rm = T))
}
if ('fm_pc_' %in% names(study_sample)) {
  fm_sample <- study_sample[!is.na(study_sample['fm_pc_']), ]
  fm_sample <- fm_sample[!duplicated(fm_sample$child_id),]
  descriptives$N_fm_pc_ = nrow(fm_sample)
  descriptives$fm_pc_ = list(mean = mean(study_sample$fm_pc_, na.rm = T), 
                             SD = sd(study_sample$fm_pc_, na.rm = T))
  descriptives$fm_age_ = list(mean = mean(study_sample$fm_age_, na.rm = T), 
                              SD = sd(study_sample$fm_age_, na.rm = T))
  fm_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['fm_pc_']), ]
  fm_sample_CC <- fm_sample_CC[!duplicated(fm_sample_CC$child_id),]
  descriptives$N_fm_pc_CC = nrow(fm_sample_CC)
  descriptives$fm_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$fm_pc_, na.rm = T), 
                             SD = sd(COMPLETE_COVAR_sample$fm_pc_, na.rm = T))
  descriptives$fm_age_CC = list(mean = mean(COMPLETE_COVAR_sample$fm_age_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$fm_age_, na.rm = T))
}
if ('gm_pc_' %in% names(study_sample)) {
  gm_sample <- study_sample[!is.na(study_sample['gm_pc_']), ]
  gm_sample <- gm_sample[!duplicated(gm_sample$child_id),]
  descriptives$N_gm_pc_ = nrow(gm_sample)
  descriptives$gm_pc_ = list(mean = mean(study_sample$gm_pc_, na.rm = T), 
                             SD = sd(study_sample$gm_pc_, na.rm = T))
  descriptives$gm_age_ = list(mean = mean(study_sample$gm_age_, na.rm = T), 
                              SD = sd(study_sample$gm_age_, na.rm = T))
  gm_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['gm_pc_']), ]
  gm_sample_CC <- gm_sample_CC[!duplicated(gm_sample_CC$child_id),]
  descriptives$N_gm_pc_CC = nrow(gm_sample_CC)
  descriptives$gm_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$gm_pc_, na.rm = T), 
                             SD = sd(COMPLETE_COVAR_sample$gm_pc_, na.rm = T))
  descriptives$gm_age_CC = list(mean = mean(COMPLETE_COVAR_sample$gm_age_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$gm_age_, na.rm = T))
}
if ('lan_pc_' %in% names(study_sample)) {
  lan_sample <- study_sample[!is.na(study_sample['lan_pc_']), ]
  lan_sample <- lan_sample[!duplicated(lan_sample$child_id),]
  descriptives$N_lan_pc_ = nrow(lan_sample)
  descriptives$lan_pc_ = list(mean = mean(study_sample$lan_pc_, na.rm = T), 
                              SD = sd(study_sample$lan_pc_, na.rm = T))
  descriptives$lan_age_ = list(mean = mean(study_sample$lan_age_, na.rm = T), 
                               SD = sd(study_sample$lan_age_, na.rm = T))
  lan_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['lan_pc_']), ]
  lan_sample_CC <- lan_sample_CC[!duplicated(lan_sample_CC$child_id),]
  descriptives$N_lan_pc_CC = nrow(lan_sample_CC)
  descriptives$lan_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$lan_pc_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$lan_pc_, na.rm = T))
  descriptives$lan_age_CC = list(mean = mean(COMPLETE_COVAR_sample$lan_age_, na.rm = T), 
                               SD = sd(COMPLETE_COVAR_sample$lan_age_, na.rm = T))
}
if ('nvi_pc_' %in% names(study_sample)) {
  nvi_sample <- study_sample[!is.na(study_sample['nvi_pc_']), ]
  nvi_sample <- nvi_sample[!duplicated(nvi_sample$child_id),]
  descriptives$N_nvi_pc_ = nrow(nvi_sample)
  descriptives$nvi_pc_ = list(mean = mean(study_sample$nvi_pc_, na.rm = T), 
                              SD = sd(study_sample$nvi_pc_, na.rm = T))
  descriptives$nvi_age_ = list(mean = mean(study_sample$nvi_age_, na.rm = T), 
                               SD = sd(study_sample$nvi_age_, na.rm = T))
  nvi_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['nvi_pc_']), ]
  nvi_sample_CC <- nvi_sample_CC[!duplicated(nvi_sample_CC$child_id),]
  descriptives$N_nvi_pc_CC = nrow(nvi_sample_CC)
  descriptives$nvi_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$nvi_pc_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$nvi_pc_, na.rm = T))
  descriptives$nvi_age_CC = list(mean = mean(COMPLETE_COVAR_sample$nvi_age_, na.rm = T), 
                               SD = sd(COMPLETE_COVAR_sample$nvi_age_, na.rm = T))
}
if ('wm_pc_' %in% names(study_sample)) {
  wm_sample <- study_sample[!is.na(study_sample['wm_pc_']), ]
  wm_sample <- wm_sample[!duplicated(wm_sample$child_id),]
  descriptives$N_wm_pc_ = nrow(wm_sample)
  descriptives$wm_pc_ = list(mean = mean(study_sample$wm_pc_, na.rm = T), 
                             SD = sd(study_sample$wm_pc_, na.rm = T))
  descriptives$wm_age_ = list(mean = mean(study_sample$wm_age_, na.rm = T), 
                              SD = sd(study_sample$wm_age_, na.rm = T))
  wm_sample_CC <- COMPLETE_COVAR_sample[!is.na(COMPLETE_COVAR_sample['wm_pc_']), ]
  wm_sample_CC <- wm_sample_CC[!duplicated(wm_sample_CC$child_id),]
  descriptives$N_wm_pc_CC = nrow(wm_sample_CC)
  descriptives$wm_pc_CC = list(mean = mean(COMPLETE_COVAR_sample$wm_pc_, na.rm = T), 
                             SD = sd(COMPLETE_COVAR_sample$wm_pc_, na.rm = T))
  descriptives$wm_age_CC = list(mean = mean(COMPLETE_COVAR_sample$wm_age_, na.rm = T), 
                              SD = sd(COMPLETE_COVAR_sample$wm_age_, na.rm = T))
}

saveRDS(descriptives, file=paste0(outdir, "descriptives_", cohort_id, ".RData"))

# Make plots of the age distribution per outcome
for (i in 1:nrow(available_data)){
  if(available_data[i,2] == 1){
    outcome <- available_data[i,1]
    if (outcome == 'int_') {outcome_name <- 'internalising symptoms'
    age_distr_plot <- ggplot(data=study_sample, aes(x=int_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'ext_') {outcome_name <- 'externalising symptoms'
    age_distr_plot <- ggplot(data=study_sample, aes(x=ext_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'adhd_') {outcome_name <- 'ADHD symptoms'
    age_distr_plot <- ggplot(data=study_sample, aes(x=adhd_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'asd_') {outcome_name <- 'ASD symptoms'
    age_distr_plot <- ggplot(data=study_sample, aes(x=asd_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'fm_') {outcome_name <- 'fine motor skills'
    age_distr_plot <- ggplot(data=study_sample, aes(x=fm_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'gm_') {outcome_name <- 'gross motor skills'
    age_distr_plot <- ggplot(data=study_sample, aes(x=gm_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'lan_') {outcome_name <- 'language skills'
    age_distr_plot <- ggplot(data=study_sample, aes(x=lan_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'nvi_') {outcome_name <- 'non-verbal intelligence'
    age_distr_plot <- ggplot(data=study_sample, aes(x=nvi_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'wm_') {outcome_name <- 'working memory'
    age_distr_plot <- ggplot(data=study_sample, aes(x=wm_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, ".tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
  }
}


# Make plots of the age distribution per outcome (sample with compelte covariates)
for (i in 1:nrow(available_data)){
  if(available_data[i,2] == 1){
    outcome <- available_data[i,1]
    if (outcome == 'int_') {outcome_name <- 'internalising symptoms'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=int_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'ext_') {outcome_name <- 'externalising symptoms'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=ext_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'adhd_') {outcome_name <- 'ADHD symptoms'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=adhd_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'asd_') {outcome_name <- 'ASD symptoms'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=asd_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'fm_') {outcome_name <- 'fine motor skills'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=fm_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'gm_') {outcome_name <- 'gross motor skills'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=gm_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'lan_') {outcome_name <- 'language skills'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=lan_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'nvi_') {outcome_name <- 'non-verbal intelligence'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=nvi_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
    if (outcome == 'wm_') {outcome_name <- 'working memory'
    age_distr_plot <- ggplot(data=COMPLETE_COVAR_sample, aes(x=wm_age_)) +
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = .5) +
      labs(title = paste0('Age distribution ', outcome_name, ' in ', cohort_id), 
           x = 'age at outcome', y = 'no of oberservations') +
      theme_classic() +
      theme(plot.title=element_text(hjust=.5)) 
    ggsave(filename = paste0(outdir, "age_distr_plot_", outcome, cohort_id, "_CC.tiff"), 
           width = 7, plot = age_distr_plot, height = 6, device='tiff', dpi=700)
    }
  }
}

#-------------------------5) Create complete datasets---------------------------
# Create different complete datasets per outcome
ind_complete_datasets <- vector(mode = "list", length = nrow(available_data))
for (i in 1:nrow(available_data)) 
{
  if (available_data[i,2] == "1") {
    outcome <- available_data[i,1]
    dataset_complete <- as.data.frame(cohort_data[,c('child_id','sex','edu_m_.0',
                                                     'preg_dep','agebirth_m_y',
                                                     'agebirth_m_y','preg_alc',
                                                     'preg_smk','prepreg_BMI',
                                                     paste0(outcome,'pc_'),
                                                     paste0(outcome,'age_')),])
    dataset_complete <- dataset_complete[complete.cases(dataset_complete), ]
    name <- paste("complete_cases", outcome, sep = "_")
    assign(name, dataset_complete)
    ind_complete_datasets[[i]] <- dataset_complete
  }
  else
  {
    ind_complete_datasets[[i]] <- 'not available'
  }
}

#-------------------------------6) Minimal LMM and LMs--------------------------
# Create a dataframe for the output
results_minimal <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the minimally adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ + (1|child_id)")
        model0.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output0 <- summary(model0.lmer)
        results <- as.data.frame(output0$coefficients)
        results <- results["preg_dep1",]
        results$type <- 'LMER'
        mf0 <- model.frame(model0.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_")
        model0.glm <- glm(formula, family = gaussian, data = cohort_data)
        output0 <- summary(model0.glm)
        results <- as.data.frame(output0$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        mf0 <- model.frame(model0.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf0)
      results_minimal[i,] <- results
    }
    else {
      results_minimal[i,] <- "not available"
    }
  }
}
colnames(results_minimal) <- colnames(results)

# SAVE OUTPUT
write.csv(results_minimal, paste0(outdir, "results_minimal_model_", cohort_id, ".csv"), row.names =  F)

# Create a dataframe for the output
results_minimal_CC <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the minimally adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ + (1|child_id)")
        model0.lmer <- lmer(formula, data = ind_complete_datasets[[i]], REML = TRUE)
        output0 <- summary(model0.lmer)
        results <- as.data.frame(output0$coefficients)
        results <- results["preg_dep1",]
        results$type <- 'LMER'
        mf0 <- model.frame(model0.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_")
        model0.glm <- glm(formula, family = gaussian, data = ind_complete_datasets[[i]])
        output0 <- summary(model0.glm)
        results <- as.data.frame(output0$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        mf0 <- model.frame(model0.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf0)
      results_minimal_CC[i,] <- results
    }
    else {
      results_minimal_CC[i,] <- "not available"
    }
  }
}
colnames(results_minimal_CC) <- colnames(results)

# SAVE OUTPUT
write.csv(results_minimal_CC, paste0(outdir, "results_minimal_model_CC_", cohort_id, ".csv"), row.names =  F)

#---------------------------7a) Main models LMM and LMs-----------------------
# Create a dataframe for the output
results_fulladj <- data.frame(matrix(NA, nrow = 9, ncol = 6))
vif_list <- vector("list", 9)
names(vif_list) <- c("int_pc_", "ext_pc_", "adhd_pc_", "asd_pc_", "fm_pc_",
                     "gm_pc_", "lan_pc_", "nvi_pc_", "wm_pc_")

# Run the fully adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output1 <- summary(model1.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["preg_dep1",]
        results$type <- 'LMER'
        png(filename = paste0(outdir, "res_pred_plot_", outcome, cohort_id, ".png"), 
            width = 1000, height = 700, res = 100)
        plot(predict(model1.lmer),residuals(model1.lmer),
             main= paste0("Residuals ", outcome, " model"),
             xlab="predicted", ylab="residuals")
        dev.off()
        mf1 <- model.frame(model1.lmer)
        res_distr_plot <- ggplot(data = mf1, aes(x = resid(model1.lmer))) +
          geom_histogram(fill = 'steelblue', color = 'black', binwidth = 5) +
          labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
        ggsave(filename = paste0(outdir, "res_distr_plot_", outcome, cohort_id, ".tiff"), 
               width = 10, plot = res_distr_plot, height = 7, device='tiff', dpi=700)
        vif_list[[i]] <- vif(model1.lmer)
        
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        png(filename = paste0(outdir, "res_pred_plot_", outcome, cohort_id, ".png"), width = 1000, 
            height = 700, res = 100)
        plot(predict(model1.glm),residuals(model1.glm), 
             main= paste0("Residuals ", outcome, " model"),
             xlab="predicted", ylab="residuals")
        dev.off()
        mf1 <- model.frame(model1.glm)
        res_distr_plot <- ggplot(data = mf1, aes(x = resid(model1.glm))) +
          geom_histogram(fill = 'steelblue', color = 'black', binwidth = 5) +
          labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
        ggsave(filename = paste0(outdir, "res_distr_plot_", outcome, cohort_id, ".tiff"), 
               width = 10, plot = res_distr_plot, height = 7, device='tiff', dpi=700)
        vif_list[[i]] <- vif(model1.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1)
      results_fulladj[i,] <- results
    }
    else {
      results_fulladj[i,] <- "not available"
    }
  }
}
colnames(results_fulladj) <- colnames(results)

# SAVE OUTPUT
write.csv(results_fulladj, paste0(outdir, "results_fulladj_model_", cohort_id, ".csv"), row.names =  F)
saveRDS(vif_list, file="vif_list.RData")

#-----------------7b) COMPLETE (single) main models LMM and LMs-----------------
# Create a dataframe for the output
results_COMPLETE_single_outcome <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:9) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1.lmer <- lmer(formula, data = ind_complete_datasets[[i]], REML = F)
        output1 <- summary(model1.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["preg_dep1",]
        results$type <- 'LMER'
        mf1 <- model.frame(model1.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1.glm <- glm(formula, family = gaussian, data = ind_complete_datasets[[i]])
        output1 <- summary(model1.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        mf1 <- model.frame(model1.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1)
      results_COMPLETE_single_outcome[i,] <- results
    }
    else {
      results_COMPLETE_single_outcome[i,] <- "not available"
    }
  }
}
colnames(results_COMPLETE_single_outcome) <- colnames(results)

# SAVE OUTPUT
write.csv(results_COMPLETE_single_outcome, paste0(outdir, "results_COMPLETE_single_outcome_", cohort_id, ".csv"), row.names =  F)
#--------------------------8a) Sex interaction LMM and LMs-----------------------
# Create a dataframe for the output
results_sexint <- data.frame(matrix(NA, nrow = nrow(available_data), ncol = 6))
results_boys <- data.frame(matrix(NA, nrow = nrow(available_data), ncol = 6))
results_girls <- data.frame(matrix(NA, nrow = nrow(available_data), ncol = 6))
colnames(results_sexint) <- colnames(results_boys) <- colnames(results_girls) <- 
  c("Estimate", "Std.Error", "t.value", "type", "samplesize", "outcome")

# Run the fully adjusted  model with sex interaction on your data (taking into account 
# what you specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  if (available_data[i, 2] == "1") {
    outcome <- available_data[i, 1]
    
    if (available_data[i, 3] == "yes") {
      formula <- paste0(outcome, "pc_ ~ preg_dep*sex + ", outcome, "age_ + 
                        edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                        prepreg_BMI + (1|child_id)")
      model <- lmer(as.formula(formula), data = cohort_data, REML = TRUE)
      model_type <- "LMER"
      sample_size <- length(unique(model.frame(model)$child_id))
      
    } else if (available_data[i, 3] == "no") {
      formula <- paste0(outcome, "pc_ ~ preg_dep*sex + ", outcome, "age_ +
                        edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
      model <- glm(as.formula(formula), data = cohort_data, family = gaussian)
      model_type <- "GLM"
      sample_size <- nrow(model.frame(model))
      
    } else {
      next
    }
    
    coefs <- summary(model)$coefficients
    vcov_mat <- vcov(model)
    
    # Extract main effect and interaction
    b_boys <- coefs["preg_dep1", "Estimate"]
    se_boys <- coefs["preg_dep1", "Std. Error"]
    t_boys <- ifelse("t value" %in% colnames(coefs), coefs["preg_dep1", "t value"], NA)
    
    b_int <- coefs["preg_dep1:sex2", "Estimate"]
    se_int <- coefs["preg_dep1:sex2", "Std. Error"]
    t_int <- ifelse("t value" %in% colnames(coefs), coefs["preg_dep1:sex2", "t value"], NA)
    
    # Combined effect in girls
    b_girls <- b_boys + b_int
    var_boys <- vcov_mat["preg_dep1", "preg_dep1"]
    var_int <- vcov_mat["preg_dep1:sex2", "preg_dep1:sex2"]
    cov_both <- vcov_mat["preg_dep1", "preg_dep1:sex2"]
    se_girls <- sqrt(var_boys + var_int + 2 * cov_both)
    t_girls <- b_girls / se_girls
    
    # Save interaction
    results_sexint[i, ] <- c(b_int, se_int, t_int, model_type, sample_size, outcome)
    
    # Save main effect in boys
    results_boys[i, ] <- c(b_boys, se_boys, t_boys, model_type, sample_size, outcome)
    
    # Save combined effect in girls
    results_girls[i, ] <- c(b_girls, se_girls, t_girls, model_type, sample_size, outcome)
    
  } else {
    results_sexint[i, ] <- "not available"
    results_boys[i, ] <- "not available"
    results_girls[i, ] <- "not available"
  }
}

# SAVE OUTPUT
write.csv(results_sexint, paste0(outdir, "results_sexint_model_", cohort_id, ".csv"), row.names =  F)
write.csv(results_boys, paste0(outdir, "results_boys_model_", cohort_id, ".csv"), row.names =  F)
write.csv(results_girls, paste0(outdir, "results_girls_model_", cohort_id, ".csv"), row.names =  F)

#-----------------8b) COMPLETE (single) main models LMM and LMs-----------------
# Create a dataframe for the output
results_sexint_COMPLETE_single_outcome <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:9) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep*sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1.lmer <- lmer(formula, data = ind_complete_datasets[[i]], REML = F)
        output1 <- summary(model1.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["preg_dep1:sex2",]
        results$type <- 'LMER'
        mf1 <- model.frame(model1.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep*sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1.glm <- glm(formula, family = gaussian, data = ind_complete_datasets[[i]])
        output1 <- summary(model1.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1:sex2",]
        results$type <- 'GLM'
        mf1 <- model.frame(model1.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1)
      results_sexint_COMPLETE_single_outcome[i,] <- results
    }
    else {
      results_sexint_COMPLETE_single_outcome[i,] <- "not available"
    }
  }
}
colnames(results_sexint_COMPLETE_single_outcome) <- colnames(results)

# SAVE OUTPUT
write.csv(results_sexint_COMPLETE_single_outcome, paste0(outdir, "sexint_COMPLETE_single_outcome_", cohort_id, ".csv"), row.names =  F)

#---------------------------9) COB adjusted LMM and LMs-------------------------
# Create a dataframe for the output
results_COBadj <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the model adjusted for COB on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + cob_other_country_f + (1|child_id)")
        model2.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output2 <- summary(model2.lmer)
        results <- as.data.frame(output2$coefficients)
        results <- results["preg_dep1",]
        results$type <- 'LMER'
        mf2 <- model.frame(model2.lmer)
        results$samplesize <- length(unique(mf2$child_id)) # ADJUSTED N FOR LMER
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + cob_other_country_f")
        model2.glm <- glm(formula, family = gaussian, data = cohort_data)
        output2 <- summary(model2.glm)
        results <- as.data.frame(output2$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        mf2 <- model.frame(model2.glm)
        results$samplesize <- nrow(mf2)
      }
      results$outcome <- outcome
      results_COBadj[i,] <- results
    }
    else {
      results_COBadj[i,] <- "not available"
    }
  }
}
colnames(results_COBadj) <- colnames(results)

# SAVE OUTPUT
write.csv(results_COBadj, paste0(outdir, "results_COBadj_model_", cohort_id, ".csv"), row.names =  F)
#------------10a) Individual associations | Pre-pregnancy depression-------------
# Create a dataframe for the output
results_prepregind <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model on your data with pre-pregnancy depression as exposure:
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ prepreg_dep + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1_prepregind.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output1 <- summary(model1_prepregind.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["prepreg_dep1",]
        results$type <- 'LMER'
        mf1_pre <- model.frame(model1_prepregind.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ prepreg_dep + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1_prepregind.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1_prepregind.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["prepreg_dep1",]
        results$type <- 'GLM'
        mf1_pre <- model.frame(model1_prepregind.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1_pre)
      results_prepregind[i,] <- results
    }
    else {
      results_prepregind[i,] <- "not available"
    }
  }
}
colnames(results_prepregind) <- colnames(results)

# SAVE OUTPUT
write.csv(results_prepregind, paste0(outdir, "results_prepregind_model_", cohort_id, ".csv"), row.names =  F)

#--------------10b) Individual associations | Postnatal depression---------------
# Create a dataframe for the output
results_ppdind <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model on your data with postnatal depression as exposure:
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ ppd + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1_ppdind.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output1 <- summary(model1_ppdind.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["ppd1",]
        results$type <- 'LMER'
        mf1_ppd <- model.frame(model1_ppdind.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ ppd + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1_ppdind.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1_ppdind.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["ppd1",]
        results$type <- 'GLM'
        mf1_ppd <- model.frame(model1_ppdind.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1_ppd)
      results_ppdind[i,] <- results
    }
    else {
      results_ppdind[i,] <- "not available"
    }
  }
}
colnames(results_ppdind) <- colnames(results)

# SAVE OUTPUT
write.csv(results_ppdind, paste0(outdir, "results_ppdind_model_", cohort_id, ".csv"), row.names =  F)

#------------------11) Adjusting for pre-pregnancy depression-------------------
# Create a dataframe for the output
results_prepregadj <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + prepreg_dep + (1|child_id)")
        model1_prepregadj.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output1 <- summary(model1_prepregadj.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["preg_dep1",]
        results$type <- 'LMER'
        mf1_preadj <- model.frame(model1_prepregadj.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + prepreg_dep")
        model1_prepregadj.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1_prepregadj.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        mf1_preadj <- model.frame(model1_prepregadj.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1_preadj)
      results_prepregadj[i,] <- results
    }
    else {
      results_prepregadj[i,] <- "not available"
    }
  }
}
colnames(results_prepregadj) <- colnames(results)

# SAVE OUTPUT
write.csv(results_prepregadj, paste0(outdir, "results_prepregadj_model_", cohort_id, ".csv"), row.names =  F)

#--------------------12) Mediation by postnatal depression----------------------
# Convert mediator to numeric:
cohort_data$ppd.n <- as.numeric(as.character(cohort_data$ppd))

# I) Internalising symptoms
# Filter for outcomes at 6-10 and select the latest outcome
med_data_int <- cohort_data%>% filter( between(int_age_, 6, 10) )
med_data_int <- med_data_int %>%
  group_by(child_id) %>%
  slice_max(int_age_)

# Use complete cases only:
med_data_int <- med_data_int[c("child_id", "sex", "edu_m_.0", "agebirth_m_y", "preg_dep", 
                               "ppd.n", "preg_alc", "preg_smk", "int_pc_", "int_age_", "prepreg_BMI")]
med_data_int <- na.omit(med_data_int)


med.fit.int <- glm(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                             family= binomial, data = med_data_int)

out.fit.int <- glm(formula="int_pc_ ~ ppd.n + preg_dep + sex + int_age_ + edu_m_.0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                             family = gaussian, data = med_data_int)

med.out.int <- mediate(model.m = med.fit.int, model.y = out.fit.int,
                             treat = "preg_dep", mediator = "ppd.n", robustSE = TRUE, sims = 1000,seed=707)

output_mediation.int <- extract_mediation_summary(summary(med.out.int))

# direct effect
ADE_est.int <- output_mediation.int['ADE', 'Estimate']
ADE_upper.int <- output_mediation.int['ADE', '95% CI Upper']
ADE_lower.int <- output_mediation.int['ADE', '95% CI Lower']

ADE_SE.int <- (ADE_upper.int - ADE_lower.int)/3.92

# indirect effect
ACME_est.int <- output_mediation.int['ACME', 'Estimate']
ACME_upper.int <- output_mediation.int['ACME', '95% CI Upper']
ACME_lower.int <- output_mediation.int['ACME', '95% CI Lower']

ACME_SE.int <- (ACME_upper.int - ACME_lower.int)/3.92

# total effect
TE_est.int <- output_mediation.int['Total Effect', 'Estimate']
TE_upper.int <- output_mediation.int['Total Effect', '95% CI Upper']
TE_lower.int <- output_mediation.int['Total Effect', '95% CI Lower']

TE_SE.int <- (TE_upper.int - TE_lower.int)/3.92

# proportion mediated
PM.int <- output_mediation.int['Prop. Mediated', 'Estimate']*100

results_mediation_int <- list(
  direct_effect = list(est = ADE_est.int, up = ADE_upper.int, lo = ADE_lower.int, se = ADE_SE.int),
  indirect_effect = list(est = ACME_est.int, up = ACME_upper.int, lo = ACME_lower.int, se = ACME_SE.int),
  total_effect = list(est = TE_est.int, up = TE_upper.int, lo = TE_lower.int, se = TE_SE.int),
  proportion_mediated = PM.int,
  samplesize = med.out.int[["nobs"]]
)

saveRDS(results_mediation_int, file=paste0(outdir, "results_mediation_int_", 
                                           cohort_id, ".RData"))

# II) Externalising symptoms
# Filter for outcomes at 6-10 and select the latest outcome
med_data_ext <- cohort_data%>% filter( between(ext_age_, 6, 10) )
med_data_ext <- med_data_ext %>%
  group_by(child_id) %>%
  slice_max(ext_age_)

# Use complete cases only:
med_data_ext <- med_data_ext[c("child_id", "sex", "edu_m_.0", "agebirth_m_y", "preg_dep", "ppd.n", 
                               "preg_alc", "preg_smk", "ext_pc_", "ext_age_", "prepreg_BMI")]
med_data_ext <- na.omit(med_data_ext)

med.fit.ext <- glm(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_ext)

out.fit.ext <- glm(formula="ext_pc_ ~ ppd.n + preg_dep + sex + ext_age_ + edu_m_.0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_ext)

med.out.ext <- mediate(model.m = med.fit.ext, model.y = out.fit.ext,
                       treat = "preg_dep", mediator = "ppd.n", robustSE = TRUE, sims = 1000,
                       seed=707)

output_mediation.ext <- extract_mediation_summary(summary(med.out.ext))

# direct effect
ADE_est.ext <- output_mediation.ext['ADE', 'Estimate']
ADE_upper.ext <- output_mediation.ext['ADE', '95% CI Upper']
ADE_lower.ext <- output_mediation.ext['ADE', '95% CI Lower']

ADE_SE.ext <- (ADE_upper.ext - ADE_lower.ext)/3.92

# indirect effect
ACME_est.ext <- output_mediation.ext['ACME', 'Estimate']
ACME_upper.ext <- output_mediation.ext['ACME', '95% CI Upper']
ACME_lower.ext <- output_mediation.ext['ACME', '95% CI Lower']

ACME_SE.ext <- (ACME_upper.ext - ACME_lower.ext)/3.92

# total effect
TE_est.ext <- output_mediation.ext['Total Effect', 'Estimate']
TE_upper.ext <- output_mediation.ext['Total Effect', '95% CI Upper']
TE_lower.ext <- output_mediation.ext['Total Effect', '95% CI Lower']

TE_SE.ext <- (TE_upper.ext - TE_lower.ext)/3.92

# proportion mediated
PM.ext <- output_mediation.ext['Prop. Mediated', 'Estimate']*100

results_mediation_ext <- list(
  direct_effect = list(est = ADE_est.ext, up = ADE_upper.ext, lo = ADE_lower.ext, se = ADE_SE.ext),
  indirect_effect = list(est = ACME_est.ext, up = ACME_upper.ext, lo = ACME_lower.ext, se = ACME_SE.ext),
  total_effect = list(est = TE_est.ext, up = TE_upper.ext, lo = TE_lower.ext, se = TE_SE.ext),
  proportion_mediated = PM.ext,
  samplesize = med.out.ext[["nobs"]]
)

saveRDS(results_mediation_ext, file=paste0(outdir, "results_mediation_ext_", 
                                           cohort_id, ".RData"))

# III) ADHD symptoms
# Filter for outcomes at 4-9 and select the latest outcome
med_data_adhd <- cohort_data%>% filter( between(adhd_age_, 4, 9) )
med_data_adhd <- med_data_adhd %>%
  group_by(child_id) %>%
  slice_max(adhd_age_)

# Use complete cases only:
med_data_adhd <- med_data_adhd[c("child_id", "sex", "edu_m_.0", "agebirth_m_y", "preg_dep", "ppd.n", 
                                 "preg_alc", "preg_smk", "adhd_pc_", "adhd_age_", "prepreg_BMI")]
med_data_adhd <- na.omit(med_data_adhd)


med.fit.adhd <- glm(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + 
                    preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_adhd)

out.fit.adhd <- glm(formula="adhd_pc_ ~ ppd.n + preg_dep + sex + adhd_age_ + edu_m_.0 + 
                    agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_adhd)

med.out.adhd <- mediate(model.m = med.fit.adhd, model.y = out.fit.adhd,
                       treat = "preg_dep", mediator = "ppd.n", robustSE = TRUE, sims = 1000,
                       seed=707)

output_mediation.adhd <- extract_mediation_summary(summary(med.out.adhd))

# direct effect
ADE_est.adhd <- output_mediation.adhd['ADE', 'Estimate']
ADE_upper.adhd <- output_mediation.adhd['ADE', '95% CI Upper']
ADE_lower.adhd <- output_mediation.adhd['ADE', '95% CI Lower']

ADE_SE.adhd <- (ADE_upper.adhd - ADE_lower.adhd)/3.92

# indirect effect
ACME_est.adhd <- output_mediation.adhd['ACME', 'Estimate']
ACME_upper.adhd <- output_mediation.adhd['ACME', '95% CI Upper']
ACME_lower.adhd <- output_mediation.adhd['ACME', '95% CI Lower']

ACME_SE.adhd <- (ACME_upper.adhd - ACME_lower.adhd)/3.92

# total effect
TE_est.adhd <- output_mediation.adhd['Total Effect', 'Estimate']
TE_upper.adhd <- output_mediation.adhd['Total Effect', '95% CI Upper']
TE_lower.adhd <- output_mediation.adhd['Total Effect', '95% CI Lower']

TE_SE.adhd <- (TE_upper.adhd - TE_lower.adhd)/3.92

# proportion mediated
PM.adhd <- output_mediation.adhd['Prop. Mediated', 'Estimate']*100

results_mediation_adhd <- list(
  direct_effect = list(est = ADE_est.adhd, up = ADE_upper.adhd, lo = ADE_lower.adhd, se = ADE_SE.adhd),
  indirect_effect = list(est = ACME_est.adhd, up = ACME_upper.adhd, lo = ACME_lower.adhd, se = ACME_SE.adhd),
  total_effect = list(est = TE_est.adhd, up = TE_upper.adhd, lo = TE_lower.adhd, se = TE_SE.adhd),
  proportion_mediated = PM.adhd,
  samplesize = med.out.adhd[["nobs"]]
)

saveRDS(results_mediation_adhd, file=paste0(outdir, "results_mediation_adhd_", 
                                            cohort_id, ".RData"))

# IV) ASD symptoms
# Filter for outcomes at 0-7 and select the latest outcome
med_data_asd <- cohort_data%>% filter( between(asd_age_, 0, 7) )
med_data_asd <- med_data_asd %>%
  group_by(child_id) %>%
  slice_max(asd_age_)

# Use complete cases only:
med_data_asd <- med_data_asd[c("child_id", "sex", "edu_m_.0", "agebirth_m_y", "preg_dep", "ppd.n", 
                               "preg_alc", "preg_smk", "asd_pc_", "asd_age_", "prepreg_BMI")]
med_data_asd <- na.omit(med_data_asd)


med.fit.asd <- glm(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI", 
                    family= binomial, data = med_data_asd)

out.fit.asd <- glm(formula="asd_pc_ ~ ppd.n + preg_dep + sex + asd_age_ + edu_m_.0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                    family = gaussian, data = med_data_asd)

med.out.asd <- mediate(model.m = med.fit.asd, model.y = out.fit.asd,
                        treat = "preg_dep", mediator = "ppd.n", robustSE = TRUE, sims = 1000,
                        seed=707)

output_mediation.asd <- extract_mediation_summary(summary(med.out.asd))

# direct effect
ADE_est.asd <- output_mediation.asd['ADE', 'Estimate']
ADE_upper.asd <- output_mediation.asd['ADE', '95% CI Upper']
ADE_lower.asd <- output_mediation.asd['ADE', '95% CI Lower']

ADE_SE.asd <- (ADE_upper.asd - ADE_lower.asd)/3.92

# indirect effect
ACME_est.asd <- output_mediation.asd['ACME', 'Estimate']
ACME_upper.asd <- output_mediation.asd['ACME', '95% CI Upper']
ACME_lower.asd <- output_mediation.asd['ACME', '95% CI Lower']

ACME_SE.asd <- (ACME_upper.asd - ACME_lower.asd)/3.92

# total effect
TE_est.asd <- output_mediation.asd['Total Effect', 'Estimate']
TE_upper.asd <- output_mediation.asd['Total Effect', '95% CI Upper']
TE_lower.asd <- output_mediation.asd['Total Effect', '95% CI Lower']

TE_SE.asd <- (TE_upper.asd - TE_lower.asd)/3.92

# proportion mediated
PM.asd <- output_mediation.asd['Prop. Mediated', 'Estimate']*100

results_mediation_asd <- list(
  direct_effect = list(est = ADE_est.asd, up = ADE_upper.asd, lo = ADE_lower.asd, se = ADE_SE.asd),
  indirect_effect = list(est = ACME_est.asd, up = ACME_upper.asd, lo = ACME_lower.asd, se = ACME_SE.asd),
  total_effect = list(est = TE_est.asd, up = TE_upper.asd, lo = TE_lower.asd, se = TE_SE.asd),
  proportion_mediated = PM.asd,
  samplesize = med.out.asd[["nobs"]]
)

saveRDS(results_mediation_asd, file=paste0(outdir, "results_mediation_asd_", 
                                           cohort_id, ".RData"))

#--------------------13) Cumulative exposure to depression----------------------
# Create a dataframe for the output
results_cumul <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model on your data with accumulation of depression as 
# exposure:
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ cumul_dep + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1_cumul.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output1 <- summary(model1_cumul.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["cumul_dep",]
        results$type <- 'LMER'
        mf1_cumul <- model.frame(model1_cumul.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ cumul_dep + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1_cumul.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1_cumul.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["cumul_dep",]
        results$type <- 'GLM'
        mf1_cumul <- model.frame(model1_cumul.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1_cumul)
      results_cumul[i,] <- results
    }
    else {
      results_cumul[i,] <- "not available"
    }
  }
}
colnames(results_cumul) <- colnames(results)

# SAVE OUTPUT
write.csv(results_cumul, paste0("results_cumul_model_", cohort_id, ".csv"), row.names =  F)

#----------------------14) Continuous depressive symptoms-----------------------
# Create a dataframe for the output
results_cont <- data.frame(matrix(NA, nrow = 9, ncol = 6))

# Run the fully adjusted model with continuous depressive symptoms as exposure:
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "yes") {
        formula <- paste0(outcome, "pc_ ~ preg_dep_std_continuous + sex + ", outcome, "age_ + 
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + 
                          prepreg_BMI + (1|child_id)")
        model1_cont.lmer <- lmer(formula, data = cohort_data, REML = TRUE)
        output1 <- summary(model1_cont.lmer)
        results <- as.data.frame(output1$coefficients)
        results <- results["preg_dep_std_continuous",]
        results$type <- 'LMER'
        mf1_cont <- model.frame(model1_cont.lmer)
      }
      else if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "pc_ ~ preg_dep_std_continuous + sex + ", outcome, "age_ +
                          edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1_cont.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1_cont.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep_std_continuous",]
        results$type <- 'GLM'
        mf1_cont <- model.frame(model1_cont.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1_cont)
      results_cont[i,] <- results
    }
    else {
      results_cont[i,] <- "not available"
    }
  }
}
colnames(results_cont) <- colnames(results)

# SAVE OUTPUT
write.csv(results_cont, paste0("results_cont_model_", cohort_id, ".csv"), row.names =  F)

#-------------------------15) Save session information--------------------------
sink(paste0(outdir, cohort_id, "_session_info.txt"))
sessionInfo()
sink()

