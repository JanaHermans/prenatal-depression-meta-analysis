################################################################################
# Scripts with teacher-rated outcomes and maternal depression at child age 3 and 9
# Purpose: Revision November 2025
# Date created: 13-11-2025
################################################################################
library(dplyr)
library(haven)
library(purrr)
library(mediation)
library(tidyverse)
library(lme4)
datapath <- dirname(file.choose())
cohort_id <- 'GenR' 
outdir <- "V:/medewerkers/093307 Hermans A.P.C/project_2/results/REV/"
#----------------------------------1) Set up------------------------------------
# Load data files 
BSI3y <- read_sav(file.path(datapath,'BSI 3 years of age_GR1065 G1-GR1066 C1_22112016_cleaned.sav'));
BSI3y <- BSI3y[c('IDC','dep_M36_m')]
BSI9y <- read_sav(file.path(datapath,'GR1081-GR1083_D1_BSI_19042017.sav'));
BSI9y <- BSI9y[c('IDC','dep_M108_m')]
TRF <- read_sav(file.path(datapath,"Child_TRF_GR1079_20251006.sav"))
TRF <- TRF[c('IDC','s_trf1_int', 's_trf1_ext', 's_trf1_att', 'age_TRF')]
ECCN_core <- read_sav(file.path(datapath,"ECCNLC202378_LC Core DV10.0_USERfile_JanaHermans_20251118.sav"))
ECCN_core <- ECCN_core[c('IDC','agebirth_m_y', 'height_m', 'ppd', 'preg_alc',
                         'preg_dep', 'preg_smk', 'prepreg_weight', 'sex', 'edu_m_0')]
ECCN_out  <- read_sav(file.path(datapath,"ECCNLC202378_LC Outc DV5.0_USERfile_JanaHermans_20251118.sav"))

# Merge
cohort_data <- list(ECCN_core, ECCN_out, BSI3y, BSI9y,TRF) %>%
  reduce(~ full_join(.x, .y, by = "IDC"))

# Convert variables to correct variable types
cohort_data$sex <- as.factor(cohort_data$sex)
cohort_data$edu_m_0 <- as.factor(cohort_data$edu_m_0)
cohort_data$agebirth_m_y <- as.numeric(cohort_data$agebirth_m_y)
cohort_data$height_m <- as.numeric(cohort_data$height_m)
cohort_data$prepreg_weight <- as.numeric(cohort_data$prepreg_weight)
cohort_data$preg_dep <- as.factor(cohort_data$preg_dep)
cohort_data$ppd <- as.factor(cohort_data$ppd)
cohort_data$preg_alc <- as.factor(cohort_data$preg_alc)
cohort_data$preg_smk <- as.factor(cohort_data$preg_smk)

# Remove for local memory
rm(BSI3y, BSI9y, ECCN_core, ECCN_out, TRF)
#------------------------------2a) Add prepreg_BMI-------------------------------
# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
cohort_data$prepreg_BMI <- (cohort_data$prepreg_weight/((cohort_data$height_m/100)^2))
cohort_data$prepreg_BMI <- as.numeric(cohort_data$prepreg_BMI)

#--------------------2b) Maternal depression harmonisation----------------------
cohort_data <- cohort_data %>% 
  dplyr::mutate(dep_bin_3y = dplyr::case_when(
    dep_M36_m  <= 0.75 ~ "0",
    dep_M36_m  > 0.75 ~ "1",
    TRUE ~ NA_character_
  ),
  dep_bin_3y = factor(dep_bin_3y)
  )

cohort_data <- cohort_data %>% 
  dplyr::mutate(dep_bin_9y = dplyr::case_when(
    dep_M108_m  <= 0.75 ~ "0",
    dep_M108_m  > 0.75 ~ "1",
    TRUE ~ NA_character_
  ),
  dep_bin_9y = factor(dep_bin_9y)
  )

#---------------------------2c) Create TRF percentiles--------------------------
# Percentile ranks (like SPSS RANK /PERCENT)

rank_percentile <- function(x) {
  r <- rank(x, ties.method = "max", na.last = "keep")
  100 * r / sum(!is.na(x))
}

cohort_data <- cohort_data %>%
  mutate(
    int_TRF_pc_  = rank_percentile(s_trf1_int), # weighted sumscore internalizing scale 
    ext_TRF_pc_  = rank_percentile(s_trf1_ext), # weighted sumscore externalizing scale 
    adhd_TRF_pc_ = rank_percentile(s_trf1_att), # weighted sumscore attention problems
    TRF_age_     = age_TRF/12                   # age child in months
  )

# Check 
summary(cohort_data$int_TRF_pc_)
summary(cohort_data$ext_TRF_pc_)
summary(cohort_data$adhd_TRF_pc_)
summary(cohort_data$TRF_age_)

results_TRF <- data.frame(matrix(NA, nrow = 3, ncol = 8))
available_data <- as.data.frame(c('int_', 'ext_', 'adhd_'))
colnames(available_data) <- "outcome"
available_data$available <- c("1", "1", "1")  
available_data$repeated <- c("no", "no", "no")

# Run the fully adjusted model on your data (taking into account what you 
# specified about their availability in the 'available_data' dataframe):
for (i in 1:nrow(available_data)) {
  {
    if (available_data[i,2] == "1") {
      outcome <- available_data[i,1]
      if (available_data[i,3] == "no") {
        formula <- paste0(outcome, "TRF_pc_ ~ preg_dep + sex + TRF_age_ +
                          edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
        model1.glm <- glm(formula, family = gaussian, data = cohort_data)
        output1 <- summary(model1.glm)
        results <- as.data.frame(output1$coefficients)
        results$`Pr(>|t|)` <- NULL
        results <- results["preg_dep1",]
        results$type <- 'GLM'
        results$conf_low <- results$Estimate - results$`Std. Error`
        results$conf_high <- results$Estimate + results$`Std. Error`
        mf1 <- model.frame(model1.glm)
      }
      results$outcome <- outcome
      results$samplesize <- nrow(mf1)
      results_TRF[i,] <- results
    }
    else {
      results_TRF[i,] <- "not available"
    }
  }
}
colnames(results_TRF) <- colnames(results)
saveRDS(results_TRF, file=paste0(outdir, "REV_results_TRF_", 
                    cohort_id, ".RData"))
#-----------------------4) Load mediation output function-----------------------
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

#---------------------5) Mediation by postnatal depression----------------------
# Convert mediator to numeric:
cohort_data$ppd.n <- as.numeric(as.character(cohort_data$ppd))
cohort_data$dep_bin_3y.n <- as.numeric(as.character(cohort_data$dep_bin_3y))
cohort_data$dep_bin_9y.n <- as.numeric(as.character(cohort_data$dep_bin_9y))

#--------------------------5a) Internalising symptoms---------------------------

# Internalising symptoms
long_int <- cohort_data %>%
  pivot_longer(
    cols = matches("^int_"),                   
    names_to = c("var", "time"),
    names_pattern = "(int_.*_)(\\d+)",          
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = var,                           # names: int_age_ and int_pc_
    values_from = value
  )

# Direct associations - 3y
formula <- "int_pc_ ~ dep_bin_3y + sex + int_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI + (1|IDC)"
model1.lmer <- lmer(formula, data = long_int, REML = TRUE)
output1 <- summary(model1.lmer)
results <- as.data.frame(output1$coefficients)
results <- results["dep_bin_3y1",]
results$type <- 'LMER'
mf1 <- model.frame(model1.lmer)
results$outcome <- 'int_'
results$samplesize <- length(unique(mf1$IDC))
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep3y <- results
colnames(results__direct_dep3y) <- colnames(results)
results__direct_dep3y_int <- results__direct_dep3y

# Direct associations - 9y
formula <- "int_pc_ ~ dep_bin_9y + sex + int_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI + (1|IDC)"
model1.lmer <- lmer(formula, data = long_int, REML = TRUE)
output1 <- summary(model1.lmer)
results <- as.data.frame(output1$coefficients)
results <- results["dep_bin_9y1",]
results$type <- 'LMER'
mf1 <- model.frame(model1.lmer)
results$outcome <- 'int_'
results$samplesize <- length(unique(mf1$IDC))
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep9y <- results
colnames(results__direct_dep9y) <- colnames(results)
results__direct_dep9y_int <- results__direct_dep9y

saveRDS(list(dep3y = results__direct_dep3y_int, dep9y = results__direct_dep9y_int), 
        file=paste0(outdir, "REV_results_direct_int_3y9y_", 
                    cohort_id, ".RData"))

# Filter for outcomes at 6-10 and select the latest outcome
med_data_int <- long_int %>% filter( between(int_age_, 6, 10) )
med_data_int <- med_data_int %>%
  group_by(IDC) %>%
  slice_max(int_age_)
#----------------------i) Maternal depression at 3 years------------------------
med_data_int_3y <- med_data_int[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                               "dep_bin_3y.n", "preg_alc", "preg_smk", "int_pc_", "int_age_", "prepreg_BMI")]
med_data_int_3y <- na.omit(med_data_int_3y)

med.fit.int <- glm(formula="dep_bin_3y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_int_3y)

out.fit.int <- glm(formula="int_pc_ ~ dep_bin_3y.n + preg_dep + sex + int_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_int_3y)

set.seed(707)
med.out.int <- mediate(model.m = med.fit.int, model.y = out.fit.int,
                       treat = "preg_dep", mediator = "dep_bin_3y.n", robustSE = TRUE, sims = 1000, seed=707)

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

results_mediation_int_3y <- list(
  direct_effect = list(est = ADE_est.int, up = ADE_upper.int, lo = ADE_lower.int, se = ADE_SE.int),
  indirect_effect = list(est = ACME_est.int, up = ACME_upper.int, lo = ACME_lower.int, se = ACME_SE.int),
  total_effect = list(est = TE_est.int, up = TE_upper.int, lo = TE_lower.int, se = TE_SE.int),
  proportion_mediated = PM.int,
  samplesize = med.out.int[["nobs"]]
)

saveRDS(results_mediation_int_3y, file=paste0(outdir, "REV_results_mediation_int_3y_", 
                                           cohort_id, ".RData"))
#----------------------i) Maternal depression at 9 years------------------------
med_data_int_9y <- med_data_int[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_9y.n", "preg_alc", "preg_smk", "int_pc_", "int_age_", "prepreg_BMI")]
med_data_int_9y <- na.omit(med_data_int_9y)

med.fit.int <- glm(formula="dep_bin_9y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_int_9y)

out.fit.int <- glm(formula="int_pc_ ~ dep_bin_9y.n + preg_dep + sex + int_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_int_9y)

set.seed(707)
med.out.int <- mediate(model.m = med.fit.int, model.y = out.fit.int,
                       treat = "preg_dep", mediator = "dep_bin_9y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_int_9y <- list(
  direct_effect = list(est = ADE_est.int, up = ADE_upper.int, lo = ADE_lower.int, se = ADE_SE.int),
  indirect_effect = list(est = ACME_est.int, up = ACME_upper.int, lo = ACME_lower.int, se = ACME_SE.int),
  total_effect = list(est = TE_est.int, up = TE_upper.int, lo = TE_lower.int, se = TE_SE.int),
  proportion_mediated = PM.int,
  samplesize = med.out.int[["nobs"]]
)

saveRDS(results_mediation_int_9y, file=paste0(outdir, "REV_results_mediation_int_9y_", 
                                           cohort_id, ".RData"))
#--------------------------5b) Externalising symptoms---------------------------
rm(long_int)
rm(med_data_int)
rm(med_data_int_3y)
rm(med_data_int_9y)

# Externalising symptoms
long_ext <- cohort_data %>%
  pivot_longer(
    cols = matches("^ext_"),                   
    names_to = c("var", "time"),
    names_pattern = "(ext_.*_)(\\d+)",          
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = var,                           # names: ext_age_ and ext_pc_
    values_from = value
  )

# Direct associations - 3y
formula <- "ext_pc_ ~ dep_bin_3y + sex + ext_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI + (1|IDC)"
model1.lmer <- lmer(formula, data = long_ext, REML = TRUE)
output1 <- summary(model1.lmer)
results <- as.data.frame(output1$coefficients)
results <- results["dep_bin_3y1",]
results$type <- 'LMER'
mf1 <- model.frame(model1.lmer)
results$outcome <- 'ext_'
results$samplesize <- length(unique(mf1$IDC))
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep3y <- results
colnames(results__direct_dep3y) <- colnames(results)
results__direct_dep3y_ext <- results__direct_dep3y

# Direct associations - 9y
formula <- "ext_pc_ ~ dep_bin_9y + sex + ext_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI + (1|IDC)"
model1.lmer <- lmer(formula, data = long_ext, REML = TRUE)
output1 <- summary(model1.lmer)
results <- as.data.frame(output1$coefficients)
results <- results["dep_bin_9y1",]
results$type <- 'LMER'
mf1 <- model.frame(model1.lmer)
results$outcome <- 'ext_'
results$samplesize <- length(unique(mf1$IDC))
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep9y <- results
colnames(results__direct_dep9y) <- colnames(results)
results__direct_dep9y_ext <- results__direct_dep9y

saveRDS(list(dep3y = results__direct_dep3y_ext, dep9y = results__direct_dep9y_ext), 
        file=paste0(outdir, "REV_results_direct_ext_3y9y_", 
                    cohort_id, ".RData"))

# Filter for outcomes at 6-10 and select the latest outcome
med_data_ext <- long_ext %>% filter( between(ext_age_, 6, 10) )
med_data_ext <- med_data_ext %>%
  group_by(IDC) %>%
  slice_max(ext_age_)
#----------------------i) Maternal depression at 3 years------------------------
med_data_ext_3y <- med_data_ext[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_3y.n", "preg_alc", "preg_smk", "ext_pc_", "ext_age_", "prepreg_BMI")]
med_data_ext_3y <- na.omit(med_data_ext_3y)

med.fit.ext <- glm(formula="dep_bin_3y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_ext_3y)

out.fit.ext <- glm(formula="ext_pc_ ~ dep_bin_3y.n + preg_dep + sex + ext_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_ext_3y)

set.seed(707)
med.out.ext <- mediate(model.m = med.fit.ext, model.y = out.fit.ext,
                       treat = "preg_dep", mediator = "dep_bin_3y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_ext_3y <- list(
  direct_effect = list(est = ADE_est.ext, up = ADE_upper.ext, lo = ADE_lower.ext, se = ADE_SE.ext),
  indirect_effect = list(est = ACME_est.ext, up = ACME_upper.ext, lo = ACME_lower.ext, se = ACME_SE.ext),
  total_effect = list(est = TE_est.ext, up = TE_upper.ext, lo = TE_lower.ext, se = TE_SE.ext),
  proportion_mediated = PM.ext,
  samplesize = med.out.ext[["nobs"]]
)

saveRDS(results_mediation_ext_3y, file=paste0(outdir, "REV_results_mediation_ext_3y_", 
                                           cohort_id, ".RData"))
#----------------------i) Maternal depression at 9 years------------------------
med_data_ext_9y <- med_data_ext[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_9y.n", "preg_alc", "preg_smk", "ext_pc_", "ext_age_", "prepreg_BMI")]
med_data_ext_9y <- na.omit(med_data_ext_9y)

med.fit.ext <- glm(formula="dep_bin_9y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_ext_9y)

out.fit.ext <- glm(formula="ext_pc_ ~ dep_bin_9y.n + preg_dep + sex + ext_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_ext_9y)

set.seed(707)
med.out.ext <- mediate(model.m = med.fit.ext, model.y = out.fit.ext,
                       treat = "preg_dep", mediator = "dep_bin_9y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_ext_9y <- list(
  direct_effect = list(est = ADE_est.ext, up = ADE_upper.ext, lo = ADE_lower.ext, se = ADE_SE.ext),
  indirect_effect = list(est = ACME_est.ext, up = ACME_upper.ext, lo = ACME_lower.ext, se = ACME_SE.ext),
  total_effect = list(est = TE_est.ext, up = TE_upper.ext, lo = TE_lower.ext, se = TE_SE.ext),
  proportion_mediated = PM.ext,
  samplesize = med.out.ext[["nobs"]]
)

saveRDS(results_mediation_ext_9y, file=paste0(outdir, "REV_results_mediation_ext_9y_", 
                                           cohort_id, ".RData"))
#-------------------------------5c) ADHD symptoms-------------------------------
rm(long_ext)
rm(med_data_ext)
rm(med_data_ext_3y)
rm(med_data_ext_9y)

long_adhd <- cohort_data %>%
  pivot_longer(
    cols = matches("^adhd_"),                   
    names_to = c("var", "time"),
    names_pattern = "(adhd_.*_)(\\d+)",          
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = var,                           # names: adhd_age_ and adhd_pc_
    values_from = value
  )

# Direct associations - 3y
formula <- "adhd_pc_ ~ dep_bin_3y + sex + adhd_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI"
model1.glm <- glm(formula, family = gaussian, data = long_adhd)
output1 <- summary(model1.glm)
results <- as.data.frame(output1$coefficients)
results$`Pr(>|t|)` <- NULL
results <- results["dep_bin_3y1",]
results$type <- 'GLM'
mf1 <- model.frame(model1.glm)
results$outcome <- 'adhd_'
results$samplesize <- nrow(mf1)
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep3y <- results
colnames(results__direct_dep3y) <- colnames(results)
results__direct_dep3y_adhd <- results__direct_dep3y

# Direct associations - 9y
formula <- "adhd_pc_ ~ dep_bin_9y + sex + adhd_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI"
model1.glm <- glm(formula, family = gaussian, data = long_adhd)
output1 <- summary(model1.glm)
results <- as.data.frame(output1$coefficients)
results$`Pr(>|t|)` <- NULL
results <- results["dep_bin_9y1",]
results$type <- 'GLM'
mf1 <- model.frame(model1.glm)
results$outcome <- 'adhd_'
results$samplesize <- nrow(mf1)
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep9y <- results
colnames(results__direct_dep9y) <- colnames(results)
results__direct_dep9y_adhd <- results__direct_dep9y

saveRDS(list(dep3y = results__direct_dep3y_adhd, dep9y = results__direct_dep9y_adhd), 
        file=paste0(outdir, "REV_results_direct_adhd_3y9y_", 
                    cohort_id, ".RData"))

# Filter for outcomes at 4-9 and select the latest outcome
med_data_adhd <- long_adhd %>% filter( between(adhd_age_, 4, 9) )
med_data_adhd <- med_data_adhd %>%
  group_by(IDC) %>%
  slice_max(adhd_age_)
#----------------------i) Maternal depression at 3 years------------------------
med_data_adhd_3y <- med_data_adhd[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_3y.n", "preg_alc", "preg_smk", "adhd_pc_", "adhd_age_", "prepreg_BMI")]
med_data_adhd_3y <- na.omit(med_data_adhd_3y)

med.fit.adhd <- glm(formula="dep_bin_3y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_adhd_3y)

out.fit.adhd <- glm(formula="adhd_pc_ ~ dep_bin_3y.n + preg_dep + sex + adhd_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_adhd_3y)

set.seed(707)
med.out.adhd <- mediate(model.m = med.fit.adhd, model.y = out.fit.adhd,
                       treat = "preg_dep", mediator = "dep_bin_3y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_adhd_3y <- list(
  direct_effect = list(est = ADE_est.adhd, up = ADE_upper.adhd, lo = ADE_lower.adhd, se = ADE_SE.adhd),
  indirect_effect = list(est = ACME_est.adhd, up = ACME_upper.adhd, lo = ACME_lower.adhd, se = ACME_SE.adhd),
  total_effect = list(est = TE_est.adhd, up = TE_upper.adhd, lo = TE_lower.adhd, se = TE_SE.adhd),
  proportion_mediated = PM.adhd,
  samplesize = med.out.adhd[["nobs"]]
)

saveRDS(results_mediation_adhd_3y, file=paste0(outdir, "REV_results_mediation_adhd_3y_", 
                                           cohort_id, ".RData"))
#----------------------i) Maternal depression at 9 years------------------------
med_data_adhd_9y <- med_data_adhd[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_9y.n", "preg_alc", "preg_smk", "adhd_pc_", "adhd_age_", "prepreg_BMI")]
med_data_adhd_9y <- na.omit(med_data_adhd_9y)

med.fit.adhd <- glm(formula="dep_bin_9y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_adhd_9y)

out.fit.adhd <- glm(formula="adhd_pc_ ~ dep_bin_9y.n + preg_dep + sex + adhd_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_adhd_9y)

set.seed(707)
med.out.adhd <- mediate(model.m = med.fit.adhd, model.y = out.fit.adhd,
                       treat = "preg_dep", mediator = "dep_bin_9y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_adhd_9y <- list(
  direct_effect = list(est = ADE_est.adhd, up = ADE_upper.adhd, lo = ADE_lower.adhd, se = ADE_SE.adhd),
  indirect_effect = list(est = ACME_est.adhd, up = ACME_upper.adhd, lo = ACME_lower.adhd, se = ACME_SE.adhd),
  total_effect = list(est = TE_est.adhd, up = TE_upper.adhd, lo = TE_lower.adhd, se = TE_SE.adhd),
  proportion_mediated = PM.adhd,
  samplesize = med.out.adhd[["nobs"]]
)

saveRDS(results_mediation_adhd_9y, file=paste0(outdir, "REV_results_mediation_adhd_9y_", 
                                           cohort_id, ".RData"))
#-------------------------------5d) ASD symptoms--------------------------------
rm(long_adhd)
rm(med_data_adhd)
rm(med_data_adhd_3y)
rm(med_data_adhd_9y)

long_asd <- cohort_data %>%
  pivot_longer(
    cols = matches("^asd_"),                   
    names_to = c("var", "time"),
    names_pattern = "(asd_.*_)(\\d+)",          
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = var,                           # names: asd_age_ and asd_pc_
    values_from = value
  )

# Direct associations - 3y
formula <- "asd_pc_ ~ dep_bin_3y + sex + asd_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI"
model1.glm <- glm(formula, family = gaussian, data = long_asd)
output1 <- summary(model1.glm)
results <- as.data.frame(output1$coefficients)
results$`Pr(>|t|)` <- NULL
results <- results["dep_bin_3y1",]
results$type <- 'GLM'
mf1 <- model.frame(model1.glm)
results$outcome <- 'asd_'
results$samplesize <- nrow(mf1)
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep3y <- results
colnames(results__direct_dep3y) <- colnames(results)
results__direct_dep3y_asd <- results__direct_dep3y

# Direct associations - 9y
formula <- "asd_pc_ ~ dep_bin_9y + sex + asd_age_ + edu_m_0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI"
model1.glm <- glm(formula, family = gaussian, data = long_asd)
output1 <- summary(model1.glm)
results <- as.data.frame(output1$coefficients)
results$`Pr(>|t|)` <- NULL
results <- results["dep_bin_9y1",]
results$type <- 'GLM'
mf1 <- model.frame(model1.glm)
results$outcome <- 'asd_'
results$samplesize <- nrow(mf1)
results$conf.low <- results$Estimate - 1.96*results$`Std. Error`
results$conf.high <- results$Estimate + 1.96*results$`Std. Error`
results__direct_dep9y <- results
colnames(results__direct_dep9y) <- colnames(results)
results__direct_dep9y_asd <- results__direct_dep9y

saveRDS(list(dep3y = results__direct_dep3y_asd, dep9y = results__direct_dep9y_asd), 
        file=paste0(outdir, "REV_results_direct_asd_3y9y_", 
                    cohort_id, ".RData"))

# Filter for outcomes at 0-7 and select the latest outcome
med_data_asd <- long_asd %>% filter( between(asd_age_, 0, 7) )
med_data_asd <- med_data_asd %>%
  group_by(IDC) %>%
  slice_max(asd_age_)
#----------------------i) Maternal depression at 3 years------------------------
med_data_asd_3y <- med_data_asd[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_3y.n", "preg_alc", "preg_smk", "asd_pc_", "asd_age_", "prepreg_BMI")]
med_data_asd_3y <- na.omit(med_data_asd_3y)

med.fit.asd <- glm(formula="dep_bin_3y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_asd_3y)

out.fit.asd <- glm(formula="asd_pc_ ~ dep_bin_3y.n + preg_dep + sex + asd_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_asd_3y)

set.seed(707)
med.out.asd <- mediate(model.m = med.fit.asd, model.y = out.fit.asd,
                       treat = "preg_dep", mediator = "dep_bin_3y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_asd_3y <- list(
  direct_effect = list(est = ADE_est.asd, up = ADE_upper.asd, lo = ADE_lower.asd, se = ADE_SE.asd),
  indirect_effect = list(est = ACME_est.asd, up = ACME_upper.asd, lo = ACME_lower.asd, se = ACME_SE.asd),
  total_effect = list(est = TE_est.asd, up = TE_upper.asd, lo = TE_lower.asd, se = TE_SE.asd),
  proportion_mediated = PM.asd,
  samplesize = med.out.asd[["nobs"]]
)

saveRDS(results_mediation_asd_3y, file=paste0(outdir, "REV_results_mediation_asd_3y_", 
                                           cohort_id, ".RData"))
#----------------------i) Maternal depression at 9 years------------------------
med_data_asd_9y <- med_data_asd[c("IDC", "sex", "edu_m_0", "agebirth_m_y", "preg_dep", 
                                  "dep_bin_9y.n", "preg_alc", "preg_smk", "asd_pc_", "asd_age_", "prepreg_BMI")]
med_data_asd_9y <- na.omit(med_data_asd_9y)

med.fit.asd <- glm(formula="dep_bin_9y.n ~ preg_dep + sex + edu_m_0 + agebirth_m_y + 
                   preg_alc + preg_smk + prepreg_BMI", 
                   family= binomial, data = med_data_asd_9y)

out.fit.asd <- glm(formula="asd_pc_ ~ dep_bin_9y.n + preg_dep + sex + asd_age_ + edu_m_0 + 
                   agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                   family = gaussian, data = med_data_asd_9y)

set.seed(707)
med.out.asd <- mediate(model.m = med.fit.asd, model.y = out.fit.asd,
                       treat = "preg_dep", mediator = "dep_bin_9y.n", robustSE = TRUE, sims = 1000,seed=707)

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

results_mediation_asd_9y <- list(
  direct_effect = list(est = ADE_est.asd, up = ADE_upper.asd, lo = ADE_lower.asd, se = ADE_SE.asd),
  indirect_effect = list(est = ACME_est.asd, up = ACME_upper.asd, lo = ACME_lower.asd, se = ACME_SE.asd),
  total_effect = list(est = TE_est.asd, up = TE_upper.asd, lo = TE_lower.asd, se = TE_SE.asd),
  proportion_mediated = PM.asd,
  samplesize = med.out.asd[["nobs"]]
)

saveRDS(results_mediation_asd_9y, file=paste0(outdir, "REV_results_mediation_asd_9y_", 
                                           cohort_id, ".RData"))
