library(DSMolgenisArmadillo)
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(grid)
plot <- 'no'

#-------------------------1a) Log in multiple cohorts---------------------------
# Login details were removed from this publicly shared script 
#-----------------------------1b) Merge dataframes------------------------------

# select from the repeated df age_years, edu_m_
ds.dataFrame(x = c("core_yearlyrep$child_id", "core_yearlyrep$age_years", "core_yearlyrep$edu_m_"),
             newobj = "core_yearlyrep_small", completeCases = F,
             datasources = connections)

ds.colnames('core_yearlyrep_small', datasources = connections)

ds.dataFrame(x = c("outcome_yearlyrep$child_id", "outcome_yearlyrep$age_years", 
                   "outcome_yearlyrep$adhd_age_", "outcome_yearlyrep$adhd_pc_",
                   "outcome_yearlyrep$asd_age_", "outcome_yearlyrep$asd_pc_",
                   "outcome_yearlyrep$nvi_age_", "outcome_yearlyrep$nvi_pc_",
                   "outcome_yearlyrep$fm_age_", "outcome_yearlyrep$fm_pc_",
                   "outcome_yearlyrep$gm_age_", "outcome_yearlyrep$gm_pc_",
                   "outcome_yearlyrep$lan_age_", "outcome_yearlyrep$lan_pc_",
                   "outcome_yearlyrep$ext_age_", "outcome_yearlyrep$ext_pc_",
                   "outcome_yearlyrep$int_pc_", "outcome_yearlyrep$int_age_"),
             newobj = "outcome_yearlyrep_small", completeCases = F,
             datasources = connections[c('genr')])

ds.dataFrame(x = c("outcome_yearlyrep$child_id", "outcome_yearlyrep$age_years", 
                   "outcome_yearlyrep$adhd_age_", "outcome_yearlyrep$adhd_pc_",                  
                   "outcome_yearlyrep$fm_age_", "outcome_yearlyrep$fm_pc_",
                   "outcome_yearlyrep$gm_age_", "outcome_yearlyrep$gm_pc_",
                   "outcome_yearlyrep$ext_age_", "outcome_yearlyrep$ext_pc_",
                   "outcome_yearlyrep$int_age_", "outcome_yearlyrep$int_pc_"),
             newobj = "outcome_yearlyrep_small", completeCases = F,
             datasources = connections[c('dnbc')])

ds.dataFrame(x = c("outcome_yearlyrep$child_id", "outcome_yearlyrep$age_years", 
                   "outcome_yearlyrep$adhd_age_", "outcome_yearlyrep$adhd_pc_",
                   "outcome_yearlyrep$asd_age_", "outcome_yearlyrep$asd_pc_",
                   "outcome_yearlyrep$nvi_age_", "outcome_yearlyrep$nvi_pc_",
                   "outcome_yearlyrep$fm_age_", "outcome_yearlyrep$fm_pc_",
                   "outcome_yearlyrep$gm_age_", "outcome_yearlyrep$gm_pc_",
                   "outcome_yearlyrep$lan_age_", "outcome_yearlyrep$lan_pc_",
                   "outcome_yearlyrep$ext_age_", "outcome_yearlyrep$ext_pc_",
                   "outcome_yearlyrep$int_pc_", "outcome_yearlyrep$int_age_"),
             newobj = "outcome_yearlyrep_small", completeCases = F,
             datasources = connections[c('eden')])

ds.dataFrame(x = c("outcome_yearlyrep$child_id", "outcome_yearlyrep$age_years", 
                   "outcome_yearlyrep$adhd_age_", "outcome_yearlyrep$adhd_pc_",
                   "outcome_yearlyrep$fm_age_", "outcome_yearlyrep$fm_pc_",
                   "outcome_yearlyrep$gm_age_", "outcome_yearlyrep$gm_pc_",
                   "outcome_yearlyrep$lan_age_", "outcome_yearlyrep$lan_pc_",
                   "outcome_yearlyrep$ext_age_", "outcome_yearlyrep$ext_pc_",
                   "outcome_yearlyrep$int_pc_", "outcome_yearlyrep$int_age_"),
             newobj = "outcome_yearlyrep_small", completeCases = F,
             datasources = connections[c('ninfea')])

ds.dataFrame(x = c("outcome_yearlyrep$child_id", "outcome_yearlyrep$age_years", 
                   "outcome_yearlyrep$adhd_age_", "outcome_yearlyrep$adhd_pc_",
                   "outcome_yearlyrep$nvi_age_", "outcome_yearlyrep$nvi_pc_",
                   "outcome_yearlyrep$ext_age_", "outcome_yearlyrep$ext_pc_",
                   "outcome_yearlyrep$int_pc_", "outcome_yearlyrep$int_age_"),
             newobj = "outcome_yearlyrep_small", completeCases = F,
             datasources = connections[c('abcd')])

ds.colnames('outcome_yearlyrep_small', datasources = connections)
ds.dim('core_nonrep', datasources = connections)
ds.dim('core_yearlyrep_small', datasources = connections)

# reshape from long format into wide format repeated df
ds.reShape(data.name = 'core_yearlyrep_small', 
           timevar.name = "age_years",
           idvar.name = "child_id",
           direction = "wide",
           newobj = "core_rep_w",
           datasources = connections)

ds.colnames('core_rep_w', datasources = connections)
ds.dataFrame(x = c("core_rep_w$child_id", "core_rep_w$edu_m_.0"),
             newobj = "core_rep_w", completeCases = F,
             datasources = connections)
ds.dim('core_rep_w', datasources = connections)
ds.dim('core_nonrep', datasources = connections) # check if it's the same

ds.reShape(data.name = 'outcome_yearlyrep_small', 
           timevar.name = "age_years",
           idvar.name = "child_id",
           direction = "wide",
           newobj = "outcome_wide",
           datasources = connections)

ds.colnames('outcome_wide', datasources = connections)
ds.dim('outcome_wide', datasources = connections)

#-3) Merge all tables using ds.merge
#Merge core_nonrep with core_yearlyrep
ds.merge(
  x.name = "core_nonrep",
  y.name = "core_rep_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_basic_w",
  datasources = connections
)
ds.merge(
  x.name = "outcome_wide",
  y.name = "df_basic_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_basic_w",
  datasources = connections
)

ds.colnames('df_basic_w', datasources = connections)
ds.dim('df_basic_w', datasources = connections)

#reshape back to long format

vars_genr <- c(paste0('ext_pc_.', 0:12), paste0('ext_age_.', 0:12), paste0('int_pc_.', 0:12),
               paste0('int_age_.', 0:12), paste0('adhd_pc_.', 0:12), paste0('adhd_age_.', 0:12),
               paste0('asd_pc_.', 0:12), paste0('asd_age_.', 0:12), paste0('fm_pc_.', 0:12),
               paste0('fm_age_.', 0:12), paste0('gm_pc_.', 0:12), paste0('gm_age_.', 0:12),
               paste0('nvi_pc_.', 0:12), paste0('nvi_age_.', 0:12), paste0('lan_pc_.', 0:12),
               paste0('lan_age_.', 0:12))

vars_genr

ds.reShape(data.name = 'df_basic_w', 
           varying = vars_genr,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_basic",
           datasources = connections[c('genr')])

vars_dnbc <- c(paste0('ext_pc_.', c(0:2, 7:9, 11:15, 18:21)), 
               paste0('ext_age_.', c(0:2, 7:9, 11:15, 18:21)), 
               paste0('int_pc_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('int_age_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('adhd_pc_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('adhd_age_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('fm_pc_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('fm_age_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('gm_pc_.', c(0:2, 7:9, 11:15, 18:21)),
               paste0('gm_age_.', c(0:2, 7:9, 11:15, 18:21)))
vars_dnbc

ds.reShape(data.name = 'df_basic_w', 
           varying = vars_dnbc,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_basic",
           datasources = connections[c('dnbc')])

vars_eden <- c(paste0('ext_pc_.', 0:8), paste0('ext_age_.', 0:8), paste0('int_pc_.', 0:8),
               paste0('int_age_.', 0:8), paste0('adhd_pc_.', 0:8), paste0('adhd_age_.', 0:8),
               paste0('asd_pc_.', 0:8), paste0('asd_age_.', 0:8), paste0('fm_pc_.', 0:8),
               paste0('fm_age_.', 0:8), paste0('gm_pc_.', 0:8), paste0('gm_age_.', 0:8),
               paste0('nvi_pc_.', 0:8), paste0('nvi_age_.', 0:8), paste0('lan_pc_.', 0:8),
               paste0('lan_age_.', 0:8))
vars_eden

ds.reShape(data.name = 'df_basic_w', 
           varying = vars_eden,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_basic",
           datasources = connections[c('eden')])

vars_ninfea <- c(paste0('ext_pc_.', c(0, 4, 10, 13)), paste0('ext_age_.', c(0, 4, 10, 13)), 
                 paste0('int_pc_.', c(0, 4, 10, 13)), paste0('int_age_.', c(0, 4, 10, 13)), 
                 paste0('adhd_pc_.', c(0, 4, 10, 13)), paste0('adhd_age_.', c(0, 4, 10, 13)), 
                 paste0('fm_pc_.', c(0, 4, 10, 13)), paste0('fm_age_.', c(0, 4, 10, 13)), 
                 paste0('gm_pc_.', c(0, 4, 10, 13)), paste0('gm_age_.', c(0, 4, 10, 13)),
               paste0('lan_pc_.', c(0, 4, 10, 13)), paste0('lan_age_.', c(0, 4, 10, 13)))
vars_ninfea

ds.reShape(data.name = 'df_basic_w', 
           varying = vars_ninfea,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_basic",
           datasources = connections[c('ninfea')])

vars_abcd <- c(paste0('ext_pc_.', c(0, 4:7, 10:13)), paste0('ext_age_.', c(0, 4:7, 10:13)), 
                 paste0('int_pc_.', c(0, 4:7, 10:13)), paste0('int_age_.', c(0, 4:7, 10:13)), 
                 paste0('adhd_pc_.', c(0, 4:7, 10:13)), paste0('adhd_age_.', c(0, 4:7, 10:13)), 
                 paste0('nvi_pc_.', c(0, 4:7, 10:13)), paste0('nvi_age_.', c(0, 4:7, 10:13)))
vars_abcd

ds.reShape(data.name = 'df_basic_w', 
           varying = vars_abcd,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_basic",
           datasources = connections[c('abcd')])

ds.colnames('df_basic', datasources = connections)
ds.dim('df_basic', datasources = connections)

################################# DESCRIPTIVES #################################
#----------------------------2a) Create Complete Cases--------------------------
ds.dataFrame("df_basic", newobj = "df_complete")

ds.replaceNA(x = 'df_complete$int_pc_', forNA = c(2000, 2000, 2000, 2000, 2000), 
             newobj = 'int_pc_2000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd', 'ninfea')])

ds.replaceNA(x = 'df_complete$ext_pc_', forNA = c(2000, 2000, 2000, 2000, 2000), 
             newobj = 'ext_pc_2000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd', 'ninfea')])

ds.replaceNA(x = 'df_complete$adhd_pc_', forNA = c(2000, 2000, 2000, 2000, 2000), 
             newobj = 'adhd_pc_2000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd', 'ninfea')])

ds.replaceNA(x = 'df_complete$asd_pc_', forNA = c(2000), 
             newobj = 'asd_pc_2000',
             datasources = connections[c('genr')])

ds.replaceNA(x = 'df_complete$fm_pc_', forNA = c(2000, 2000, 2000, 2000), 
             newobj = 'fm_pc_2000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.replaceNA(x = 'df_complete$gm_pc_', forNA = c(2000, 2000, 2000, 2000), 
             newobj = 'gm_pc_2000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.replaceNA(x = 'df_complete$lan_pc_', forNA = c(2000, 2000, 2000), 
             newobj = 'lan_pc_2000',
             datasources = connections[c('genr', 'eden', 'ninfea')])

ds.replaceNA(x = 'df_complete$nvi_pc_', forNA = c(2000, 2000, 2000), 
             newobj = 'nvi_pc_2000',
             datasources = connections[c('genr', 'eden', 'abcd')])

ds.colnames('df_complete', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd', 'ninfea')])

ds.make('int_pc_2000 + ext_pc_2000 + adhd_pc_2000 + fm_pc_2000 + gm_pc_2000', 
        newobj = 'outcome_2000',
        datasources = connections[c('dnbc')])

ds.make('int_pc_2000 + ext_pc_2000 + adhd_pc_2000 + fm_pc_2000 + gm_pc_2000 + nvi_pc_2000 + lan_pc_2000', 
        newobj = 'outcome_2000',
        datasources = connections[c('eden')])

ds.make('int_pc_2000 + ext_pc_2000 + adhd_pc_2000 + asd_pc_2000 + fm_pc_2000 + gm_pc_2000 + nvi_pc_2000 + lan_pc_2000', 
        newobj = 'outcome_2000',
        datasources = connections[c('genr')])

ds.make('int_pc_2000 + ext_pc_2000 + adhd_pc_2000 + fm_pc_2000 + gm_pc_2000 + lan_pc_2000', 
        newobj = 'outcome_2000',
        datasources = connections[c('ninfea')])

ds.make('int_pc_2000 + ext_pc_2000 + adhd_pc_2000 + nvi_pc_2000', 
        newobj = 'outcome_2000',
        datasources = connections[c('abcd')])

ds.dataFrame(c('df_complete', 'outcome_2000'), newobj = 'df_complete')

ds.dataFrameSubset('df_complete', V1.name = 'outcome_2000', V2.name = '10000', 
                   Boolean.operator = '!=', newobj = 'df_complete', 
                   datasources = connections[c('dnbc')])

ds.dataFrameSubset('df_complete', V1.name = 'outcome_2000', V2.name = '14000', 
                   Boolean.operator = '!=', newobj = 'df_complete', 
                   datasources = connections[c('eden')])

ds.dataFrameSubset('df_complete', V1.name = 'outcome_2000', V2.name = '16000', 
                   Boolean.operator = '!=', newobj = 'df_complete', 
                   datasources = connections[c('genr')])

ds.dataFrameSubset('df_complete', V1.name = 'outcome_2000', V2.name = '12000', 
                   Boolean.operator = '!=', newobj = 'df_complete', 
                   datasources = connections[c('ninfea')])

ds.dataFrameSubset('df_complete', V1.name = 'outcome_2000', V2.name = '8000', 
                   Boolean.operator = '!=', newobj = 'df_complete', 
                   datasources = connections[c('abcd')])

ds.reShape('df_complete', timevar.name = 'time', idvar.name = 'child_id', 
           v.names = c("ext_pc_", "int_pc_", "ext_age_", "int_age_", "lan_pc_", 
                       "lan_age_", "nvi_age_", "nvi_pc_", "gm_pc_", "gm_age_", "fm_pc_", 
                       "fm_age_", "asd_pc_", "asd_age_", "adhd_age_", "adhd_pc_"),
           direction = 'wide', newobj = 'df_complete_w', datasources = connections[c('genr')])

ds.reShape('df_complete', timevar.name = 'time', idvar.name = 'child_id', 
           v.names = c("ext_pc_", "int_pc_", "ext_age_", "int_age_", "gm_pc_", 
                       "gm_age_", "fm_pc_", "fm_age_", "adhd_age_", "adhd_pc_"),
           direction = 'wide', newobj = 'df_complete_w', datasources = connections[c('dnbc')])

ds.reShape('df_complete', timevar.name = 'time', idvar.name = 'child_id', 
           v.names = c("ext_pc_", "int_pc_", "ext_age_", "int_age_", "lan_pc_", 
                       "lan_age_", "nvi_age_", "nvi_pc_", "gm_pc_", "gm_age_", "fm_pc_", 
                       "fm_age_", "asd_pc_", "asd_age_", "adhd_age_", "adhd_pc_"),
           direction = 'wide', newobj = 'df_complete_w', datasources = connections[c('eden')])

ds.reShape('df_complete', timevar.name = 'time', idvar.name = 'child_id', 
           v.names = c("ext_pc_", "int_pc_", "ext_age_", "int_age_", "lan_pc_", 
                       "lan_age_", "gm_pc_", "gm_age_", "fm_pc_", 
                       "fm_age_", "adhd_age_", "adhd_pc_"),
           direction = 'wide', newobj = 'df_complete_w', datasources = connections[c('ninfea')])

ds.reShape('df_complete', timevar.name = 'time', idvar.name = 'child_id', 
           v.names = c("ext_pc_", "int_pc_", "ext_age_", "int_age_", "nvi_pc_", 
                       "nvi_age_", "adhd_age_", "adhd_pc_"),
           direction = 'wide', newobj = 'df_complete_w', datasources = connections[c('abcd')])

ds.asNumeric('df_complete_w$preg_dep', newobj = 'preg_dep_numeric')

ds.replaceNA('preg_dep_numeric', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'preg_dep_2000')

ds.dataFrame(c('df_complete_w', 'preg_dep_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'preg_dep_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

# Sample size of entire cohort
ds.unique('df_basic$child_id')
N_full_cohort <- ds.length('unique.newobj')

# How many have data on exposure (preg_dep)
ds.dataFrame(c('df_basic$child_id','df_basic$preg_dep'),
             newobj = 'df_exp',
             completeCases = T)
ds.unique('df_exp$child_id')
N_EXP <- ds.length('unique.newobj')

# How many have data on exposure (preg_dep) AND at least one outcome
N_EXP_OUT <- ds.dim('df_complete_w')

# Select complete covariates
ds.asNumeric('df_complete_w$sex', newobj = 'sex_numeric')

ds.replaceNA('sex_numeric', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'sex_2000')

ds.dataFrame(c('df_complete_w', 'sex_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'sex_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.asNumeric('df_complete_w$edu_m_.0', newobj = 'edu_m_.0_numeric')

ds.replaceNA('edu_m_.0_numeric', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'edu_m_.0_2000')

ds.dataFrame(c('df_complete_w', 'edu_m_.0_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'edu_m_.0_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.asNumeric('df_complete_w$agebirth_m_y', newobj = 'agebirth_m_y')

ds.replaceNA('agebirth_m_y', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'agebirth_m_y_2000')

ds.dataFrame(c('df_complete_w', 'agebirth_m_y_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'agebirth_m_y_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.asNumeric('df_complete_w$preg_alc', newobj = 'preg_alc_numeric')

ds.replaceNA('preg_alc_numeric', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'preg_alc_2000')

ds.dataFrame(c('df_complete_w', 'preg_alc_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'preg_alc_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.asNumeric('df_complete_w$preg_smk', newobj = 'preg_smk_numeric')

ds.replaceNA('preg_smk_numeric', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'preg_smk_2000')

ds.dataFrame(c('df_complete_w', 'preg_smk_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'preg_smk_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.asNumeric('df_complete_w$height_m', newobj = 'height_m')

ds.replaceNA('height_m', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'height_m_2000')

ds.dataFrame(c('df_complete_w', 'height_m_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'height_m_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.asNumeric('df_complete_w$prepreg_weight', newobj = 'prepreg_weight')

ds.replaceNA('prepreg_weight', forNA = c(2000,2000,2000,2000,2000), 
             newobj = 'prepreg_weight_2000')

ds.dataFrame(c('df_complete_w', 'prepreg_weight_2000'), newobj = 'df_complete_w')

ds.dataFrameSubset('df_complete_w', V1.name = 'prepreg_weight_2000', V2.name = '2000',
                   Boolean.operator = '!=', newobj = 'df_complete_w')

ds.dim('df_complete_w')

ds.colnames('df_complete_w')

# How many have data on exposure (preg_dep) AND at least one outcome
N_EXP_OUT_COV <- ds.dim('df_complete_w')

flowchart_list <- list(N_full_cohort = N_full_cohort, N_EXP = N_EXP, 
                       N_EXP_OUT = N_EXP_OUT, N_EXP_OUT_COV = N_EXP_OUT_COV)

saveRDS(flowchart_list, file="results/00_descriptives/flowchart_ECCN.RData")

vars_genr <- c(paste0('ext_pc_.', 0:12), paste0('ext_age_.', 0:12), paste0('int_pc_.', 0:12),
               paste0('int_age_.', 0:12), paste0('adhd_pc_.', 0:12), paste0('adhd_age_.', 0:12),
               paste0('asd_pc_.', 0:12), paste0('asd_age_.', 0:12), paste0('fm_pc_.', 0:12),
               paste0('fm_age_.', 0:12), paste0('gm_pc_.', 0:12), paste0('gm_age_.', 0:12),
               paste0('nvi_pc_.', 0:12), paste0('nvi_age_.', 0:12), paste0('lan_pc_.', 0:12),
               paste0('lan_age_.', 0:12))

vars_genr

ds.reShape(data.name = 'df_complete_w', 
           varying = vars_genr,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_complete_l",
           datasources = connections[c('genr')])

vars_dnbc <- c(paste0('ext_pc_.', c(7:8, 11:14, 18:20)), 
               paste0('ext_age_.', c(7:8, 11:14, 18:20)), 
               paste0('int_pc_.', c(7:8, 11:14, 18:20)),
               paste0('int_age_.', c(7:8, 11:14, 18:20)),
               paste0('adhd_pc_.', c(7:8, 11:14, 18:20)),
               paste0('adhd_age_.', c(7:8, 11:14, 18:20)),
               paste0('fm_pc_.', c(7:8, 11:14, 18:20)),
               paste0('fm_age_.', c(7:8, 11:14, 18:20)),
               paste0('gm_pc_.', c(7:8, 11:14, 18:20)),
               paste0('gm_age_.', c(7:8, 11:14, 18:20)))
vars_dnbc

ds.reShape(data.name = 'df_complete_w', 
           varying = vars_dnbc,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_complete_l",
           datasources = connections[c('dnbc')])

vars_eden <- c(paste0('ext_pc_.', 0:8), paste0('ext_age_.', 0:8), paste0('int_pc_.', 0:8),
               paste0('int_age_.', 0:8), paste0('adhd_pc_.', 0:8), paste0('adhd_age_.', 0:8),
               paste0('asd_pc_.', 0:8), paste0('asd_age_.', 0:8), paste0('fm_pc_.', 0:8),
               paste0('fm_age_.', 0:8), paste0('gm_pc_.', 0:8), paste0('gm_age_.', 0:8),
               paste0('nvi_pc_.', 0:8), paste0('nvi_age_.', 0:8), paste0('lan_pc_.', 0:8),
               paste0('lan_age_.', 0:8))
vars_eden

ds.reShape(data.name = 'df_complete_w', 
           varying = vars_eden,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_complete_l",
           datasources = connections[c('eden')])

vars_ninfea <- c(paste0('ext_pc_.', c(0, 4, 13)), paste0('ext_age_.', c(0, 4, 13)), 
                 paste0('int_pc_.', c(0, 4, 13)), paste0('int_age_.', c(0, 4, 13)), 
                 paste0('adhd_pc_.', c(0, 4, 13)), paste0('adhd_age_.', c(0, 4, 13)), 
                 paste0('fm_pc_.', c(0, 4, 13)), paste0('fm_age_.', c(0, 4, 13)), 
                 paste0('gm_pc_.', c(0, 4, 13)), paste0('gm_age_.', c(0, 4, 13)),
                 paste0('lan_pc_.', c(0, 4, 13)), paste0('lan_age_.', c(0, 4, 13)))
vars_ninfea

ds.reShape(data.name = 'df_complete_w', 
           varying = vars_ninfea,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_complete_l",
           datasources = connections[c('ninfea')])

vars_abcd <- c(paste0('ext_pc_.', c(5:6, 10:12)), paste0('ext_age_.', c(5:6, 10:12)), 
               paste0('int_pc_.', c(5:6, 10:12)), paste0('int_age_.', c(5:6, 10:12)), 
               paste0('adhd_pc_.', c(5:6, 10:12)), paste0('adhd_age_.', c(5:6, 10:12)), 
               paste0('nvi_pc_.', c(5:6, 10:12)), paste0('nvi_age_.', c(5:6, 10:12)))
vars_abcd

ds.reShape(data.name = 'df_complete_w', 
           varying = vars_abcd,
           idvar.name = "child_id",
           direction = "long",
           newobj = "df_complete_l",
           datasources = connections[c('abcd')])

ds.colnames('df_complete_l', datasources = connections)
ds.dim('df_complete_l', datasources = connections)

ds.replaceNA(x = 'df_complete_l$int_pc_', forNA = c(3000, 3000, 3000, 3000, 3000), 
             newobj = 'int_pc_3000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

ds.dataFrame(c('df_complete_l', 'int_pc_3000'), newobj = 'df_complete_l')

ds.replaceNA(x = 'df_complete_l$ext_pc_', forNA = c(3000, 3000, 3000, 3000, 3000), 
             newobj = 'ext_pc_3000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

ds.dataFrame(c('df_complete_l', 'ext_pc_3000'), newobj = 'df_complete_l')

ds.replaceNA(x = 'df_complete_l$adhd_pc_', forNA = c(3000, 3000, 3000, 3000, 3000), 
             newobj = 'adhd_pc_3000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

ds.dataFrame(c('df_complete_l', 'adhd_pc_3000'), newobj = 'df_complete_l')

ds.replaceNA(x = 'df_complete_l$asd_pc_', forNA = c(3000), 
             newobj = 'asd_pc_3000',
             datasources = connections[c('genr')])

ds.dataFrame(c('df_complete_l', 'asd_pc_3000'), newobj = 'df_complete_l',
             datasources = connections[c('genr')])

ds.replaceNA(x = 'df_complete_l$fm_pc_', forNA = c(3000, 3000, 3000, 3000), 
             newobj = 'fm_pc_3000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_complete_l', 'fm_pc_3000'), newobj = 'df_complete_l',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.replaceNA(x = 'df_complete_l$gm_pc_', forNA = c(3000, 3000, 3000, 3000), 
             newobj = 'gm_pc_3000',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_complete_l', 'gm_pc_3000'), newobj = 'df_complete_l',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.replaceNA(x = 'df_complete_l$lan_pc_', forNA = c(3000, 3000, 3000), 
             newobj = 'lan_pc_3000',
             datasources = connections[c('genr', 'eden', 'ninfea')])

ds.dataFrame(c('df_complete_l', 'lan_pc_3000'), newobj = 'df_complete_l',
             datasources = connections[c('genr', 'eden', 'ninfea')])

ds.replaceNA(x = 'df_complete_l$nvi_pc_', forNA = c(3000, 3000, 3000), 
             newobj = 'nvi_pc_3000',
             datasources = connections[c('genr', 'eden', 'abcd')])

ds.dataFrame(c('df_complete_l', 'nvi_pc_3000'), newobj = 'df_complete_l',
             datasources = connections[c('genr', 'eden', 'abcd')])

# CHECK HOW MANY
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$int_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_int')
ds.unique('complete_int$child_id', newobj='unique_id_int')
N_int <- ds.length('unique_id_int')
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$ext_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_ext')
ds.unique('complete_ext$child_id', newobj='unique_id_ext')
N_ext <- ds.length('unique_id_ext')
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$adhd_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_adhd')
ds.unique('complete_adhd$child_id', newobj='unique_id_adhd')
N_adhd <- ds.length('unique_id_adhd')
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$asd_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_asd',
                   datasources = connections[c('genr')])
ds.unique('complete_asd$child_id', newobj='unique_id_asd',
          datasources = connections[c('genr')])
N_asd <- ds.length('unique_id_asd',
                   datasources = connections[c('genr')])
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$fm_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_fm',
                   datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.unique('complete_fm$child_id', newobj='unique_id_fm',
          datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
N_fm <- ds.length('unique_id_fm',
                  datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$gm_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_gm',
                   datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.unique('complete_gm$child_id', newobj='unique_id_gm',
          datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
N_gm <- ds.length('unique_id_gm',
                  datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$lan_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_lan',
                   datasources = connections[c('genr', 'eden', 'ninfea')])
ds.unique('complete_lan$child_id', newobj='unique_id_lan',
          datasources = connections[c('genr', 'eden', 'ninfea')])
N_lan <- ds.length('unique_id_lan',
                   datasources = connections[c('genr', 'eden', 'ninfea')])
ds.dataFrameSubset('df_complete_l', V1.name = 'df_complete_l$nvi_pc_3000', V2.name = '3000',
                   Boolean.operator = '!=', newobj = 'complete_nvi',
                   datasources = connections[c('genr', 'eden', 'abcd')])
ds.unique('complete_nvi$child_id', newobj='unique_id_nvi',
          datasources = connections[c('genr', 'eden', 'abcd')])
N_nvi <- ds.length('unique_id_nvi',
                   datasources = connections[c('genr', 'eden', 'abcd')])

# Sample size:
samples <- list(N_int=N_int,N_ext=N_ext,N_adhd=N_adhd,N_asd=N_asd,N_fm=N_fm,
                N_gm=N_gm,N_lan=N_lan,N_nvi=N_nvi)
saveRDS(samples, file="results/00_descriptives/samples_ECCN.RData")

# Add prepreg_BMI variable---------------------------
# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "df_complete_w$prepreg_weight/((df_complete_w$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections)

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections)


# bind to df
ds.cbind(
  x = c("df_complete_w","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "df_complete_w",
  datasources = connections
)

# Add COB variable------------------------------
# Make a dummy variable for cob_m
# cob_m = Born in country of cohort = 0, Born in EU country (outside cohort country) = 1, Born in other country = 2

ds.Boole(V1 = "df_complete_w$cob_m",
         V2 = 0,
         Boolean.operator = "==",
         numeric.output = TRUE, 
         na.assign = "NA", 
         newobj = "cob_same_as_cohort",
         datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
ds.asFactor(input.var.name = 'cob_same_as_cohort', newobj.name = 'cob_other_country_f', 
            datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
#cob_other_country_f = Born in country of cohort = 1, Born in another country = 0

# check 
ds.table(rvar = 'df_complete_w$cob_m', datasources = connections[c('genr','eden', 
                                                                   'abcd', 'ninfea')])
ds.table(rvar = 'cob_other_country_f', datasources = connections[c('genr', 'eden', 
                                                                   'abcd', 'ninfea')])

# bind to df
ds.cbind(
  x = c("df_complete_w","cob_other_country_f"),
  DataSHIELD.checks = FALSE,
  newobj = "df_complete_w",
  datasources = connections[c('genr', 'eden', 'abcd', 'ninfea')]
)

# Cumulative exposure-----------------------------

# convert to numeric to be able to add them up
ds.asNumeric(x.name = "df_complete_w$prepreg_dep",
             newobj = "num_prepreg_dep",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.asNumeric(x.name = "df_complete_w$preg_dep",
             newobj = "num_preg_dep",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.asNumeric(x.name = "df_complete_w$ppd",
             newobj = "num_ppd",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.replaceNA(x = "num_prepreg_dep", forNA = c(10,10,10), newobj = "num_prepreg_dep_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.replaceNA(x = "num_preg_dep", forNA = c(10,10,10), newobj = "num_preg_dep_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.replaceNA(x = "num_ppd", forNA = c(10,10,10), newobj = "num_ppd_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.make(toAssign = "num_prepreg_dep_10 + num_preg_dep_10 + num_ppd_10",
        newobj = "cumul_dep", 
        datasources = connections[c('genr', 'dnbc', 'ninfea')])

# check
ds.table(rvar = 'cumul_dep', datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep", values2replace.vector = c(0,1,2,3), 
                new.values.vector = c(0,1,2,3), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep_weighted", values2replace.vector = c(10,11,12), 
                new.values.vector = c(0,1.5,3), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep_weighted", values2replace.vector = c(20,21,30), 
                new.values.vector = c(NA,NA,NA), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.table(rvar = 'cumul_dep_weighted', datasources = connections[c('genr', 'dnbc', 'ninfea')])

# bind to df
ds.cbind(
  x = c("df_complete_w","cumul_dep_weighted"),
  DataSHIELD.checks = FALSE,
  newobj = "df_complete_w",
  datasources = connections[c('genr', 'dnbc', 'ninfea')]
)

# Check # of follow ups----------------------------
ds.tapply.assign(X.name = "df_basic$int_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections)
int_table <- ds.table(rvar = 'vector1$N', datasources = connections)
num_int <- int_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$ext_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections)
ext_table <- ds.table(rvar = 'vector1$N', datasources = connections)
num_ext <- ext_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$adhd_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections)
adhd_table <- ds.table(rvar = 'vector1$N', datasources = connections)
num_adhd <- adhd_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$asd_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'eden')])
asd_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'eden')])
num_asd <- asd_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$fm_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
fm_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
num_fm <- fm_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$gm_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
gm_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
num_gm <- gm_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$lan_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'eden', 'ninfea')])
lan_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'eden', 'ninfea')])
num_lan <- lan_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$nvi_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'eden','abcd')])
nvi_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'eden', 'abcd')])
num_nvi <- nvi_table$output.list$TABLE_rvar.by.study_counts

follow_up <- list(num_int=num_int,num_ext=num_ext,num_adhd=num_adhd,
                  num_asd=num_asd,num_fm=num_fm,num_gm=num_gm,num_lan=num_lan,
                  num_nvi=num_nvi)

# DESCRIPTIVES:-----------------------------

# Sample size:
totalsize <- ds.dim('core_nonrep')
samplesize <- ds.dim('df_complete_w')
child_sex <- ds.table('df_complete_w$sex')
child_sex <- child_sex$output.list[3]$TABLE_rvar.by.study_counts
agebirth_m_y_mean <- ds.mean('df_complete_w$agebirth_m_y')
agebirth_m_y_mean <- agebirth_m_y_mean$Mean.by.Study[,1]
agebirth_m_y_var <- ds.var('df_complete_w$agebirth_m_y')
agebirth_m_y_sd <- sqrt(agebirth_m_y_var$Variance.by.Study[,1])
edu_m_.0 <- ds.table('df_complete_w$edu_m_.0')
edu_m_.0 <- edu_m_.0$output.list[3]$TABLE_rvar.by.study_counts
preg_dep <- ds.table('df_complete_w$preg_dep')
preg_dep <- preg_dep$output.list[3]$TABLE_rvar.by.study_counts
ppd <- ds.table('df_complete_w$ppd')
ppd <- ppd$output.list[3]$TABLE_rvar.by.study_counts
prepreg_dep <- ds.table('df_complete_w$prepreg_dep', datasources = connections[c('genr','dnbc','ninfea')])
prepreg_dep <- prepreg_dep$output.list[3]$TABLE_rvar.by.study_counts
cumul_dep_weighted <- ds.table('df_complete_w$cumul_dep_weighted', datasources = connections[c('genr','dnbc','ninfea')])
cumul_dep_weighted <- cumul_dep_weighted$output.list[3]$TABLE_rvar.by.study_counts
cob_other_country_f <- ds.table('df_complete_w$cob_other_country_f', datasources = connections[c('abcd','genr','eden','ninfea')])
cob_other_country_f <- cob_other_country_f$output.list[3]$TABLE_rvar.by.study_counts
preg_alc <- ds.table('df_complete_w$preg_alc')
preg_alc <- preg_alc$output.list[3]$TABLE_rvar.by.study_counts
preg_smk <- ds.table('df_complete_w$preg_smk')
preg_smk <- preg_smk$output.list[3]$TABLE_rvar.by.study_counts
prepreg_BMI_mean <- ds.mean('df_complete_w$prepreg_BMI')
prepreg_BMI_mean <- prepreg_BMI_mean$Mean.by.Study[,1]
prepreg_BMI_var <- ds.var('df_complete_w$prepreg_BMI')
prepreg_BMI_sd <- sqrt(prepreg_BMI_var$Variance.by.Study[,1])
int_pc_mean <- ds.mean('df_complete$int_pc_')
int_pc_mean <- int_pc_mean$Mean.by.Study[,1]
int_pc_var <- ds.var('df_complete$int_pc_')
int_pc_sd <- sqrt(int_pc_var$Variance.by.Study[,1])
int_age_mean <- ds.mean('df_complete$int_age_')
int_age_mean <- int_age_mean$Mean.by.Study[,1]
int_age_var <- ds.var('df_complete$int_age_')
int_age_sd <- sqrt(int_age_var$Variance.by.Study[,1])
ext_pc_mean <- ds.mean('df_complete$ext_pc_')
ext_pc_mean <- ext_pc_mean$Mean.by.Study[,1]
ext_pc_var <- ds.var('df_complete$ext_pc_')
ext_pc_sd <- sqrt(ext_pc_var$Variance.by.Study[,1])
ext_age_mean <- ds.mean('df_complete$ext_age_')
ext_age_mean <- ext_age_mean$Mean.by.Study[,1]
ext_age_var <- ds.var('df_complete$ext_age_')
ext_age_sd <- sqrt(ext_age_var$Variance.by.Study[,1])
adhd_pc_mean <- ds.mean('df_complete$adhd_pc_')
adhd_pc_mean <- adhd_pc_mean$Mean.by.Study[,1]
adhd_pc_var <- ds.var('df_complete$adhd_pc_')
adhd_pc_sd <- sqrt(adhd_pc_var$Variance.by.Study[,1])
adhd_age_mean <- ds.mean('df_complete$adhd_age_')
adhd_age_mean <- adhd_age_mean$Mean.by.Study[,1]
adhd_age_var <- ds.var('df_complete$adhd_age_')
adhd_age_sd <- sqrt(adhd_age_var$Variance.by.Study[,1])
asd_pc_mean <- ds.mean('df_complete$asd_pc_',
                       datasources = connections[c('genr')])
asd_pc_mean <- asd_pc_mean$Mean.by.Study[,1]
asd_pc_var <- ds.var('df_complete$asd_pc_',
                     datasources = connections[c('genr')])
asd_pc_sd <- sqrt(asd_pc_var$Variance.by.Study[,1])
asd_age_mean <- ds.mean('df_complete$asd_age_',
                        datasources = connections[c('genr')])
asd_age_mean <- asd_age_mean$Mean.by.Study[,1]
asd_age_var <- ds.var('df_complete$asd_age_',
                      datasources = connections[c('genr')])
asd_age_sd <- sqrt(asd_age_var$Variance.by.Study[,1])
fm_pc_mean <- ds.mean('df_complete$fm_pc_',
                      datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
fm_pc_mean <- fm_pc_mean$Mean.by.Study[,1]
fm_pc_var <- ds.var('df_complete$fm_pc_',
                    datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
fm_pc_sd <- sqrt(fm_pc_var$Variance.by.Study[,1])
fm_age_mean <- ds.mean('df_complete$fm_age_',
                       datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
fm_age_mean <- fm_age_mean$Mean.by.Study[,1]
fm_age_var <- ds.var('df_complete$fm_age_',
                     datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
fm_age_sd <- sqrt(fm_age_var$Variance.by.Study[,1])
gm_pc_mean <- ds.mean('df_complete$gm_pc_', 
                      datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
gm_pc_mean <- gm_pc_mean$Mean.by.Study[,1]
gm_pc_var <- ds.var('df_complete$gm_pc_',
                    datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
gm_pc_sd <- sqrt(gm_pc_var$Variance.by.Study[,1])
gm_age_mean <- ds.mean('df_complete$gm_age_',
                       datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
gm_age_mean <- gm_age_mean$Mean.by.Study[,1]
gm_age_var <- ds.var('df_complete$gm_age_',
                     datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
gm_age_sd <- sqrt(gm_age_var$Variance.by.Study[,1])
lan_pc_mean <- ds.mean('df_complete$lan_pc_',
                       datasources = connections[c('genr', 'eden', 'ninfea')])
lan_pc_mean <- lan_pc_mean$Mean.by.Study[,1]
lan_pc_var <- ds.var('df_complete$lan_pc_',
                     datasources = connections[c('genr', 'eden', 'ninfea')])
lan_pc_sd <- sqrt(lan_pc_var$Variance.by.Study[,1])
lan_age_mean <- ds.mean('df_complete$lan_age_',
                        datasources = connections[c('genr', 'eden', 'ninfea')])
lan_age_mean <- lan_age_mean$Mean.by.Study[,1]
lan_age_var <- ds.var('df_complete$lan_age_',
                      datasources = connections[c('genr', 'eden', 'ninfea')])
lan_age_sd <- sqrt(lan_age_var$Variance.by.Study[,1])
nvi_pc_mean <- ds.mean('df_complete$nvi_pc_',
                       datasources = connections[c('genr', 'eden', 'abcd')])
nvi_pc_mean <- nvi_pc_mean$Mean.by.Study[,1]
nvi_pc_var <- ds.var('df_complete$nvi_pc_',
                     datasources = connections[c('genr', 'eden', 'abcd')])
nvi_pc_sd <- sqrt(nvi_pc_var$Variance.by.Study[,1])
nvi_age_mean <- ds.mean('df_complete$nvi_age_',
                        datasources = connections[c('genr', 'eden', 'abcd')])
nvi_age_mean <- nvi_age_mean$Mean.by.Study[,1]
nvi_age_var <- ds.var('df_complete$nvi_age_',
                      datasources = connections[c('genr', 'eden', 'abcd')])
nvi_age_sd <- sqrt(nvi_age_var$Variance.by.Study[,1])

descriptives <- list(
  total_sample_N = totalsize,
  study_sample_N = samplesize,
  num_follow_up = follow_up,
  child_sex = child_sex,
  agebirth_m_y = list(mean = agebirth_m_y_mean, SD = agebirth_m_y_sd),
  edu_m_.0 = edu_m_.0,
  prepreg_BMI = list(mean = prepreg_BMI_mean, SD = prepreg_BMI_sd),
  preg_dep = preg_dep,
  ppd = ppd,
  prepreg_dep = prepreg_dep,
  cumul_dep_weighted = cumul_dep_weighted,
  cob_other_country_f = cob_other_country_f,
  preg_alc = preg_alc,
  preg_smk = preg_smk,
  int_pc = list(mean = int_pc_mean, SD = int_pc_sd),
  int_age = list(mean = int_age_mean, SD = int_age_sd 
  ),
  ext_pc = list(mean = ext_pc_mean, SD = ext_pc_sd),
  ext_age = list(mean = ext_age_mean, SD = ext_age_sd 
  ),
  adhd_pc = list(mean = adhd_pc_mean, SD = adhd_pc_sd),
  adhd_age = list(mean = adhd_age_mean, SD = adhd_age_sd 
  ),
  asd_pc = list(mean = asd_pc_mean, SD = asd_pc_sd),
  asd_age = list(mean = asd_age_mean, SD = asd_age_sd 
  ),
  fm_pc = list(mean = fm_pc_mean, SD = fm_pc_sd),
  fm_age = list(mean = fm_age_mean, SD = fm_age_sd 
  ),
  gm_pc = list(mean = gm_pc_mean, SD = gm_pc_sd),
  gm_age = list(mean = gm_age_mean, SD = gm_age_sd 
  ),
  lan_pc = list(mean = lan_pc_mean, SD = lan_pc_sd),
  lan_age = list(mean = lan_age_mean, SD = lan_age_sd 
  ),
  nvi_pc = list(mean = nvi_pc_mean, SD = nvi_pc_sd),
  nvi_age = list(mean = nvi_age_mean, SD = nvi_age_sd 
  )
)
saveRDS(descriptives, file="results/00_descriptives/descriptives_ECCN_included.RData")

#----------------------------2b) Create Excluded Cases--------------------------
dim_list <- ds.dim("df_complete_w")
row_counts <- sapply(dim_list, function(x) x[1])

ds.rep(x1 = 1, times = row_counts['dimensions of df_complete_w in genr'], 
       newobj = "ones_vector", source.times = 'c', datasources = connections[c('genr')])
ds.rep(x1 = 1, times = row_counts['dimensions of df_complete_w in dnbc'], 
       newobj = "ones_vector", source.times = 'c', datasources = connections[c('dnbc')])
ds.rep(x1 = 1, times = row_counts['dimensions of df_complete_w in ninfea'], 
       newobj = "ones_vector", source.times = 'c', datasources = connections[c('ninfea')])
ds.rep(x1 = 1, times = row_counts['dimensions of df_complete_w in eden'], 
       newobj = "ones_vector", source.times = 'c', datasources = connections[c('eden')])
ds.rep(x1 = 1, times = row_counts['dimensions of df_complete_w in abcd'], 
       newobj = "ones_vector", source.times = 'c', datasources = connections[c('abcd')])
ds.length('ones_vector')
ds.dataFrame(x = c("df_complete_w$child_id","ones_vector"), 
             newobj = "df_ones")

ds.merge(x.name = 'df_basic_w', y.name = 'df_ones', by.x.names = c('child_id'), 
         by.y.names = c('child_id'), all.x = TRUE, all.y = FALSE, # make sure extra rows in df_complete_l are not taken
         newobj = 'df_b_ones')

ds.replaceNA(x = 'df_b_ones$ones_vector', 
             forNA = rep(0, times = length(connections)),
             newobj = 'ind_complete')

ds.table('df_b_ones$ones_vector')
ds.table('df_b_ones$ind_complete')

# Create the logical condition
ds.dataFrameSubset(
  df.name = "df_b_ones",
  V1.name = "df_b_ones$ind_complete",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "excluded_subset"
)

ds.table('excluded_subset$sex', 'excluded_subset$ind_complete')
ds.meanSdGp('excluded_subset$agebirth_m_y', 'excluded_subset$ind_complete')

# Add prepreg_BMI variable---------------------------
# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "excluded_subset$prepreg_weight/((excluded_subset$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections)

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections)


# bind to df
ds.cbind(
  x = c("excluded_subset","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "excluded_subset",
  datasources = connections
)

# Add COB variable------------------------------
# Make a dummy variable for cob_m
# cob_m = Born in country of cohort = 0, Born in EU country (outside cohort country) = 1, Born in other country = 2

ds.Boole(V1 = "excluded_subset$cob_m",
         V2 = 0,
         Boolean.operator = "==",
         numeric.output = TRUE, 
         na.assign = "NA", 
         newobj = "cob_same_as_cohort",
         datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
ds.asFactor(input.var.name = 'cob_same_as_cohort', newobj.name = 'cob_other_country_f', 
            datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
#cob_other_country_f = Born in country of cohort = 1, Born in another country = 0

# check 
ds.table(rvar = 'excluded_subset$cob_m', datasources = connections[c('genr', 'eden', 
                                                                     'abcd', 'ninfea')])
ds.table(rvar = 'cob_other_country_f', datasources = connections[c('genr', 'eden', 
                                                                   'abcd', 'ninfea')])

# bind to df
ds.cbind(
  x = c("excluded_subset","cob_other_country_f"),
  DataSHIELD.checks = FALSE,
  newobj = "excluded_subset",
  datasources = connections[c('genr', 'eden', 'abcd', 'ninfea')]
)

# Cumulative exposure-----------------------------

# convert to numeric to be able to add them up
ds.asNumeric(x.name = "excluded_subset$prepreg_dep",
             newobj = "num_prepreg_dep",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.asNumeric(x.name = "excluded_subset$preg_dep",
             newobj = "num_preg_dep",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.asNumeric(x.name = "excluded_subset$ppd",
             newobj = "num_ppd",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.replaceNA(x = "num_prepreg_dep", forNA = c(10,10,10), newobj = "num_prepreg_dep_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.replaceNA(x = "num_preg_dep", forNA = c(10,10,10), newobj = "num_preg_dep_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.replaceNA(x = "num_ppd", forNA = c(10,10,10), newobj = "num_ppd_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.make(toAssign = "num_prepreg_dep_10 + num_preg_dep_10 + num_ppd_10",
        newobj = "cumul_dep", 
        datasources = connections[c('genr', 'dnbc', 'ninfea')])

# check
ds.table(rvar = 'cumul_dep', datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep", values2replace.vector = c(0,1,2,3), 
                new.values.vector = c(0,1,2,3), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep_weighted", values2replace.vector = c(10,11,12), 
                new.values.vector = c(0,1.5,3), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep_weighted", values2replace.vector = c(20,21,30), 
                new.values.vector = c(NA,NA,NA), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.table(rvar = 'cumul_dep_weighted', datasources = connections[c('genr', 'dnbc', 'ninfea')])

# bind to df
ds.cbind(
  x = c("excluded_subset","cumul_dep_weighted"),
  DataSHIELD.checks = FALSE,
  newobj = "excluded_subset",
  datasources = connections[c('genr', 'dnbc', 'ninfea')]
)

# DESCRIPTIVES:-----------------------------

# Sample size:
samplesize_EX <- ds.table('df_b_ones$ind_complete')$output.list$TABLE_rvar.by.study_counts
child_sex <- ds.table('excluded_subset$sex')
child_sex <- child_sex$output.list[3]$TABLE_rvar.by.study_counts
agebirth_m_y_mean <- ds.mean('excluded_subset$agebirth_m_y')
agebirth_m_y_mean <- agebirth_m_y_mean$Mean.by.Study[,1]
agebirth_m_y_var <- ds.var('excluded_subset$agebirth_m_y')
agebirth_m_y_sd <- sqrt(agebirth_m_y_var$Variance.by.Study[,1])
edu_m_.0 <- ds.table('excluded_subset$edu_m_.0')
edu_m_.0 <- edu_m_.0$output.list[3]$TABLE_rvar.by.study_counts
preg_dep <- ds.table('excluded_subset$preg_dep')
preg_dep <- preg_dep$output.list[3]$TABLE_rvar.by.study_counts
ppd <- ds.table('excluded_subset$ppd')
ppd <- ppd$output.list[3]$TABLE_rvar.by.study_counts
prepreg_dep <- ds.table('excluded_subset$prepreg_dep', datasources = connections[c('genr','dnbc','ninfea')])
prepreg_dep <- prepreg_dep$output.list[3]$TABLE_rvar.by.study_counts
cumul_dep_weighted <- ds.table('excluded_subset$cumul_dep_weighted', datasources = connections[c('genr','dnbc','ninfea')])
cumul_dep_weighted <- cumul_dep_weighted$output.list[3]$TABLE_rvar.by.study_counts
cob_other_country_f <- ds.table('excluded_subset$cob_other_country_f', datasources = connections[c('abcd','genr','eden','ninfea')])
cob_other_country_f <- cob_other_country_f$output.list[3]$TABLE_rvar.by.study_counts
preg_alc <- ds.table('excluded_subset$preg_alc')
preg_alc <- preg_alc$output.list[3]$TABLE_rvar.by.study_counts
preg_smk <- ds.table('excluded_subset$preg_smk')
preg_smk <- preg_smk$output.list[3]$TABLE_rvar.by.study_counts
prepreg_BMI_mean <- ds.mean('excluded_subset$prepreg_BMI')
prepreg_BMI_mean <- prepreg_BMI_mean$Mean.by.Study[,1]
prepreg_BMI_var <- ds.var('excluded_subset$prepreg_BMI')
prepreg_BMI_sd <- sqrt(prepreg_BMI_var$Variance.by.Study[,1])

descriptives <- list(
  study_sample_N = samplesize_EX,
  child_sex = child_sex,
  agebirth_m_y = list(mean = agebirth_m_y_mean, SD = agebirth_m_y_sd),
  edu_m_.0 = edu_m_.0,
  prepreg_BMI = list(mean = prepreg_BMI_mean, SD = prepreg_BMI_sd),
  preg_dep = preg_dep,
  ppd = ppd,
  prepreg_dep = prepreg_dep,
  cumul_dep_weighted = cumul_dep_weighted,
  cob_other_country_f = cob_other_country_f,
  preg_alc = preg_alc,
  preg_smk = preg_smk
)
saveRDS(descriptives, file="results/00_descriptives/descriptives_ECCN_excluded.RData")
#----------------------------3a) Plot distributions------------------------------
if (plot == 'yes')
{
  jpeg(file="/home/jovyan/plots/outcome_desc/int_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$int_pc_',
               type = "split", num.breaks = 50,
               datasources = connections)
  grid.text("Internalising symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/ext_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$ext_pc_',
               type = "split", num.breaks = 50,
               datasources = connections)
  grid.text("Externalising symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/adhd_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$adhd_pc_',
               type = "split", num.breaks = 50,
               datasources = connections)
  grid.text("ADHD symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/asd_hist.jpeg",
       width=500, height=400, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$asd_pc_',
               type = "split", num.breaks = 50,
               datasources = connections[c('genr', 'eden')])
  grid.text("ASD symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/fm_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$fm_pc_',
               type = "split", num.breaks = 50,
               datasources = connections[c('genr',  'dnbc', 'eden', 'ninfea')])
  grid.text("Fine motor skills in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/gm_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$gm_pc_',
               type = "split", num.breaks = 50,
               datasources = connections[c('genr',  'dnbc', 'eden', 'ninfea')])
  grid.text("Gross motor skills in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/lan_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$lan_pc_',
               type = "split", num.breaks = 50,
               datasources = connections[c('genr',  'eden', 'ninfea')])
  grid.text("Language skills in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/nvi_hist.jpeg",
       width=500, height=700, pointsize = 20, quality = 100)
  ds.histogram(x = 'df_complete_l$nvi_pc_',
               type = "split", num.breaks = 50,
               datasources = connections[c('abcd', 'eden', 'genr')])
  grid.text("Non-verbal intelligence in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/int_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$int_age_",
                 y = "df_complete_l$int_pc_",
                 datasources = connections)
  grid.text("Internalising symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/ext_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$ext_age_",
                 y = "df_complete_l$ext_pc_",
                 datasources = connections)
  grid.text("Externalising symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/adhd_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$adhd_age_",
                 y = "df_complete_l$adhd_pc_",
                 datasources = connections)
  grid.text("ADHD symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/asd_scat.jpeg",
       width=600, height=400, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$asd_age_",
                 y = "df_complete_l$asd_pc_",
                 datasources = connections[c('genr', 'eden')])
  grid.text("ASD symptoms in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/fm_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$fm_age_",
                 y = "df_complete_l$fm_pc_",
                 datasources = connections[c('genr',  'dnbc', 'eden', 'ninfea')])
  grid.text("Fine motor skills in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/gm_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$gm_age_",
                 y = "df_complete_l$gm_pc_",
                 datasources = connections[c('genr',  'dnbc', 'eden', 'ninfea')])
  grid.text("Gross motor skills in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/lan_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$lan_age_",
                 y = "df_complete_l$lan_pc_",
                 datasources = connections[c('genr',  'eden', 'ninfea')])
  grid.text("Language skills in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
  
  jpeg(file="/home/jovyan/plots/outcome_desc/nvi_scat.jpeg",
       width=600, height=800, pointsize = 20, quality = 100)
  ds.scatterPlot(x = "df_complete_l$nvi_age_",
                 y = "df_complete_l$nvi_pc_",
                 datasources = connections[c('abcd', 'eden', 'genr')])
  grid.text("Non-verbal intelligence in complete dataset", .5, .98, gp=gpar(cex=1))
  dev.off()
}
#---------------------------3b) Check # of follow ups----------------------------
ds.tapply.assign(X.name = "df_basic$int_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections)
int_table <- ds.table(rvar = 'vector1$N', datasources = connections)
num_int <- int_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$ext_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections)
ext_table <- ds.table(rvar = 'vector1$N', datasources = connections)
num_ext <- ext_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$adhd_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections)
adhd_table <- ds.table(rvar = 'vector1$N', datasources = connections)
num_adhd <- adhd_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$asd_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'eden')])
asd_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'eden')])
num_asd <- asd_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$fm_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
fm_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
num_fm <- fm_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$gm_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
gm_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
num_gm <- gm_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$lan_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'eden', 'ninfea')])
lan_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'eden', 'ninfea')])
num_lan <- lan_table$output.list$TABLE_rvar.by.study_counts

ds.tapply.assign(X.name = "df_basic$nvi_pc_", INDEX.names = 'df_basic$child_id', FUN.name = 'N',
                 newobj = 'vector1', datasources = connections[c('genr', 'eden','abcd')])
nvi_table <- ds.table(rvar = 'vector1$N', datasources = connections[c('genr', 'eden', 'abcd')])
num_nvi <- nvi_table$output.list$TABLE_rvar.by.study_counts

# Number of follow-ups per cohort per outcome:
num_followup <- list(num_int=num_int,num_ext=num_ext,num_adhd=num_adhd,num_asd=num_asd,
                num_fm=num_fm,num_gm=num_gm,num_lan=num_lan,num_nvi=num_nvi)
saveRDS(num_followup, file="results/00_descriptives/num_followup_ECCN.RData")

################################### ANALYSIS ###################################
#------------------------4a) Add prepreg_BMI variable---------------------------
# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "df_basic$prepreg_weight/((df_basic$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections)

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections)


# bind to df
ds.cbind(
  x = c("df_basic","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "df_basic",
  datasources = connections
)

#-----------------------------4b) Add COB variable------------------------------
# Make a dummy variable for cob_m
# cob_m = Born in country of cohort = 0, Born in EU country (outside cohort country) = 1, Born in other country = 2

ds.Boole(V1 = "df_basic$cob_m",
         V2 = 0,
         Boolean.operator = "==",
         numeric.output = TRUE, 
         na.assign = "NA", 
         newobj = "cob_same_as_cohort",
         datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
ds.asFactor(input.var.name = 'cob_same_as_cohort', newobj.name = 'cob_other_country_f', 
            datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
#cob_other_country_f = Born in country of cohort = 1, Born in another country = 0

# check 
ds.table(rvar = 'df_basic$cob_m', datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])
ds.table(rvar = 'cob_other_country_f', datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')])

# bind to df
ds.cbind(
  x = c("df_basic","cob_other_country_f"),
  DataSHIELD.checks = FALSE,
  newobj = "df_basic",
  datasources = connections[c('genr', 'eden', 'ninfea', 'abcd')]
)

#---------------------------4c) Cumulative exposure-----------------------------

# convert to numeric to be able to add them up
ds.asNumeric(x.name = "core_nonrep$prepreg_dep",
             newobj = "num_prepreg_dep",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.asNumeric(x.name = "core_nonrep$preg_dep",
             newobj = "num_preg_dep",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.asNumeric(x.name = "core_nonrep$ppd",
             newobj = "num_ppd",
             datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.replaceNA(x = "num_prepreg_dep", forNA = c(10,10,10), newobj = "num_prepreg_dep_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.replaceNA(x = "num_preg_dep", forNA = c(10,10,10), newobj = "num_preg_dep_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.replaceNA(x = "num_ppd", forNA = c(10,10,10), newobj = "num_ppd_10",
             datasources = connections[c('genr', 'dnbc', 'ninfea')]) # 10 per cohort, can be any value

ds.make(toAssign = "num_prepreg_dep_10 + num_preg_dep_10 + num_ppd_10",
        newobj = "cumul_dep", 
        datasources = connections[c('genr', 'dnbc', 'ninfea')])

# check
ds.table(rvar = 'cumul_dep', datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep", values2replace.vector = c(0,1,2,3), 
                new.values.vector = c(0,1,2,3), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep_weighted", values2replace.vector = c(10,11,12), 
                new.values.vector = c(0,1.5,3), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.recodeValues(var.name = "cumul_dep_weighted", values2replace.vector = c(20,21,30), 
                new.values.vector = c(NA,NA,NA), newobj = "cumul_dep_weighted",
                datasources = connections[c('genr', 'dnbc', 'ninfea')])

ds.table(rvar = 'cumul_dep_weighted', datasources = connections[c('genr', 'dnbc', 'ninfea')])

# bind to df
ds.cbind(
  x = c("df_basic","cumul_dep_weighted"),
  DataSHIELD.checks = FALSE,
  newobj = "df_basic",
  datasources = connections[c('genr', 'dnbc', 'ninfea')]
)


#-----------------------------5) Minimal models---------------------------------

# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# only correcting for age at outcome and sex.

# Function definition
run_minimal_model <- function(outcome, age_var, 
                              lmm_cohorts = NULL, glm_cohorts = NULL, 
                              lmm_extra = NULL,
                              output_file,
                              data_lmm = "df_complete_l",
                              data_glm = "df_complete_l") {
  
  # Helper to extract and format results
  extract_results <- function(output, cohort_names) {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    betas <- data.frame(beta = betas[, "preg_dep1"], row.names = cohort_names)
    
    ses <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    ses <- data.frame(ses = ses[, "preg_dep1"], row.names = cohort_names)
    
    df <- cbind(betas, ses)
    colnames(df) <- c("preg_dep_betas", "ses")  
    return(df)
  }
  
  # Collect results
  res_parts <- list()
  
  # --- LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(outcome, " ~ preg_dep + sex + ", age_var, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    res_parts <- append(res_parts, list(extract_results(model_lmm$output.summary, lmm_cohorts)))
  }
  
  # --- Separate LMM (if non-convergence occurs this can be run on separate cohorts)
  if (!is.null(lmm_extra)) {
    for (cohort_name in names(lmm_extra)) {
      model_extra <- ds.lmerSLMA(
        dataName = data_lmm,
        formula = paste0(outcome, " ~ preg_dep + sex + ", age_var, " + (1|child_id)"),
        REML = TRUE,
        datasources = connections[lmm_extra[[cohort_name]]]
      )
      res_parts <- append(res_parts, list(extract_results(model_extra$output.summary, lmm_extra[[cohort_name]])))
    }
  }
  
  # --- GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = paste0(outcome, " ~ preg_dep + sex + ", age_var),
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    res_parts <- append(res_parts, list(extract_results(model_glm$output.summary, glm_cohorts)))
  }
  
  # Combine without inherited prefixes
  results_total <- do.call(rbind, res_parts)
  rownames(results_total) <- sub(".*\\.", "", rownames(results_total))  # remove "lmm." etc.
  
  # Save
  write.csv(results_total, output_file)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

# List of what to run
models_to_run <- list(
  list(outcome = "int_pc_",  age_var = "int_age_",  
       lmm = c('genr','dnbc','eden','abcd'), glm = c('ninfea'),
       file = "results/sensitivity/minimal/results_int_minimal_model_ECCN.csv"),
  
  list(outcome = "ext_pc_",  age_var = "ext_age_",  
       lmm = c('genr','eden','abcd'),
       lmm_extra = list(dnbc = c('dnbc')),   # <- DNBC separate
       glm = c('ninfea'),
       file = "results/sensitivity/minimal/results_ext_minimal_model_ECCN.csv"),
  
  list(outcome = "adhd_pc_", age_var = "adhd_age_", 
       lmm = c('dnbc','eden','ninfea','abcd'), 
       glm = c('genr'),
       file = "results/sensitivity/minimal/results_adhd_minimal_model_ECCN.csv"),
  
  list(outcome = "asd_pc_",  age_var = "asd_age_",  
       glm = c('genr'),
       file = "results/sensitivity/minimal/results_asd_minimal_model_ECCN.csv"),
  
  list(outcome = "fm_pc_",   age_var = "fm_age_",   
       lmm = c('genr','eden'), glm = c('dnbc','ninfea'),
       file = "results/sensitivity/minimal/results_fm_minimal_model_ECCN.csv"),
  
  list(outcome = "gm_pc_",   age_var = "gm_age_",   
       lmm = c('genr','eden','ninfea'), glm = c('dnbc'),
       file = "results/sensitivity/minimal/results_gm_minimal_model_ECCN.csv"),
  
  list(outcome = "lan_pc_",  age_var = "lan_age_",  
       lmm = c('eden'), 
       glm = c('genr','ninfea'),
       file = "results/sensitivity/minimal/results_lan_minimal_model_ECCN.csv"),
  
  list(outcome = "nvi_pc_",  age_var = "nvi_age_",  
       lmm = c('eden'), glm = c('genr','abcd'),
       file = "results/sensitivity/minimal/results_nvi_minimal_model_ECCN.csv")
)

results_list <- lapply(models_to_run, function(m) {
  run_minimal_model(outcome = m$outcome,
                    age_var = m$age_var,
                    lmm_cohorts = m$lmm,
                    glm_cohorts = m$glm,
                    lmm_extra = m$lmm_extra,
                    output_file = m$file)
})

#---------------------------6) Fully adjusted models----------------------------

# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for additional covariates income, maternal age at birth, 
# prepregnancy BMI, maternal alcohol use and smoking during pregnancy

# Function definition
run_fully_adjusted_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs for preg_dep
  extract_results <- function(output, cohort_names) {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!"preg_dep1" %in% colnames(betas)) {
      stop("Column 'preg_dep1' not found in beta matrix — check model output.")
    }
    
    df <- data.frame(
      cohort = cohort_names,
      preg_dep_betas = betas[, "preg_dep1"],
      ses = ses[, "preg_dep1"],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Helper functions to get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      output[[paste0("study", i)]]$Nvalid
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Build base formula
  base_formula <- paste0(
    outcome, " ~ preg_dep + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI"
  )
  
  results_all <- list()
  
  # LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(base_formula, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    output_lmm <- model_lmm$output.summary
    
    df_lmm <- extract_results(output_lmm, lmm_cohorts)
    Ns_lmm <- get_N_counts_lmm(output_lmm, lmm_cohorts)
    
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = base_formula,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    output_glm <- model_glm$output.summary
    
    df_glm <- extract_results(output_glm, glm_cohorts)
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine cleanly
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  
  # Save results
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}


# List of what to run
models_fully_adjusted <- list(
  list(outcome = "int_pc_",  age_var = "int_age_",  lmm = c('genr','dnbc','eden','abcd'), glm = c('ninfea'),
       file = "results/01_RQ/results_int_main_model_ECCN.csv"),
  list(outcome = "ext_pc_",  age_var = "ext_age_",  lmm = c('genr','dnbc','eden','abcd'), glm = c('ninfea'),
       file = "results/01_RQ/results_ext_main_model_ECCN.csv"),
  list(outcome = "adhd_pc_", age_var = "adhd_age_", lmm = c('dnbc','eden','ninfea','abcd'), glm = c('genr'),
       file = "results/01_RQ/results_adhd_main_model_ECCN.csv"),
  list(outcome = "asd_pc_",  age_var = "asd_age_",  glm = c('genr'),
       file = "results/01_RQ/results_asd_main_model_ECCN.csv"),
  list(outcome = "fm_pc_",   age_var = "fm_age_",   lmm = c('genr','eden'), glm = c('dnbc','ninfea'),
       file = "results/01_RQ/results_fm_main_model_ECCN.csv"),
  list(outcome = "gm_pc_",   age_var = "gm_age_",   lmm = c('genr','eden','ninfea'), glm = c('dnbc'),
       file = "results/01_RQ/results_gm_main_model_ECCN.csv"),
  list(outcome = "lan_pc_",  age_var = "lan_age_",  lmm = c('eden'), glm = c('genr','ninfea'),
       file = "results/01_RQ/results_lan_main_model_ECCN.csv"),
  list(outcome = "nvi_pc_",  age_var = "nvi_age_",  lmm = c('eden'), glm = c('genr','abcd'),
       file = "results/01_RQ/results_nvi_main_model_ECCN.csv")
)

results_full_adj <- lapply(models_fully_adjusted, function(m) {
  run_fully_adjusted_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    output_file = m$file
  )
})

#-------------------------7a) Sex interaction datasets---------------------------
ds.dataFrameSubset(
  df.name = "df_basic",
  V1.name = "df_basic$sex",
  V2.name = "1",       
  Boolean.operator = "==",
  newobj = "df_male"
)

ds.dataFrameSubset(
  df.name = "df_basic",
  V1.name = "df_basic$sex",
  V2.name = "2",       
  Boolean.operator = "==",
  newobj = "df_female"
)
#-------------------------7b) Sex interaction models----------------------------
# Sex interaction for internalising symptoms
# sex 1 'Male' 2 'Female'.

run_sexint_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    data_lmm = "df_basic",
    data_glm = "df_basic",
    interaction = TRUE,
    sex_subset = NULL, # "male", "female", or NULL
    output_file
) {
  # Build formula
  if (interaction) {
    formula_core <- paste0(outcome, " ~ preg_dep*sex + ", age_var,
                           " + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
  } else {
    formula_core <- paste0(outcome, " ~ preg_dep + ", age_var,
                           " + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI")
  }
  
  if (!is.null(sex_subset)) {
    data_lmm <- paste0("df_", sex_subset)
    data_glm <- paste0("df_", sex_subset)
  }
  
  # Helper to extract Ns from LMM and GLM
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) output[[paste0("study", i)]]$Nvalid)
    data.frame(cohort = cohorts, N = Ns)
  }
  
  # Run LMM
  results_all <- list()
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(formula_core, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    out_lmm <- model_lmm$output.summary
    
    betas <- t(as.matrix(out_lmm[["input.beta.matrix.for.SLMA"]]))
    ses <- t(as.matrix(out_lmm[["input.se.matrix.for.SLMA"]]))
    
    df_lmm <- as.data.frame(betas)
    ses_lmm <- as.data.frame(ses)
    
    # Select relevant columns
    keep <- c("preg_dep1", "preg_dep1:sex2")[c("preg_dep1", "preg_dep1:sex2") %in% colnames(df_lmm)]
    df_lmm <- df_lmm[, keep, drop = FALSE]
    ses_lmm <- ses_lmm[, keep, drop = FALSE]
    
    # Rename
    names(df_lmm) <- gsub("preg_dep1(:sex2)?", 
                          if (interaction) c("preg_dep_betas","preg_dep_sex_betas") else "preg_dep_betas",
                          names(df_lmm))
    names(ses_lmm) <- gsub("preg_dep1(:sex2)?", 
                           if (interaction) c("preg_dep_ses","preg_dep_sex_int_ses") else "preg_dep_ses",
                           names(ses_lmm))
    
    df_lmm <- cbind(cohort = lmm_cohorts, df_lmm, ses_lmm)
    Ns_lmm <- get_N_counts_lmm(out_lmm, lmm_cohorts)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort")
    results_all <- append(results_all, list(df_lmm))
  }
  
  # Run GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = formula_core,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    out_glm <- model_glm$output.summary
    
    betas <- t(as.matrix(out_glm[["input.beta.matrix.for.SLMA"]]))
    ses <- t(as.matrix(out_glm[["input.se.matrix.for.SLMA"]]))
    
    df_glm <- as.data.frame(betas)
    ses_glm <- as.data.frame(ses)
    
    keep <- c("preg_dep1", "preg_dep1:sex2")[c("preg_dep1", "preg_dep1:sex2") %in% colnames(df_glm)]
    df_glm <- df_glm[, keep, drop = FALSE]
    ses_glm <- ses_glm[, keep, drop = FALSE]
    
    names(df_glm) <- gsub("preg_dep1(:sex2)?", 
                          if (interaction) c("preg_dep_betas","preg_dep_sex_betas") else "preg_dep_betas",
                          names(df_glm))
    names(ses_glm) <- gsub("preg_dep1(:sex2)?", 
                           if (interaction) c("preg_dep_ses","preg_dep_sex_int_ses") else "preg_dep_ses",
                           names(ses_glm))
    
    df_glm <- cbind(cohort = glm_cohorts, df_glm, ses_glm)
    Ns_glm <- get_N_counts_glm(out_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort")
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine and save
  results_total <- do.call(rbind, results_all)
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

models_sexint_all <- list(
  # Internalising symptoms
  list(outcome = "int_pc_",  age_var = "int_age_",  
       lmm = c('genr','dnbc','eden','abcd'), 
       glm = c('ninfea')),
  
  # Externalising
  list(outcome = "ext_pc_",  age_var = "ext_age_",  
       lmm = c('genr','dnbc','eden','abcd'), 
       glm = c('ninfea')),
  
  # ADHD
  list(outcome = "adhd_pc_", age_var = "adhd_age_", 
       lmm = c('dnbc','eden','ninfea','abcd'), 
       glm = c('genr')),
  
  # ASD
  list(outcome = "asd_pc_",  age_var = "asd_age_",  
       glm = c('genr')),  # only GLM here
  
  # Fine motor
  list(outcome = "fm_pc_",   age_var = "fm_age_",   
       lmm = c('genr','eden'), glm = c('dnbc','ninfea')),
  
  # Gross motor
  list(outcome = "gm_pc_",   age_var = "gm_age_",   
       lmm = c('genr','eden','ninfea'), glm = c('dnbc')),
  
  # Language
  list(outcome = "lan_pc_",  age_var = "lan_age_",  
       lmm = c('eden'), glm = c('genr','ninfea')),
  
  # Nonverbal IQ
  list(outcome = "nvi_pc_",  age_var = "nvi_age_",  
       lmm = c('eden'), glm = c('genr','abcd'))
)

# Helper function to build file paths
make_file <- function(prefix, outcome, suffix) {
  clean_name <- gsub("_pc_?", "", outcome)
  paste0("results/02_RQ/results_", clean_name, "_", prefix, "_model_ECCN", suffix, ".csv")
}

# Expand all models for each outcome
models_sexint_expanded <- lapply(models_sexint_all, function(m) {
  list(
    # Interaction
    list(outcome = m$outcome, age_var = m$age_var,
         lmm = m$lmm, glm = m$glm,
         interaction = TRUE,
         sex_subset = NULL,
         file = make_file("sexint", sub("_$", "", m$outcome), "")),
    
    # Male-only
    list(outcome = m$outcome, age_var = m$age_var,
         lmm = m$lmm, glm = m$glm,
         interaction = FALSE,
         sex_subset = "male",
         file = make_file("sexint", sub("_$", "", m$outcome), "_male")),
    
    # Female-only
    list(outcome = m$outcome, age_var = m$age_var,
         lmm = m$lmm, glm = m$glm,
         interaction = FALSE,
         sex_subset = "female",
         file = make_file("sexint", sub("_$", "", m$outcome), "_female"))
  )
}) |> unlist(recursive = FALSE)

results_sexint_all <- lapply(models_sexint_expanded, function(m) {
  run_sexint_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    data_lmm = ifelse(is.null(m$sex_subset), "df_basic", paste0("df_", m$sex_subset)),
    data_glm = ifelse(is.null(m$sex_subset), "df_basic", paste0("df_", m$sex_subset)),
    interaction = m$interaction,
    sex_subset = m$sex_subset,
    output_file = m$file
  )
})

#---------------------------8) COB adjusted models------------------------------

#-I) Fit linear mixed model with preg_dep as exposure and internalising symptoms 
# as outcomes, correcting for covariates income, maternal age at birth, prepregnancy BMI,
# maternal alcohol use and smoking during pregnancy and country of birth of the mother
# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for covariates income, maternal age at birth, prepregnancy BMI, 
# maternal alcohol use, smoking during pregnancy, and in addition country of birth mother

run_COB_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs for preg_dep
  extract_results <- function(output, cohort_names) {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!"preg_dep1" %in% colnames(betas)) {
      stop("Column 'preg_dep1' not found in beta matrix — check model output.")
    }
    
    df <- data.frame(
      cohort = cohort_names,
      preg_dep_betas = betas[, "preg_dep1"],
      preg_dep_ses   = ses[, "preg_dep1"],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Helper functions to get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      output[[paste0("study", i)]]$Nvalid
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Build formula including the COB term
  base_formula <- paste0(
    outcome, " ~ preg_dep + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI + cob_other_country_f"
  )
  
  results_all <- list()
  
  # LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(base_formula, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    output_lmm <- model_lmm$output.summary
    
    df_lmm <- extract_results(output_lmm, lmm_cohorts)
    Ns_lmm <- get_N_counts_lmm(output_lmm, lmm_cohorts)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = base_formula,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    output_glm <- model_glm$output.summary
    
    df_glm <- extract_results(output_glm, glm_cohorts)
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine + Save 
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

models_COB <- list(
  list(outcome = "int_pc_",  age_var = "int_age_",  
       lmm = c('genr','eden','abcd'), glm = c('ninfea'),
       file = "results/sensitivity/COB/results_int_COB_model_ECCN.csv"),
  
  list(outcome = "ext_pc_",  age_var = "ext_age_",  
       lmm = c('genr','eden','abcd'), glm = c('ninfea'),
       file = "results/sensitivity/COB/results_ext_COB_model_ECCN.csv"),
  
  list(outcome = "adhd_pc_", age_var = "adhd_age_", 
       lmm = c('eden','ninfea','abcd'), glm = c('genr'),
       file = "results/sensitivity/COB/results_adhd_COB_model_ECCN.csv"),
  
  list(outcome = "asd_pc_",  age_var = "asd_age_",  
       glm = c('genr'),
       file = "results/sensitivity/COB/results_asd_COB_model_ECCN.csv"),
  
  list(outcome = "fm_pc_",   age_var = "fm_age_",   
       lmm = c('genr','eden'), glm = c('ninfea'),
       file = "results/sensitivity/COB/results_fm_COB_model_ECCN.csv"),
  
  list(outcome = "gm_pc_",   age_var = "gm_age_",   
       lmm = c('genr','eden','ninfea'),
       file = "results/sensitivity/COB/results_gm_COB_model_ECCN.csv"),
  
  list(outcome = "lan_pc_",  age_var = "lan_age_",  
       lmm = c('eden'), glm = c('genr','ninfea'),
       file = "results/sensitivity/COB/results_lan_COB_model_ECCN.csv"),
  
  list(outcome = "nvi_pc_",  age_var = "nvi_age_",  
       lmm = c('eden'), glm = c('genr','abcd'),
       file = "results/sensitivity/COB/results_nvi_COB_model_ECCN.csv")
)

results_COB <- lapply(models_COB, function(m) {
  run_COB_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    output_file = m$file
  )
})

#--------------------------9) Individual associations---------------------------
#-------------------------9a) Pre-pregnancy depression--------------------------

#-I) Fit linear mixed model with prepreg_dep as exposure and internalising symptoms 
# as outcomes, correcting for covariates income, maternal age at birth, prepregnancy BMI,
# maternal alcohol use and smoking during pregnancy and country of birth of the mother
# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for covariates income, maternal age at birth, and prepregnancy BMI.

run_pre_preg_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs for prepreg_dep
  extract_results <- function(output, cohort_names, var = "prepreg_dep1") {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!var %in% colnames(betas)) {
      stop(paste0("Column '", var, "' not found in beta matrix — check model output."))
    }
    
    df <- data.frame(
      cohort = cohort_names,
      preg_dep_betas = betas[, var],
      ses = ses[, var],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Helper functions to get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) output[[paste0("study", i)]]$Nvalid)
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Build formula
  base_formula <- paste0(
    outcome, " ~ prepreg_dep + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + prepreg_BMI"
  )
  
  results_all <- list()
  
  # LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(base_formula, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    
    output_lmm <- model_lmm$output.summary
    df_lmm <- extract_results(output_lmm, lmm_cohorts, var = "prepreg_dep1")
    Ns_lmm <- get_N_counts_lmm(output_lmm, lmm_cohorts)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = base_formula,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    
    output_glm <- model_glm$output.summary
    df_glm <- extract_results(output_glm, glm_cohorts, var = "prepreg_dep1")
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine and export
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

models_pre_preg <- list(
  list(outcome = "int_pc_", age_var = "int_age_",  lmm = c('genr','dnbc'), glm = c('ninfea'),
       file = "results/sensitivity/individual_timing/results_int_pre_indivi_ECCN.csv"),
  list(outcome = "ext_pc_", age_var = "ext_age_",  lmm = c('genr','dnbc'), glm = c('ninfea'),
       file = "results/sensitivity/individual_timing/results_ext_pre_indivi_ECCN.csv"),
  list(outcome = "adhd_pc_", age_var = "adhd_age_",  lmm = c('dnbc', 'ninfea'), glm = c('genr'),
       file = "results/sensitivity/individual_timing/results_adhd_pre_indivi_ECCN.csv"),
  list(outcome = "asd_pc_", age_var = "asd_age_",  glm = c('genr'),
       file = "results/sensitivity/individual_timing/results_asd_pre_indivi_ECCN.csv"),
  list(outcome = "fm_pc_", age_var = "fm_age_",  lmm = c('genr'), glm = c('dnbc', 'ninfea'),
       file = "results/sensitivity/individual_timing/results_fm_pre_indivi_ECCN.csv"),
  list(outcome = "gm_pc_", age_var = "gm_age_",  lmm = c('genr', 'ninfea'), glm = c('dnbc'),
       file = "results/sensitivity/individual_timing/results_gm_pre_indivi_ECCN.csv"),
  list(outcome = "lan_pc_", age_var = "lan_age_",  glm = c('genr', 'ninfea'),
       file = "results/sensitivity/individual_timing/results_lan_pre_indivi_ECCN.csv"),
  list(outcome = "nvi_pc_", age_var = "nvi_age_",  glm = c('genr'),
       file = "results/sensitivity/individual_timing/results_nvi_pre_indivi_ECCN.csv")
)

results_pre_preg <- lapply(models_pre_preg, function(m) {
  run_pre_preg_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    output_file = m$file
  )
})
#---------------------------9b) Postnatal depression----------------------------
#-I) Fit linear mixed model with ppd as exposure and internalising symptoms 
# as outcomes, correcting for covariates income, maternal age at birth, prepregnancy BMI,
# maternal alcohol use and smoking during pregnancy and country of birth of the mother
# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for covariates income, maternal age at birth, prepregnancy BMI, 
# maternal alcohol use, and smoking during pregnancy.

run_ppd_exact <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    separate_lmm = NULL,   # <-- added back
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs
  extract_results <- function(output, cohorts, var = "ppd") {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!var %in% colnames(betas)) var <- "ppd1"
    
    df <- data.frame(
      cohort = cohorts,
      preg_dep_betas = betas[, var],
      ses = ses[, var],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) output[[paste0("study", i)]]$Nvalid)
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Formulas
  formula_lmm <- paste0(
    outcome, " ~ ppd + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI + (1|child_id)"
  )
  formula_glm <- paste0(
    outcome, " ~ ppd + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI"
  )
  
  results_all <- list()
  
  # LMM main
  main_lmm <- setdiff(lmm_cohorts, separate_lmm)
  if (!is.null(main_lmm) && length(main_lmm) > 0) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = formula_lmm,
      REML = TRUE,
      datasources = connections[main_lmm]
    )
    output_lmm <- model_lmm$output.summary
    df_lmm <- extract_results(output_lmm, main_lmm)
    Ns_lmm <- get_N_counts_lmm(output_lmm, main_lmm)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # LMM separate (non-convergence)
  if (!is.null(separate_lmm)) {
    for (cohort in separate_lmm) {
      model_sep <- ds.lmerSLMA(
        dataName = data_lmm,
        formula = formula_lmm,
        REML = TRUE,
        datasources = connections[cohort]
      )
      output_sep <- model_sep$output.summary
      df_sep <- extract_results(output_sep, cohort)
      Ns_sep <- get_N_counts_lmm(output_sep, cohort)
      df_sep <- merge(df_sep, Ns_sep, by = "cohort", all.x = TRUE)
      results_all <- append(results_all, list(df_sep))
    }
  }
  
  # GLM
  if (!is.null(glm_cohorts) && length(glm_cohorts) > 0) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = formula_glm,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    output_glm <- model_glm$output.summary
    df_glm <- extract_results(output_glm, glm_cohorts)
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine all
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  
  write.csv(results_total, output_file, row.names = FALSE)
  return(results_total)
}

run_ppd_exact(
  outcome = "int_pc_",
  age_var = "int_age_",
  lmm_cohorts = c("genr", "dnbc", "eden"),
  glm_cohorts = c("ninfea"),
  separate_lmm = c("abcd"),   # ABCD separate for convergence
  output_file = "results/sensitivity/individual_timing/results_int_ppd_indivi_ECCN.csv"
)
run_ppd_exact(
  outcome = "ext_pc_",
  age_var = "ext_age_",
  lmm_cohorts = c("genr", "dnbc", "abcd"),
  glm_cohorts = c("ninfea"),
  separate_lmm = c("eden"),   # EDEN separate for convergence
  output_file = "results/sensitivity/individual_timing/results_ext_ppd_indivi_ECCN.csv"
)
run_ppd_exact(
  outcome = "adhd_pc_",
  age_var = "adhd_age_",
  lmm_cohorts = c('dnbc', 'eden', 'ninfea', 'abcd'),
  glm_cohorts = c('genr'),
  output_file = "results/sensitivity/individual_timing/results_adhd_ppd_indivi_ECCN.csv"
)
run_ppd_exact(
  outcome = "asd_pc_",
  age_var = "asd_age_",
  lmm_cohorts = c(),
  glm_cohorts = c('genr'),
  output_file = "results/sensitivity/individual_timing/results_asd_ppd_indivi_ECCN.csv"
) 
run_ppd_exact(
  outcome = "fm_pc_",
  age_var = "fm_age_",
  lmm_cohorts = c('genr', 'eden'),
  glm_cohorts = c('dnbc', 'ninfea'),
  output_file = "results/sensitivity/individual_timing/results_fm_ppd_indivi_ECCN.csv"
) 
run_ppd_exact(
  outcome = "gm_pc_",
  age_var = "gm_age_",
  lmm_cohorts = c('genr', 'eden', 'ninfea'),
  glm_cohorts = c('dnbc'),
  output_file = "results/sensitivity/individual_timing/results_gm_ppd_indivi_ECCN.csv"
) 
run_ppd_exact(
  outcome = "lan_pc_",
  age_var = "lan_age_",
  lmm_cohorts = c('eden'),
  glm_cohorts = c('genr', 'ninfea'),
  output_file = "results/sensitivity/individual_timing/results_lan_ppd_indivi_ECCN.csv"
) 
run_ppd_exact(
  outcome = "nvi_pc_",
  age_var = "nvi_age_",
  lmm_cohorts = c('eden'),
  glm_cohorts = c('genr', 'abcd'),
  output_file = "results/sensitivity/individual_timing/results_nvi_ppd_indivi_ECCN.csv"
) 

#------------------10) Adjusting for pre-pregnancy depression-------------------
#-I) Fit linear mixed model with prepreg_dep as exposure and internalising symptoms 
# as outcomes, correcting for covariates income, maternal age at birth, prepregnancy BMI,
# maternal alcohol use and smoking during pregnancy and country of birth of the mother
# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for covariates income, maternal age at birth, prepregnancy BMI, 
# maternal alcohol use, smoking during pregnancy, and pre-pregnancy depression.

run_pre_preg_adj_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs for preg_dep
  extract_results <- function(output, cohort_names, var = "preg_dep1") {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!var %in% colnames(betas)) {
      stop(paste0("Column '", var, "' not found in beta matrix — check model output."))
    }
    
    df <- data.frame(
      cohort = cohort_names,
      preg_dep_betas = betas[, var],
      ses = ses[, var],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Helper functions to get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) output[[paste0("study", i)]]$Nvalid)
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Build formula
  base_formula <- paste0(
    outcome, " ~ preg_dep + sex + ", age_var,
    " + prepreg_dep + edu_m_.0 + agebirth_m_y + prepreg_BMI + preg_alc + preg_smk"
  )
  
  results_all <- list()
  
  # LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(base_formula, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    
    output_lmm <- model_lmm$output.summary
    df_lmm <- extract_results(output_lmm, lmm_cohorts, var = "preg_dep1")
    Ns_lmm <- get_N_counts_lmm(output_lmm, lmm_cohorts)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = base_formula,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    
    output_glm <- model_glm$output.summary
    df_glm <- extract_results(output_glm, glm_cohorts, var = "preg_dep1")
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine and export
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

models_pre_preg_adj <- list(
  list(outcome = "int_pc_", age_var = "int_age_",  lmm = c('genr','dnbc'), glm = c('ninfea'),
       file = "results/03_RQ/prepreg_adjusted/results_int_preadj_model_ECCN.csv"),
  list(outcome = "ext_pc_", age_var = "ext_age_",  lmm = c('genr','dnbc'), glm = c('ninfea'),
       file = "results/03_RQ/prepreg_adjusted/results_ext_preadj_model_ECCN.csv"),
  list(outcome = "adhd_pc_", age_var = "adhd_age_",  lmm = c('dnbc', 'ninfea'), glm = c('genr'),
       file = "results/03_RQ/prepreg_adjusted/results_adhd_preadj_model_ECCN.csv"),
  list(outcome = "asd_pc_", age_var = "asd_age_",  glm = c('genr'),
       file = "results/03_RQ/prepreg_adjusted/results_asd_preadj_model_ECCN.csv"),
  list(outcome = "fm_pc_", age_var = "fm_age_",  lmm = c('genr'), glm = c('dnbc', 'ninfea'),
       file = "results/03_RQ/prepreg_adjusted/results_fm_preadj_model_ECCN.csv"),
  list(outcome = "gm_pc_", age_var = "gm_age_",  lmm = c('genr', 'ninfea'), glm = c('dnbc'),
       file = "results/03_RQ/prepreg_adjusted/results_gm_preadj_model_ECCN.csv"),
  list(outcome = "lan_pc_", age_var = "lan_age_",  glm = c('genr', 'ninfea'),
       file = "results/03_RQ/prepreg_adjusted/results_lan_preadj_model_ECCN.csv"),
  list(outcome = "nvi_pc_", age_var = "nvi_age_",  glm = c('genr'),
       file = "results/03_RQ/prepreg_adjusted/results_nvi_preadj_model_ECCN.csv")
)

results_pre_preg_adj <- lapply(models_pre_preg_adj, function(m) {
  run_pre_preg_adj_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    output_file = m$file
  )
})

#---------------------------------11) Mediation----------------------------------
#install.packages("remotes")
remotes::install_github("datashield/dsMediationClient")
library(dsMediationClient)

ds.colnames("outcome_yearlyrep", datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# select the latest non-missing measurement for the outcome in each age interval
#install.packages("remotes")
#library(remotes)
#install_github("lifecycle-project/ds-helper")
library(dsHelper)

# Load output function
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
  
  # Add column 'N' with same value repeated
  n <- x$nobs
  smat <- cbind(smat, N = n)
  
  return(smat)
}

#-------------------------11a) Internalising symptoms----------------------------
dh.makeStrata( # can only be done in long format
  df = "outcome_yearlyrep",
  id_var = "child_id",
  age_var = "int_age_",
  var_to_subset = c("int_pc_"),
  bands = c(5,10),
  mult_action = "latest", # or 'nearest' + specify the age in other arguments
  new_obj = "df_int_pc",
  band_action = "ge_l",
  conns = connections[c('genr', 'dnbc',  'eden', 'abcd')],
  checks = TRUE)

ds.colnames("df_int_pc", datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])
ds.dim("df_int_pc", datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])

ds.dataFrame(x = c("df_int_pc$child_id", "df_int_pc$int_pc_.5_10",
                   "df_int_pc$int_age_.5_10"),
             newobj = "df_int_pc", completeCases = F,
             datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])

# wide format of core yearly rep (with edu_m.0 only)
ds.colnames('core_rep_w', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])
ds.dim('core_rep_w', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])

#Merge core_nonrep with df_int_pc
ds.merge(
  x.name = "core_nonrep",
  y.name = "df_int_pc",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_int_med",
  datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')]
)

#Merge df_int_med with core_rep_w
ds.merge(
  x.name = "df_int_med",
  y.name = "core_rep_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_int_med",
  datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')]
)

ds.colnames('df_int_med', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])
ds.dim('df_int_med', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])


# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "df_int_med$prepreg_weight/((df_int_med$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])


# bind to df
ds.cbind(
  x = c("df_int_med","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "df_int_med",
  datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')]
)

ds.dataFrame(x = c("df_int_med$int_pc_.5_10", "df_int_med$int_age_.5_10",
                   "df_int_med$ppd", "df_int_med$preg_dep", "df_int_med$sex", "df_int_med$edu_m_.0",
                   "df_int_med$agebirth_m_y", "df_int_med$preg_alc", "df_int_med$preg_smk",
                   "df_int_med$prepreg_BMI"),
             newobj = "df_int_med_full", completeCases = TRUE,
             datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])

ds.colnames('df_int_med_full', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])
ds.dim(x = 'df_int_med_full', datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')])

ds.asNumeric(x.name = "df_int_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('genr')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_int_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('dnbc')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_int_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('eden')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_int_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('abcd')]) # convert mediator to numeric if it's a factor

ds.dataFrame(x= c("df_int_med_full", "ppd.n"), newobj = "df_int_med_full", 
             datasources = connections[c("genr", "dnbc",  "eden", "abcd")])

# exclude ninfea due to underpowered sample size of 307
med.fit.int.DS <- ds.glmSLMA(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk  + prepreg_BMI", 
                             family='binomial', dataName = 'df_int_med_full', newobj ='med.fit.int.DS',
                             maxit = 100,
                             combine.with.metafor = FALSE,
                             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])
out.fit.int.DS <- ds.glmSLMA(formula="int_pc_.5_10 ~ ppd.n + preg_dep + sex + int_age_.5_10 + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                             dataName = 'df_int_med_full', newobj ='out.fit.int.DS', family = "gaussian", 
                             maxit = 100,
                             combine.with.metafor = FALSE,
                             datasources = connections[c('genr','dnbc','eden','abcd')]) 

library(dsMediationClient)
med.out.int.DS <- ds.mediate(model.m = 'med.fit.int.DS', model.y = 'out.fit.int.DS',
                             treat = "preg_dep", mediator = "ppd.n", robustSE = T, sims = 1000,
                             newobj = 'med.out.int.DS',
                             datasources = connections[c('genr', 'dnbc',  'eden', 'abcd')], 
                             seed=707)

#install.packages("mediation")
library(mediation)
#remove.packages("mediation")

output_mediation_genr <- extract_mediation_summary(summary(med.out.int.DS$genr))
output_mediation_dnbc <- extract_mediation_summary(summary(med.out.int.DS$dnbc))
output_mediation_eden <- extract_mediation_summary(summary(med.out.int.DS$eden))
output_mediation_abcd <- extract_mediation_summary(summary(med.out.int.DS$abcd))

# indirect effect
#plot(med.out.int.DS$genr)
ACME_genr_est <- output_mediation_genr['ACME', 'Estimate']
ACME_genr_upper <- output_mediation_genr['ACME', '95% CI Upper']
ACME_genr_lower <- output_mediation_genr['ACME', '95% CI Lower']
genr_N <- output_mediation_genr['ACME', 'N']

ACME_genr_SE <- (ACME_genr_upper - ACME_genr_lower)/3.92

#plot(med.out.int.DS$dnbc)
ACME_dnbc_est <- output_mediation_dnbc['ACME', 'Estimate']
ACME_dnbc_upper <- output_mediation_dnbc['ACME', '95% CI Upper']
ACME_dnbc_lower <- output_mediation_dnbc['ACME', '95% CI Lower']
dnbc_N <- output_mediation_dnbc['ACME', 'N']

ACME_dnbc_SE <- (ACME_dnbc_upper - ACME_dnbc_lower)/3.92

#plot(med.out.int.DS$eden)
ACME_eden_est <- output_mediation_eden['ACME', 'Estimate']
ACME_eden_upper <- output_mediation_eden['ACME', '95% CI Upper']
ACME_eden_lower <- output_mediation_eden['ACME', '95% CI Lower']
eden_N <- output_mediation_eden['ACME', 'N']

ACME_eden_SE <- (ACME_eden_upper - ACME_eden_lower)/3.92

#plot(med.out.int.DS$abcd)
ACME_abcd_est <- output_mediation_abcd['ACME', 'Estimate']
ACME_abcd_upper <- output_mediation_abcd['ACME', '95% CI Upper']
ACME_abcd_lower <- output_mediation_abcd['ACME', '95% CI Lower']
abcd_N <- output_mediation_abcd['ACME', 'N']

ACME_abcd_SE <- (ACME_abcd_upper - ACME_abcd_lower)/3.92

ACME_all_est <- rbind(ACME_genr_est, ACME_dnbc_est, ACME_eden_est, ACME_abcd_est)
ACME_all_SE <- rbind(ACME_genr_SE, ACME_dnbc_SE, ACME_eden_SE, ACME_abcd_SE)
all_N <- rbind(genr_N, dnbc_N, eden_N, abcd_N)

indirect_INT <- as.data.frame(cbind(ACME_all_est,ACME_all_SE,all_N))
indirect_INT$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(indirect_INT) <- c('ind_betas', 'ses', 'N', 'cohort')
write.csv(indirect_INT, "results/03_RQ/post_mediated/results_INT_ind_ppdmed_model_ECCN_5_10.csv")

# direct effect
#GenR
ADE_genr_est <- output_mediation_genr['ADE', 'Estimate']
ADE_genr_upper <- output_mediation_genr['ADE', '95% CI Upper']
ADE_genr_lower <- output_mediation_genr['ADE', '95% CI Lower']

ADE_genr_SE <- (ADE_genr_upper - ADE_genr_lower)/3.92

#DNBC
ADE_dnbc_est <- output_mediation_dnbc['ADE', 'Estimate']
ADE_dnbc_upper <- output_mediation_dnbc['ADE', '95% CI Upper']
ADE_dnbc_lower <- output_mediation_dnbc['ADE', '95% CI Lower']

ADE_dnbc_SE <- (ADE_dnbc_upper - ADE_dnbc_lower)/3.92

#EDEN
ADE_eden_est <- output_mediation_eden['ADE', 'Estimate']
ADE_eden_upper <- output_mediation_eden['ADE', '95% CI Upper']
ADE_eden_lower <- output_mediation_eden['ADE', '95% CI Lower']

ADE_eden_SE <- (ADE_eden_upper - ADE_eden_lower)/3.92

#ABCD
ADE_abcd_est <- output_mediation_abcd['ADE', 'Estimate']
ADE_abcd_upper <- output_mediation_abcd['ADE', '95% CI Upper']
ADE_abcd_lower <- output_mediation_abcd['ADE', '95% CI Lower']

ADE_abcd_SE <- (ADE_abcd_upper - ADE_abcd_lower)/3.92

ADE_all_est <- rbind(ADE_genr_est, ADE_dnbc_est, ADE_eden_est, ADE_abcd_est)
ADE_all_SE <- rbind(ADE_genr_SE, ADE_dnbc_SE, ADE_eden_SE, ADE_abcd_SE)

direct_INT <- as.data.frame(cbind(ADE_all_est,ADE_all_SE,all_N))
direct_INT$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(direct_INT) <- c('dir_betas', 'ses', 'N', 'cohort')
write.csv(direct_INT, "results/03_RQ/post_mediated/results_INT_dir_ppdmed_model_ECCN_5_10.csv")

# total effect
#GenR
TE_genr_est <- output_mediation_genr['Total Effect', 'Estimate']
TE_genr_upper <- output_mediation_genr['Total Effect', '95% CI Upper']
TE_genr_lower <- output_mediation_genr['Total Effect', '95% CI Lower']

TE_genr_SE <- (TE_genr_upper - TE_genr_lower)/3.92

#DNBC
TE_dnbc_est <- output_mediation_dnbc['Total Effect', 'Estimate']
TE_dnbc_upper <- output_mediation_dnbc['Total Effect', '95% CI Upper']
TE_dnbc_lower <- output_mediation_dnbc['Total Effect', '95% CI Lower']

TE_dnbc_SE <- (TE_dnbc_upper - TE_dnbc_lower)/3.92

#EDEN
TE_eden_est <- output_mediation_eden['Total Effect', 'Estimate']
TE_eden_upper <- output_mediation_eden['Total Effect', '95% CI Upper']
TE_eden_lower <- output_mediation_eden['Total Effect', '95% CI Lower']

TE_eden_SE <- (TE_eden_upper - TE_eden_lower)/3.92

#ABCD
TE_abcd_est <- output_mediation_abcd['Total Effect', 'Estimate']
TE_abcd_upper <- output_mediation_abcd['Total Effect', '95% CI Upper']
TE_abcd_lower <- output_mediation_abcd['Total Effect', '95% CI Lower']

TE_abcd_SE <- (TE_abcd_upper - TE_abcd_lower)/3.92

TE_all_est <- rbind(TE_genr_est, TE_dnbc_est, TE_eden_est, TE_abcd_est)
TE_all_SE <- rbind(TE_genr_SE, TE_dnbc_SE, TE_eden_SE, TE_abcd_SE)

total_INT <- as.data.frame(cbind(TE_all_est,TE_all_SE,all_N))
total_INT$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(total_INT) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_INT, "results/03_RQ/post_mediated/results_INT_tot_ppdmed_model_ECCN_5_10.csv")

#GenR
PM_genr_est <- output_mediation_genr['Prop. Mediated', 'Estimate']
PM_genr_upper <- output_mediation_genr['Prop. Mediated', '95% CI Upper']
PM_genr_lower <- output_mediation_genr['Prop. Mediated', '95% CI Lower']

PM_genr_SE <- (PM_genr_upper - PM_genr_lower)/3.92

#DNBC
PM_dnbc_est <- output_mediation_dnbc['Prop. Mediated', 'Estimate']
PM_dnbc_upper <- output_mediation_dnbc['Prop. Mediated', '95% CI Upper']
PM_dnbc_lower <- output_mediation_dnbc['Prop. Mediated', '95% CI Lower']

PM_dnbc_SE <- (PM_dnbc_upper - PM_dnbc_lower)/3.92

#EDEN
PM_eden_est <- output_mediation_eden['Prop. Mediated', 'Estimate']
PM_eden_upper <- output_mediation_eden['Prop. Mediated', '95% CI Upper']
PM_eden_lower <- output_mediation_eden['Prop. Mediated', '95% CI Lower']

PM_eden_SE <- (PM_eden_upper - PM_eden_lower)/3.92

#ABCD
PM_abcd_est <- output_mediation_abcd['Prop. Mediated', 'Estimate']
PM_abcd_upper <- output_mediation_abcd['Prop. Mediated', '95% CI Upper']
PM_abcd_lower <- output_mediation_abcd['Prop. Mediated', '95% CI Lower']

PM_abcd_SE <- (PM_abcd_upper - PM_abcd_lower)/3.92

PM_all_est <- rbind(PM_genr_est, PM_dnbc_est, PM_eden_est, PM_abcd_est)
PM_all_SE <- rbind(PM_genr_SE, PM_dnbc_SE, PM_eden_SE, PM_abcd_SE)

total_INT_PM <- as.data.frame(cbind(PM_all_est,PM_all_SE,all_N))
total_INT_PM$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(total_INT_PM) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_INT_PM, "results/03_RQ/post_mediated/results_INT_PM_ppdmed_model_ECCN_5_10.csv")

#-------------------------11b) Externalising symptoms----------------------------
dh.makeStrata( # can only be done in long format
  df = "outcome_yearlyrep",
  id_var = "child_id",
  age_var = "ext_age_",
  var_to_subset = c("ext_pc_"),
  bands = c(5,10),
  mult_action = "latest", # or 'nearest' + specify the age in other arguments
  new_obj = "df_ext_pc",
  band_action = "ge_l",
  conns = connections[c('genr', 'dnbc', 'eden', 'abcd')],
  checks = TRUE)

ds.colnames("df_ext_pc", datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])
ds.dim("df_ext_pc", datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

ds.dataFrame(x = c("df_ext_pc$child_id", "df_ext_pc$ext_pc_.5_10",
                   "df_ext_pc$ext_age_.5_10"),
             newobj = "df_ext_pc", completeCases = F,
             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

# wide format of core yearly rep (with edu_m.0 only)
ds.colnames('core_rep_w', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])
ds.dim('core_rep_w', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

#Merge core_nonrep with df_ext_pc
ds.merge(
  x.name = "core_nonrep",
  y.name = "df_ext_pc",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_ext_med",
  datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')]
)

#Merge df_ext_med with core_rep_w
ds.merge(
  x.name = "df_ext_med",
  y.name = "core_rep_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_ext_med",
  datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')]
)

ds.colnames('df_ext_med', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])
ds.dim('df_ext_med', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "df_ext_med$prepreg_weight/((df_ext_med$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])


# bind to df
ds.cbind(
  x = c("df_ext_med","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "df_ext_med",
  datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')]
)

ds.dataFrame(x = c("df_ext_med$ext_pc_.5_10", "df_ext_med$ext_age_.5_10",
                   "df_ext_med$ppd", "df_ext_med$preg_dep", "df_ext_med$sex", "df_ext_med$edu_m_.0",
                   "df_ext_med$agebirth_m_y", "df_ext_med$preg_alc", "df_ext_med$preg_smk",
                   "df_ext_med$prepreg_BMI"),
             newobj = "df_ext_med_full", completeCases = TRUE,
             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

ds.dim(x = 'df_ext_med_full', datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

ds.asNumeric(x.name = "df_ext_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('genr')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_ext_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('dnbc')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_ext_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('eden')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_ext_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('abcd')]) # convert mediator to numeric if it's a factor

ds.dataFrame(x= c("df_ext_med_full", "ppd.n"), newobj = "df_ext_med_full", 
             datasources = connections[c("genr", "dnbc", "eden", "abcd")])

# exclude ninfea due to underpowered sample size of 76
med.fit.ext.DS <- ds.glmSLMA(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                             maxit = 100,
                             combine.with.metafor = FALSE,
                             family='binomial', dataName = 'df_ext_med_full', newobj ='med.fit.ext.DS',
                             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

out.fit.ext.DS <- ds.glmSLMA(formula="ext_pc_.5_10 ~ ppd.n + preg_dep + sex + ext_age_.5_10 + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                             maxit = 100,
                             combine.with.metafor = FALSE,
                             dataName = 'df_ext_med_full', newobj ='out.fit.ext.DS', family = "gaussian", 
                             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')])

med.out.ext.DS <- ds.mediate(model.m = 'med.fit.ext.DS', model.y = 'out.fit.ext.DS',
                             treat = "preg_dep", mediator = "ppd.n", robustSE = T, sims = 1000,
                             newobj = 'med.out.ext.DS',
                             datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')],
                             seed=707)

#install.packages("mediation")
library(mediation)
#remove.packages("mediation")

output_mediation_genr <- extract_mediation_summary(summary(med.out.ext.DS$genr))
output_mediation_dnbc <- extract_mediation_summary(summary(med.out.ext.DS$dnbc))
output_mediation_eden <- extract_mediation_summary(summary(med.out.ext.DS$eden))
output_mediation_abcd <- extract_mediation_summary(summary(med.out.ext.DS$abcd))

# indirect effect
#plot(med.out.ext.DS$genr)
ACME_genr_est <- output_mediation_genr['ACME', 'Estimate']
ACME_genr_upper <- output_mediation_genr['ACME', '95% CI Upper']
ACME_genr_lower <- output_mediation_genr['ACME', '95% CI Lower']
genr_N <- output_mediation_genr['ACME', 'N']

ACME_genr_SE <- (ACME_genr_upper - ACME_genr_lower)/3.92

#plot(med.out.ext.DS$dnbc)
ACME_dnbc_est <- output_mediation_dnbc['ACME', 'Estimate']
ACME_dnbc_upper <- output_mediation_dnbc['ACME', '95% CI Upper']
ACME_dnbc_lower <- output_mediation_dnbc['ACME', '95% CI Lower']
dnbc_N <- output_mediation_dnbc['ACME', 'N']

ACME_dnbc_SE <- (ACME_dnbc_upper - ACME_dnbc_lower)/3.92

#plot(med.out.ext.DS$eden)
ACME_eden_est <- output_mediation_eden['ACME', 'Estimate']
ACME_eden_upper <- output_mediation_eden['ACME', '95% CI Upper']
ACME_eden_lower <- output_mediation_eden['ACME', '95% CI Lower']
eden_N <- output_mediation_eden['ACME', 'N']

ACME_eden_SE <- (ACME_eden_upper - ACME_eden_lower)/3.92

#plot(med.out.ext.DS$abcd)
ACME_abcd_est <- output_mediation_abcd['ACME', 'Estimate']
ACME_abcd_upper <- output_mediation_abcd['ACME', '95% CI Upper']
ACME_abcd_lower <- output_mediation_abcd['ACME', '95% CI Lower']
abcd_N <- output_mediation_abcd['ACME', 'N']

ACME_abcd_SE <- (ACME_abcd_upper - ACME_abcd_lower)/3.92

ACME_all_est <- rbind(ACME_genr_est, ACME_dnbc_est, ACME_eden_est, ACME_abcd_est)
ACME_all_SE <- rbind(ACME_genr_SE, ACME_dnbc_SE, ACME_eden_SE, ACME_abcd_SE)
all_N <- rbind(genr_N, dnbc_N, eden_N, abcd_N)

indirect_EXT <- as.data.frame(cbind(ACME_all_est,ACME_all_SE,all_N))
indirect_EXT$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(indirect_EXT) <- c('ind_betas', 'ses', 'N', 'cohort')
write.csv(indirect_EXT, "results/03_RQ/post_mediated/results_EXT_ind_ppdmed_model_ECCN_5_10.csv")

# direct effect
#GenR
ADE_genr_est <- output_mediation_genr['ADE', 'Estimate']
ADE_genr_upper <- output_mediation_genr['ADE', '95% CI Upper']
ADE_genr_lower <- output_mediation_genr['ADE', '95% CI Lower']

ADE_genr_SE <- (ADE_genr_upper - ADE_genr_lower)/3.92

#DNBC
ADE_dnbc_est <- output_mediation_dnbc['ADE', 'Estimate']
ADE_dnbc_upper <- output_mediation_dnbc['ADE', '95% CI Upper']
ADE_dnbc_lower <- output_mediation_dnbc['ADE', '95% CI Lower']

ADE_dnbc_SE <- (ADE_dnbc_upper - ADE_dnbc_lower)/3.92

#EDEN
ADE_eden_est <- output_mediation_eden['ADE', 'Estimate']
ADE_eden_upper <- output_mediation_eden['ADE', '95% CI Upper']
ADE_eden_lower <- output_mediation_eden['ADE', '95% CI Lower']

ADE_eden_SE <- (ADE_eden_upper - ADE_eden_lower)/3.92

#ABCD
ADE_abcd_est <- output_mediation_abcd['ADE', 'Estimate']
ADE_abcd_upper <- output_mediation_abcd['ADE', '95% CI Upper']
ADE_abcd_lower <- output_mediation_abcd['ADE', '95% CI Lower']

ADE_abcd_SE <- (ADE_abcd_upper - ADE_abcd_lower)/3.92

ADE_all_est <- rbind(ADE_genr_est, ADE_dnbc_est, ADE_eden_est, ADE_abcd_est)
ADE_all_SE <- rbind(ADE_genr_SE, ADE_dnbc_SE, ADE_eden_SE, ADE_abcd_SE)

direct_EXT <- as.data.frame(cbind(ADE_all_est,ADE_all_SE,all_N))
direct_EXT$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(direct_EXT) <- c('dir_betas', 'ses', 'N', 'cohort')
write.csv(direct_EXT, "results/03_RQ/post_mediated/results_EXT_dir_ppdmed_model_ECCN_5_10.csv")

# total effect
#GenR
TE_genr_est <- output_mediation_genr['Total Effect', 'Estimate']
TE_genr_upper <- output_mediation_genr['Total Effect', '95% CI Upper']
TE_genr_lower <- output_mediation_genr['Total Effect', '95% CI Lower']

TE_genr_SE <- (TE_genr_upper - TE_genr_lower)/3.92

#DNBC
TE_dnbc_est <- output_mediation_dnbc['Total Effect', 'Estimate']
TE_dnbc_upper <- output_mediation_dnbc['Total Effect', '95% CI Upper']
TE_dnbc_lower <- output_mediation_dnbc['Total Effect', '95% CI Lower']

TE_dnbc_SE <- (TE_dnbc_upper - TE_dnbc_lower)/3.92

#EDEN
TE_eden_est <- output_mediation_eden['Total Effect', 'Estimate']
TE_eden_upper <- output_mediation_eden['Total Effect', '95% CI Upper']
TE_eden_lower <- output_mediation_eden['Total Effect', '95% CI Lower']

TE_eden_SE <- (TE_eden_upper - TE_eden_lower)/3.92

#ABCD
TE_abcd_est <- output_mediation_abcd['Total Effect', 'Estimate']
TE_abcd_upper <- output_mediation_abcd['Total Effect', '95% CI Upper']
TE_abcd_lower <- output_mediation_abcd['Total Effect', '95% CI Lower']

TE_abcd_SE <- (TE_abcd_upper - TE_abcd_lower)/3.92

TE_all_est <- rbind(TE_genr_est, TE_dnbc_est, TE_eden_est, TE_abcd_est)
TE_all_SE <- rbind(TE_genr_SE, TE_dnbc_SE, TE_eden_SE, TE_abcd_SE)

total_EXT <- as.data.frame(cbind(TE_all_est,TE_all_SE,all_N))
total_EXT$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(total_EXT) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_EXT, "results/03_RQ/post_mediated/results_EXT_tot_ppdmed_model_ECCN_5_10.csv")

#GenR
PM_genr_est <- output_mediation_genr['Prop. Mediated', 'Estimate']
PM_genr_upper <- output_mediation_genr['Prop. Mediated', '95% CI Upper']
PM_genr_lower <- output_mediation_genr['Prop. Mediated', '95% CI Lower']

PM_genr_SE <- (PM_genr_upper - PM_genr_lower)/3.92

#DNBC
PM_dnbc_est <- output_mediation_dnbc['Prop. Mediated', 'Estimate']
PM_dnbc_upper <- output_mediation_dnbc['Prop. Mediated', '95% CI Upper']
PM_dnbc_lower <- output_mediation_dnbc['Prop. Mediated', '95% CI Lower']

PM_dnbc_SE <- (PM_dnbc_upper - PM_dnbc_lower)/3.92

#EDEN
PM_eden_est <- output_mediation_eden['Prop. Mediated', 'Estimate']
PM_eden_upper <- output_mediation_eden['Prop. Mediated', '95% CI Upper']
PM_eden_lower <- output_mediation_eden['Prop. Mediated', '95% CI Lower']

PM_eden_SE <- (PM_eden_upper - PM_eden_lower)/3.92

#ABCD
PM_abcd_est <- output_mediation_abcd['Prop. Mediated', 'Estimate']
PM_abcd_upper <- output_mediation_abcd['Prop. Mediated', '95% CI Upper']
PM_abcd_lower <- output_mediation_abcd['Prop. Mediated', '95% CI Lower']

PM_abcd_SE <- (PM_abcd_upper - PM_abcd_lower)/3.92

PM_all_est <- rbind(PM_genr_est, PM_dnbc_est, PM_eden_est, PM_abcd_est)
PM_all_SE <- rbind(PM_genr_SE, PM_dnbc_SE, PM_eden_SE, PM_abcd_SE)

total_EXT_PM <- as.data.frame(cbind(PM_all_est,PM_all_SE,all_N))
total_EXT_PM$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(total_EXT_PM) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_EXT_PM, "results/03_RQ/post_mediated/results_EXT_PM_ppdmed_model_ECCN_5_10.csv")
#-----------------------------11c) ADHD symptoms--------------------------------
dh.makeStrata( # can only be done in long format
  df = "outcome_yearlyrep",
  id_var = "child_id",
  age_var = "adhd_age_",
  var_to_subset = c("adhd_pc_"),
  bands = c(3,9),
  mult_action = "latest", # or 'nearest' + specify the age in other arguments
  new_obj = "df_adhd_pc",
  band_action = "ge_l", #greater than or equal to the lowest band and less than the highest band
  conns = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')],
  checks = TRUE)

ds.colnames("df_adhd_pc", datasources = connections[c('abcd','genr', 'dnbc','eden', 'ninfea')])
ds.dim("df_adhd_pc", datasources = connections[c('abcd', 'genr', 'dnbc','eden', 'ninfea')])

ds.dataFrame(x = c("df_adhd_pc$child_id", "df_adhd_pc$adhd_pc_.3_9",
                   "df_adhd_pc$adhd_age_.3_9"),
             newobj = "df_adhd_pc", completeCases = F,
             datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

# wide format of core yearly rep (with edu_m.0 only)
ds.colnames('core_rep_w', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])
ds.dim('core_rep_w', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

#Merge core_nonrep with df_adhd_pc
ds.merge(
  x.name = "core_nonrep",
  y.name = "df_adhd_pc",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_adhd_med",
  datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')]
)

#Merge df_adhd_med with core_rep_w
ds.merge(
  x.name = "df_adhd_med",
  y.name = "core_rep_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_adhd_med",
  datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')]
)

ds.colnames('df_adhd_med', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])
ds.dim('df_adhd_med', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "df_adhd_med$prepreg_weight/((df_adhd_med$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])


# bind to df
ds.cbind(
  x = c("df_adhd_med","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "df_adhd_med",
  datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')]
)

ds.dataFrame(x = c("df_adhd_med$adhd_pc_.3_9", "df_adhd_med$adhd_age_.3_9",
                   "df_adhd_med$ppd", "df_adhd_med$preg_dep", "df_adhd_med$sex", "df_adhd_med$edu_m_.0",
                   "df_adhd_med$agebirth_m_y", "df_adhd_med$preg_alc", "df_adhd_med$preg_smk",
                   "df_adhd_med$prepreg_BMI"),
             newobj = "df_adhd_med_full", completeCases = TRUE,
             datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

ds.colnames('df_adhd_med_full', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])
ds.dim(x = 'df_adhd_med_full', datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

ds.asNumeric(x.name = "df_adhd_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('abcd')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_adhd_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('genr')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_adhd_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('dnbc')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_adhd_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('eden')]) # convert mediator to numeric if it's a factor

ds.asNumeric(x.name = "df_adhd_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('ninfea')]) # convert mediator to numeric if it's a factor

ds.dataFrame(x= c("df_adhd_med_full", "ppd.n"), newobj = "df_adhd_med_full", 
             datasources = connections[c("abcd","genr","dnbc","eden", "ninfea")])

med.fit.adhd.DS <- ds.glmSLMA(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                              maxit = 100,
                              combine.with.metafor = FALSE,
                             family='binomial', dataName = 'df_adhd_med_full', newobj ='med.fit.adhd.DS',
                             datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

out.fit.adhd.DS <- ds.glmSLMA(formula="adhd_pc_.3_9 ~ ppd.n + preg_dep + sex + adhd_age_.3_9 + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                              maxit = 100,
                              combine.with.metafor = FALSE,
                              dataName = 'df_adhd_med_full', newobj ='out.fit.adhd.DS', family = "gaussian", 
                             datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')])

med.out.adhd.DS <- ds.mediate(model.m = 'med.fit.adhd.DS', model.y = 'out.fit.adhd.DS',
                             treat = "preg_dep", mediator = "ppd.n", robustSE = T, sims = 1000,
                             newobj = 'med.out.adhd.DS',
                             datasources = connections[c('abcd', 'genr', 'dnbc', 'eden', 'ninfea')],
                             seed=707)

#install.packages("mediation")
library(mediation)
#remove.packages("mediation")

output_mediation_abcd <- extract_mediation_summary(med.out.adhd.DS$abcd)
output_mediation_genr <- extract_mediation_summary(med.out.adhd.DS$genr)
output_mediation_dnbc <- extract_mediation_summary(med.out.adhd.DS$dnbc)
output_mediation_eden <- extract_mediation_summary(med.out.adhd.DS$eden)
output_mediation_ninfea <- extract_mediation_summary(med.out.adhd.DS$ninfea)

# indirect effect
#plot(med.out.adhd.DS$genr)
ACME_genr_est <- output_mediation_genr['ACME', 'Estimate']
ACME_genr_upper <- output_mediation_genr['ACME', '95% CI Upper']
ACME_genr_lower <- output_mediation_genr['ACME', '95% CI Lower']
genr_N <- output_mediation_genr['ACME', 'N']

ACME_genr_SE <- (ACME_genr_upper - ACME_genr_lower)/3.92

#plot(med.out.adhd.DS$dnbc)
ACME_dnbc_est <- output_mediation_dnbc['ACME', 'Estimate']
ACME_dnbc_upper <- output_mediation_dnbc['ACME', '95% CI Upper']
ACME_dnbc_lower <- output_mediation_dnbc['ACME', '95% CI Lower']
dnbc_N <- output_mediation_dnbc['ACME', 'N']

ACME_dnbc_SE <- (ACME_dnbc_upper - ACME_dnbc_lower)/3.92

#plot(med.out.adhd.DS$eden)
ACME_eden_est <- output_mediation_eden['ACME', 'Estimate']
ACME_eden_upper <- output_mediation_eden['ACME', '95% CI Upper']
ACME_eden_lower <- output_mediation_eden['ACME', '95% CI Lower']
eden_N <- output_mediation_eden['ACME', 'N']

ACME_eden_SE <- (ACME_eden_upper - ACME_eden_lower)/3.92

#plot(med.out.adhd.DS$abcd)
ACME_abcd_est <- output_mediation_abcd['ACME', 'Estimate']
ACME_abcd_upper <- output_mediation_abcd['ACME', '95% CI Upper']
ACME_abcd_lower <- output_mediation_abcd['ACME', '95% CI Lower']
abcd_N <- output_mediation_abcd['ACME', 'N']

ACME_abcd_SE <- (ACME_abcd_upper - ACME_abcd_lower)/3.92

ACME_all_est <- rbind(ACME_genr_est, ACME_dnbc_est, ACME_eden_est, ACME_abcd_est)
ACME_all_SE <- rbind(ACME_genr_SE, ACME_dnbc_SE, ACME_eden_SE, ACME_abcd_SE)
all_N <- rbind(genr_N, dnbc_N, eden_N, abcd_N)

indirect_ADHD <- as.data.frame(cbind(ACME_all_est,ACME_all_SE,all_N))
indirect_ADHD$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(indirect_ADHD) <- c('ind_betas', 'ses', 'N', 'cohort')
write.csv(indirect_ADHD, "results/03_RQ/post_mediated/results_ADHD_ind_ppdmed_model_ECCN_3_9.csv")

# direct effect
#GenR
ADE_genr_est <- output_mediation_genr['ADE', 'Estimate']
ADE_genr_upper <- output_mediation_genr['ADE', '95% CI Upper']
ADE_genr_lower <- output_mediation_genr['ADE', '95% CI Lower']

ADE_genr_SE <- (ADE_genr_upper - ADE_genr_lower)/3.92

#DNBC
ADE_dnbc_est <- output_mediation_dnbc['ADE', 'Estimate']
ADE_dnbc_upper <- output_mediation_dnbc['ADE', '95% CI Upper']
ADE_dnbc_lower <- output_mediation_dnbc['ADE', '95% CI Lower']

ADE_dnbc_SE <- (ADE_dnbc_upper - ADE_dnbc_lower)/3.92

#EDEN
ADE_eden_est <- output_mediation_eden['ADE', 'Estimate']
ADE_eden_upper <- output_mediation_eden['ADE', '95% CI Upper']
ADE_eden_lower <- output_mediation_eden['ADE', '95% CI Lower']

ADE_eden_SE <- (ADE_eden_upper - ADE_eden_lower)/3.92

#ABCD
ADE_abcd_est <- output_mediation_abcd['ADE', 'Estimate']
ADE_abcd_upper <- output_mediation_abcd['ADE', '95% CI Upper']
ADE_abcd_lower <- output_mediation_abcd['ADE', '95% CI Lower']

ADE_abcd_SE <- (ADE_abcd_upper - ADE_abcd_lower)/3.92

ADE_all_est <- rbind(ADE_genr_est, ADE_dnbc_est, ADE_eden_est, ADE_abcd_est)
ADE_all_SE <- rbind(ADE_genr_SE, ADE_dnbc_SE, ADE_eden_SE, ADE_abcd_SE)

direct_ADHD <- as.data.frame(cbind(ADE_all_est,ADE_all_SE,all_N))
direct_ADHD$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(direct_ADHD) <- c('dir_betas', 'ses', 'N', 'cohort')
write.csv(direct_ADHD, "results/03_RQ/post_mediated/results_ADHD_dir_ppdmed_model_ECCN_3_9.csv")

# total effect
#GenR
TE_genr_est <- output_mediation_genr['Total Effect', 'Estimate']
TE_genr_upper <- output_mediation_genr['Total Effect', '95% CI Upper']
TE_genr_lower <- output_mediation_genr['Total Effect', '95% CI Lower']

TE_genr_SE <- (TE_genr_upper - TE_genr_lower)/3.92

#DNBC
TE_dnbc_est <- output_mediation_dnbc['Total Effect', 'Estimate']
TE_dnbc_upper <- output_mediation_dnbc['Total Effect', '95% CI Upper']
TE_dnbc_lower <- output_mediation_dnbc['Total Effect', '95% CI Lower']

TE_dnbc_SE <- (TE_dnbc_upper - TE_dnbc_lower)/3.92

#EDEN
TE_eden_est <- output_mediation_eden['Total Effect', 'Estimate']
TE_eden_upper <- output_mediation_eden['Total Effect', '95% CI Upper']
TE_eden_lower <- output_mediation_eden['Total Effect', '95% CI Lower']

TE_eden_SE <- (TE_eden_upper - TE_eden_lower)/3.92

#ABCD
TE_abcd_est <- output_mediation_abcd['Total Effect', 'Estimate']
TE_abcd_upper <- output_mediation_abcd['Total Effect', '95% CI Upper']
TE_abcd_lower <- output_mediation_abcd['Total Effect', '95% CI Lower']

TE_abcd_SE <- (TE_abcd_upper - TE_abcd_lower)/3.92

TE_all_est <- rbind(TE_genr_est, TE_dnbc_est, TE_eden_est, TE_abcd_est)
TE_all_SE <- rbind(TE_genr_SE, TE_dnbc_SE, TE_eden_SE, TE_abcd_SE)

total_ADHD <- as.data.frame(cbind(TE_all_est,TE_all_SE,all_N))
total_ADHD$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(total_ADHD) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_ADHD, "results/03_RQ/post_mediated/results_ADHD_tot_ppdmed_model_ECCN_3_9.csv")

#GenR
PM_genr_est <- output_mediation_genr['Prop. Mediated', 'Estimate']
PM_genr_upper <- output_mediation_genr['Prop. Mediated', '95% CI Upper']
PM_genr_lower <- output_mediation_genr['Prop. Mediated', '95% CI Lower']

PM_genr_SE <- (PM_genr_upper - PM_genr_lower)/3.92

#DNBC
PM_dnbc_est <- output_mediation_dnbc['Prop. Mediated', 'Estimate']
PM_dnbc_upper <- output_mediation_dnbc['Prop. Mediated', '95% CI Upper']
PM_dnbc_lower <- output_mediation_dnbc['Prop. Mediated', '95% CI Lower']

PM_dnbc_SE <- (PM_dnbc_upper - PM_dnbc_lower)/3.92

#EDEN
PM_eden_est <- output_mediation_eden['Prop. Mediated', 'Estimate']
PM_eden_upper <- output_mediation_eden['Prop. Mediated', '95% CI Upper']
PM_eden_lower <- output_mediation_eden['Prop. Mediated', '95% CI Lower']

PM_eden_SE <- (PM_eden_upper - PM_eden_lower)/3.92

#ABCD
PM_abcd_est <- output_mediation_abcd['Prop. Mediated', 'Estimate']
PM_abcd_upper <- output_mediation_abcd['Prop. Mediated', '95% CI Upper']
PM_abcd_lower <- output_mediation_abcd['Prop. Mediated', '95% CI Lower']

PM_abcd_SE <- (PM_abcd_upper - PM_abcd_lower)/3.92

PM_all_est <- rbind(PM_genr_est, PM_dnbc_est, PM_eden_est, PM_abcd_est)
PM_all_SE <- rbind(PM_genr_SE, PM_dnbc_SE, PM_eden_SE, PM_abcd_SE)

total_ADHD_PM <- as.data.frame(cbind(PM_all_est,PM_all_SE,all_N))
total_ADHD_PM$cohort <- c('genr', 'dnbc', 'eden', 'abcd')
colnames(total_ADHD_PM) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_ADHD_PM, "results/03_RQ/post_mediated/results_ADHD_PM_ppdmed_model_ECCN_3_9.csv")
#------------------------------11d) ASD symptoms--------------------------------
dh.makeStrata( # can only be done in long format
  df = "outcome_yearlyrep",
  id_var = "child_id",
  age_var = "asd_age_",
  var_to_subset = c("asd_pc_"),
  bands = c(0,10),
  mult_action = "latest", # or 'nearest' + specify the age in other arguments
  new_obj = "df_asd_pc",
  band_action = "ge_l",
  conns = connections[c('genr')],
  checks = TRUE)

ds.colnames("df_asd_pc", datasources = connections[c('genr')])
ds.dim("df_asd_pc", datasources = connections[c('genr')])

ds.dataFrame(x = c("df_asd_pc$child_id", "df_asd_pc$asd_pc_.0_10",
                   "df_asd_pc$asd_age_.0_10"),
             newobj = "df_asd_pc", completeCases = F,
             datasources = connections[c('genr')])

# wide format of core yearly rep (with edu_m.0 only)
ds.colnames('core_rep_w', datasources = connections[c('genr')])
ds.dim('core_rep_w', datasources = connections[c('genr')])

#Merge core_nonrep with df_asd_pc
ds.merge(
  x.name = "core_nonrep",
  y.name = "df_asd_pc",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_asd_med",
  datasources = connections[c('genr')]
)

#Merge df_asd_med with core_rep_w
ds.merge(
  x.name = "df_asd_med",
  y.name = "core_rep_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  newobj = "df_asd_med",
  datasources = connections[c('genr')]
)

ds.colnames('df_asd_med', datasources = connections[c('genr')])
ds.dim('df_asd_med', datasources = connections[c('genr')])

# Create variable pre-pregnancy BMI from prepreg_weight and height_m

# Note: height_ m is in cm but should be in m for calculation of BMI
ds.make(toAssign = "df_asd_med$prepreg_weight/((df_asd_med$height_m/100)^2)", 
        newobj = "prepreg_BMI", 
        datasources = connections[c('genr')])

# check 
ds.mean(x = 'prepreg_BMI', datasources = connections[c('genr')])

# bind to df
ds.cbind(
  x = c("df_asd_med","prepreg_BMI"),
  DataSHIELD.checks = FALSE,
  newobj = "df_asd_med",
  datasources = connections[c('genr')]
)

ds.dataFrame(x = c("df_asd_med$asd_pc_.0_10", "df_asd_med$asd_age_.0_10",
                   "df_asd_med$ppd", "df_asd_med$preg_dep", "df_asd_med$sex", "df_asd_med$edu_m_.0",
                   "df_asd_med$agebirth_m_y", "df_asd_med$preg_alc", "df_asd_med$preg_smk",
                   "df_asd_med$prepreg_BMI"),
             newobj = "df_asd_med_full", completeCases = TRUE,
             datasources = connections[c('genr')])

ds.dim(x = 'df_asd_med_full', datasources = connections[c('genr')])

ds.asNumeric(x.name = "df_asd_med_full$ppd", newobj = "ppd.n", 
             datasources = connections[c('genr')]) # convert mediator to numeric if it's a factor

ds.dataFrame(x= c("df_asd_med_full", "ppd.n"), newobj = "df_asd_med_full", 
             datasources = connections[c("genr")])

# exclude ninfea due to underpowered sample size of 76
med.fit.asd.DS <- ds.glmSLMA(formula="ppd.n ~ preg_dep + sex + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                              family='binomial', dataName = 'df_asd_med_full', newobj ='med.fit.asd.DS',
                             maxit = 100,
                             combine.with.metafor = FALSE,
                              datasources = connections[c('genr')])

out.fit.asd.DS <- ds.glmSLMA(formula="asd_pc_.0_10 ~ ppd.n + preg_dep + sex + asd_age_.0_10 + edu_m_.0 + agebirth_m_y + preg_alc + preg_smk + prepreg_BMI",
                              dataName = 'df_asd_med_full', newobj ='out.fit.asd.DS', family = "gaussian", 
                             maxit = 100,
                             combine.with.metafor = FALSE,
                              datasources = connections[c('genr')])

med.out.asd.DS <- ds.mediate(model.m = 'med.fit.asd.DS', model.y = 'out.fit.asd.DS',
                              treat = "preg_dep", mediator = "ppd.n", robustSE = T, sims = 1000,
                              newobj = 'med.out.asd.DS',
                              datasources = connections[c('genr')],
                             seed=707)

#install.packages("mediation")
library(mediation)
#remove.packages("mediation")

output_mediation_genr <- extract_mediation_summary(med.out.asd.DS$genr)

# indirect effect
#plot(med.out.asd.DS$genr)
ACME_genr_est <- output_mediation_genr['ACME', 'Estimate']
ACME_genr_upper <- output_mediation_genr['ACME', '95% CI Upper']
ACME_genr_lower <- output_mediation_genr['ACME', '95% CI Lower']
genr_N <- output_mediation_genr['ACME', 'N']

ACME_genr_SE <- (ACME_genr_upper - ACME_genr_lower)/3.92

ACME_all_est <- rbind(ACME_genr_est)
ACME_all_SE <- rbind(ACME_genr_SE)
all_N <- rbind(genr_N)

indirect_ASD <- as.data.frame(cbind(ACME_all_est,ACME_all_SE,all_N))
indirect_ASD$cohort <- c('genr')
colnames(indirect_ASD) <- c('ind_betas', 'ses', 'N', 'cohort')
write.csv(indirect_ASD, "results/03_RQ/post_mediated/results_ASD_ind_ppdmed_model_ECCN_0_10.csv")

# direct effect
#GenR
ADE_genr_est <- output_mediation_genr['ADE', 'Estimate']
ADE_genr_upper <- output_mediation_genr['ADE', '95% CI Upper']
ADE_genr_lower <- output_mediation_genr['ADE', '95% CI Lower']

ADE_genr_SE <- (ADE_genr_upper - ADE_genr_lower)/3.92

ADE_all_est <- rbind(ADE_genr_est)
ADE_all_SE <- rbind(ADE_genr_SE)

direct_ASD <- as.data.frame(cbind(ADE_all_est,ADE_all_SE,all_N))
direct_ASD$cohort <- c('genr')
colnames(direct_ASD) <- c('dir_betas', 'ses', 'N', 'cohort')
write.csv(direct_ASD, "results/03_RQ/post_mediated/results_ASD_dir_ppdmed_model_ECCN_0_10.csv")

# total effect
#GenR
TE_genr_est <- output_mediation_genr['Total Effect', 'Estimate']
TE_genr_upper <- output_mediation_genr['Total Effect', '95% CI Upper']
TE_genr_lower <- output_mediation_genr['Total Effect', '95% CI Lower']

TE_genr_SE <- (TE_genr_upper - TE_genr_lower)/3.92

TE_all_est <- rbind(TE_genr_est)
TE_all_SE <- rbind(TE_genr_SE)

total_ASD <- as.data.frame(cbind(TE_all_est,TE_all_SE,all_N))
total_ASD$cohort <- c('genr')
colnames(total_ASD) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_ASD, "results/03_RQ/post_mediated/results_ASD_tot_ppdmed_model_ECCN_0_10.csv")

#GenR
PM_genr_est <- output_mediation_genr['Prop. Mediated', 'Estimate']
PM_genr_upper <- output_mediation_genr['Prop. Mediated', '95% CI Upper']
PM_genr_lower <- output_mediation_genr['Prop. Mediated', '95% CI Lower']

PM_genr_SE <- (PM_genr_upper - PM_genr_lower)/3.92

PM_all_est <- rbind(PM_genr_est)
PM_all_SE <- rbind(PM_genr_SE)

total_ASD_PM <- as.data.frame(cbind(PM_all_est,PM_all_SE,all_N))
total_ASD_PM$cohort <- c('genr')
colnames(total_ASD_PM) <- c('tot_betas', 'ses', 'N', 'cohort')
write.csv(total_ASD_PM, "results/03_RQ/post_mediated/results_ASD_PM_ppdmed_model_ECCN_0_10.csv")
#----------------------------12) Cumulative models------------------------------
#-I) Fit linear mixed model with cumul_dep_weighted as exposure and internalising symptoms 
# as outcomes, correcting for covariates income, maternal age at birth, prepregnancy BMI,
# maternal alcohol use and smoking during pregnancy and country of birth of the mother
# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for covariates income, maternal age at birth, prepregnancy BMI, 
# maternal alcohol use, and smoking during pregnancy.

run_cumul_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs for preg_dep
  extract_results <- function(output, cohort_names, var = "cumul_dep_weighted") {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!var %in% colnames(betas)) {
      stop(paste0("Column '", var, "' not found in beta matrix — check model output."))
    }
    
    df <- data.frame(
      cohort = cohort_names,
      cumul_dep_betas = betas[, var],
      ses = ses[, var],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Helper functions to get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) output[[paste0("study", i)]]$Nvalid)
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Build formula
  base_formula <- paste0(
    outcome, " ~ cumul_dep_weighted + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + prepreg_BMI + preg_alc + preg_smk"
  )
  
  results_all <- list()
  
  # LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(base_formula, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    
    output_lmm <- model_lmm$output.summary
    df_lmm <- extract_results(output_lmm, lmm_cohorts, var = "cumul_dep_weighted")
    Ns_lmm <- get_N_counts_lmm(output_lmm, lmm_cohorts)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = base_formula,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    
    output_glm <- model_glm$output.summary
    df_glm <- extract_results(output_glm, glm_cohorts, var = "cumul_dep_weighted")
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine and export
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

models_cumul <- list(
  list(outcome = "int_pc_", age_var = "int_age_",  lmm = c('genr','dnbc'), glm = c('ninfea'),
       file = "results/03_RQ/cumulative/results_int_cumul_model_ECCN.csv"),
  list(outcome = "ext_pc_", age_var = "ext_age_",  lmm = c('genr','dnbc'), glm = c('ninfea'),
       file = "results/03_RQ/cumulative/results_ext_cumul_model_ECCN.csv"),
  list(outcome = "adhd_pc_", age_var = "adhd_age_",  lmm = c('dnbc', 'ninfea'), glm = c('genr'),
       file = "results/03_RQ/cumulative/results_adhd_cumul_model_ECCN.csv"),
  list(outcome = "asd_pc_", age_var = "asd_age_",  glm = c('genr'),
       file = "results/03_RQ/cumulative/results_asd_cumul_model_ECCN.csv")
)

results_cumul <- lapply(models_cumul, function(m) {
  run_cumul_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    output_file = m$file
  )
})

#---------------------------13) Continuous exposure-----------------------------
#-I) Fit linear mixed model with preg_dep_std_continuous as exposure and internalising symptoms 
# as outcomes, correcting for covariates income, maternal age at birth, prepregnancy BMI,
# maternal alcohol use and smoking during pregnancy and country of birth of the mother
# Fit linear mixed model (cohorts with multiple time points) or linear 
# regressions (cohorts with single outcomes) with preg_dep as exposure and 8 outcomes, 
# correcting for covariates income, maternal age at birth, prepregnancy BMI, 
# maternal alcohol use, and smoking during pregnancy.

run_cont_model <- function(
    outcome,
    age_var,
    lmm_cohorts = NULL,
    glm_cohorts = NULL,
    output_file,
    data_lmm = "df_basic",
    data_glm = "df_basic"
) {
  # Helper to extract betas + SEs for preg_dep
  extract_results <- function(output, cohort_names, var = "preg_dep_std_continuous") {
    betas <- t(as.matrix(output[["input.beta.matrix.for.SLMA"]]))
    ses   <- t(as.matrix(output[["input.se.matrix.for.SLMA"]]))
    
    if (!var %in% colnames(betas)) {
      stop(paste0("Column '", var, "' not found in beta matrix — check model output."))
    }
    
    df <- data.frame(
      cohort = cohort_names,
      preg_dep_betas = betas[, var],
      ses = ses[, var],
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Helper functions to get N counts
  get_N_counts_lmm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) {
      lines <- capture.output(output[[paste0("study", i)]])
      line <- grep("groups: +child_id", lines, value = TRUE)
      n <- as.numeric(sub(".*groups: +child_id, +([0-9]+).*", "\\1", line))
      n
    })
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  get_N_counts_glm <- function(output, cohorts) {
    Ns <- sapply(seq_along(cohorts), function(i) output[[paste0("study", i)]]$Nvalid)
    data.frame(cohort = cohorts, N = Ns, stringsAsFactors = FALSE)
  }
  
  # Build formula
  base_formula <- paste0(
    outcome, " ~ preg_dep_std_continuous + sex + ", age_var,
    " + edu_m_.0 + agebirth_m_y + prepreg_BMI + preg_alc + preg_smk"
  )
  
  results_all <- list()
  
  # LMM
  if (!is.null(lmm_cohorts)) {
    model_lmm <- ds.lmerSLMA(
      dataName = data_lmm,
      formula = paste0(base_formula, " + (1|child_id)"),
      REML = TRUE,
      datasources = connections[lmm_cohorts]
    )
    
    output_lmm <- model_lmm$output.summary
    df_lmm <- extract_results(output_lmm, lmm_cohorts, var = "preg_dep_std_continuous")
    Ns_lmm <- get_N_counts_lmm(output_lmm, lmm_cohorts)
    df_lmm <- merge(df_lmm, Ns_lmm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_lmm))
  }
  
  # GLM
  if (!is.null(glm_cohorts)) {
    model_glm <- ds.glmSLMA(
      dataName = data_glm,
      formula = base_formula,
      family = "gaussian",
      combine.with.metafor = TRUE,
      datasources = connections[glm_cohorts]
    )
    
    output_glm <- model_glm$output.summary
    df_glm <- extract_results(output_glm, glm_cohorts, var = "preg_dep_std_continuous")
    Ns_glm <- get_N_counts_glm(output_glm, glm_cohorts)
    df_glm <- merge(df_glm, Ns_glm, by = "cohort", all.x = TRUE)
    results_all <- append(results_all, list(df_glm))
  }
  
  # Combine and export
  results_total <- do.call(rbind, results_all)
  rownames(results_total) <- results_total$cohort
  
  write.csv(results_total, output_file, row.names = FALSE)
  cat("Saved:", output_file, "\n")
  
  return(results_total)
}

models_cont <- list(
  list(outcome = "int_pc_", age_var = "int_age_",  lmm = c('genr', 'eden', 'abcd'),
       file = "results/sensitivity/continuous/results_int_cont_model_ECCN.csv"),
  list(outcome = "ext_pc_", age_var = "ext_age_",  lmm = c('genr', 'eden', 'abcd'),
       file = "results/sensitivity/continuous/results_ext_cont_model_ECCN.csv"),
  list(outcome = "adhd_pc_", age_var = "adhd_age_",  lmm = c('eden', 'abcd'), glm = c('genr'),
       file = "results/sensitivity/continuous/results_adhd_cont_model_ECCN.csv"),
  list(outcome = "asd_pc_", age_var = "asd_age_",  glm = c('genr'),
       file = "results/sensitivity/continuous/results_asd_cont_model_ECCN.csv"),
  list(outcome = "fm_pc_", age_var = "fm_age_",  lmm = c('genr', 'eden'),
       file = "results/sensitivity/continuous/results_fm_cont_model_ECCN.csv"),
  list(outcome = "gm_pc_", age_var = "gm_age_",  lmm = c('genr', 'eden'),
       file = "results/sensitivity/continuous/results_gm_cont_model_ECCN.csv"),
  list(outcome = "lan_pc_", age_var = "lan_age_",  lmm = c('eden'), glm = c('genr'),
       file = "results/sensitivity/continuous/results_lan_cont_model_ECCN.csv"),
  list(outcome = "nvi_pc_", age_var = "nvi_age_",  lmm = c('eden'), glm = c('genr', 'abcd'),
       file = "results/sensitivity/continuous/results_nvi_cont_model_ECCN.csv")
)

results_cont <- lapply(models_cont, function(m) {
  run_cont_model(
    outcome = m$outcome,
    age_var = m$age_var,
    lmm_cohorts = m$lmm,
    glm_cohorts = m$glm,
    output_file = m$file
  )
})
#----------------------------14) Model diagnostics------------------------------
# first convert any multi-categorical factors to dummy variables and convert all factors to numerics 
# (otherwise ds.make later on does not work properly with factors)
ds.asFactor('df_basic$edu_m_.0', fixed.dummy.vars = TRUE, newobj='edu_m_d',
            datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.class('edu_m_d', 
         datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')]) # object is a matrix
ds.colnames('edu_m_d',
            datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.dataFrame('edu_m_d', newobj = 'edu_m_d',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')]) # convert matrix to a dataframe
ds.make('edu_m_d$DV2', newobj='edu_m_d.2',
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')]) # rename DVs to a better name just in case you have multiple multi-level factors
ds.make('edu_m_d$DV3', newobj='edu_m_d.3',
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])

ds.asNumeric('df_basic$preg_dep', newobj='preg_dep.n', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.asNumeric('df_basic$sex', newobj='sex.n', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.asNumeric('df_basic$preg_alc', newobj='preg_alc.n', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.asNumeric('df_basic$preg_smk', newobj='preg_smk.n', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])

#------------------------------14a) Internalising-------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$int_pc_','df_basic$int_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D2', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.dim('df_basic', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.dim('D2', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
ds.colnames('D2', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])

model1_int.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep.n + sex.n + int_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')]
)

model1_int.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep.n + sex.n + int_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('ninfea')])

# use ds.make to create predicted values for each study separately
model1_int.fit$output.summary$study1
ds.make(toAssign = '60.25282 + (11.92589 * D2$preg_dep.n) + (-1.05275 * D2$sex.n) + 
        (0.15847 * D2$int_age_) + (1.70681 * D2$edu_m_d.2) + (4.61034 * D2$edu_m_d.3) +
        (-0.36199 * D2$agebirth_m_y) + (-0.07932 * D2$preg_alc.n) + 
        (0.70880 * D2$preg_smk.n) + (0.13496 * D2$prepreg_BMI)', newobj='pred',
        datasources = connections['genr'])

model1_int.fit$output.summary$study2
ds.make(toAssign = '29.43884 + (11.23675 * D2$preg_dep.n) + (5.96748 * D2$sex.n) + 
        (0.30280 * D2$int_age_) + (3.96198 * D2$edu_m_d.2) + (5.81678 * D2$edu_m_d.3) +
        (-0.44277 * D2$agebirth_m_y) + (-1.13723 * D2$preg_alc.n) + 
        (3.08232 * D2$preg_smk.n) + (0.54788 * D2$prepreg_BMI)', newobj='pred',
        datasources = connections['dnbc'])

model1_int.fit$output.summary$study3
ds.make(toAssign = '60.4863 + (7.6033 * D2$preg_dep.n) + (1.7890 * D2$sex.n) + 
        (0.1720 * D2$int_age_) + (4.0863 * D2$edu_m_d.2) + (7.9433 * D2$edu_m_d.3) +
        (-0.4435 * D2$agebirth_m_y) + (1.2549 * D2$preg_alc.n) + 
        (1.5793 * D2$preg_smk.n) + (-0.1579 * D2$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

model1_int.fit$output.summary$study4
ds.make(toAssign = '32.0592 + (2.6688 * D2$preg_dep.n) + (1.0885 * D2$sex.n) + 
        (0.7419 * D2$int_age_) + (1.7788 * D2$edu_m_d.2) + (4.9267 * D2$edu_m_d.3) +
        (-0.2578 * D2$agebirth_m_y) + (-1.7032 * D2$preg_alc.n) + 
        (3.6299 * D2$preg_smk.n) + (0.2312 * D2$prepreg_BMI)', newobj='pred',
        datasources = connections['abcd'])

ds.glmPredict(glmname = 'model1_int.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('ninfea')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('ninfea')])
ds.make(toAssign = 'D2$int_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea','abcd')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_int_ECCN.jpeg",
     width=600, height=700, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea','abcd')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals internalising model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_int_ECCN.jpeg",
     width=600, height=700, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea','abcd')])
grid.text("Histogram residuals internalising model", .5, .98, gp=gpar(cex=1))
dev.off()

# Note that the graphical functions in DataSHIELD anonymise the actual values.
# see some information here: https://epjdatascience.springeropen.com/articles/10.1140/epjds/s13688-020-00257-4

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_int.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + sex + int_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_int.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + sex + int_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('ninfea')])

L0_genr <- as.numeric(model1_int.fit$output.summary$study1[[14]]['logLik'])
L0_dnbc <- as.numeric(model1_int.fit$output.summary$study2[[14]]['logLik'])
L0_eden <- as.numeric(model1_int.fit$output.summary$study3[[14]]['logLik'])

model1_int_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep*sex + int_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_int_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep*sex + int_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int_sexint.glm.results',
  datasources = connections[c('ninfea')])

L1_genr <- as.numeric(model1_int_sexint.fit$output.summary$study1[[14]]['logLik'])
L1_dnbc <- as.numeric(model1_int_sexint.fit$output.summary$study2[[14]]['logLik'])
L1_eden <- as.numeric(model1_int_sexint.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L1_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L1_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_int.glm.fit$output.summary$study1$deviance.resid - model1_int_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_int_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_int_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_int_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep*int_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_int_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep*int_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int_ageint.glm.results',
  datasources = connections[c('ninfea')])

L2_genr <- as.numeric(model1_int_ageint.fit$output.summary$study1[[14]]['logLik'])
L2_dnbc <- as.numeric(model1_int_ageint.fit$output.summary$study2[[14]]['logLik'])
L2_eden <- as.numeric(model1_int_ageint.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L2_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L2_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_int.glm.fit$output.summary$study1$deviance.resid - model1_int_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_int_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_int_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$int_age_ * df_basic$int_age_', newobj = 'int_age_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'int_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# use ds.make to create the residuals
model1_int_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + int_age_ + int_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_int_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + int_age_ + int_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int_nonlin.glm.results',
  datasources = connections[c('ninfea')])

L3_genr <- as.numeric(model1_int_nonlin.fit$output.summary$study1[[14]]['logLik'])
L3_dnbc <- as.numeric(model1_int_nonlin.fit$output.summary$study2[[14]]['logLik'])
L3_eden <- as.numeric(model1_int_nonlin.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L3_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L3_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_int.glm.fit$output.summary$study1$deviance.resid - model1_int_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_int_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_int_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# use ds.make to create the residuals
model1_int_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + int_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_int_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + int_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int_nonlin.glm.results',
  datasources = connections[c('ninfea')])

L4_genr <- as.numeric(model1_int_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])
L4_dnbc <- as.numeric(model1_int_nonlin_BMI.fit$output.summary$study2[[14]]['logLik'])
L4_eden <- as.numeric(model1_int_nonlin_BMI.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L4_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L4_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_int.glm.fit$output.summary$study1$deviance.resid - model1_int_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_int_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_int_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------------ IIe) Random slope-------------------------------
model1_int_slope.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "int_pc_ ~ preg_dep + sex + int_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1 + int_age_|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

L4_genr <- as.numeric(model1_int_slope.fit$output.summary$study1[[14]]['logLik'])
L4_dnbc <- as.numeric(model1_int_slope.fit$output.summary$study2[[14]]['logLik'])
L4_eden <- as.numeric(model1_int_slope.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L4_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L4_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))


#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_int.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_int.fit$output.summary$study2$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_dnbc <- diag(solve(v/(d %o% d)))
names(vif_dnbc) <- nam
vif_dnbc

v <- model1_int.fit$output.summary$study3$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_int.fit$output.summary$study4$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_abcd <- diag(solve(v/(d %o% d)))
names(vif_abcd) <- nam
vif_abcd

v <- model1_int.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_ninfea <- diag(solve(v/(d %o% d)))
names(vif_ninfea) <- nam
vif_ninfea

#------------------------------14b) Externalising-------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$ext_pc_','df_basic$ext_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D3', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
ds.dim('df_basic', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
ds.dim('D3', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
ds.colnames('D3', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

model1_ext.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep.n + sex.n + ext_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = TRUE,
  datasources = connections[c('genr', 'dnbc', 'eden', 'abcd')]
)

model1_ext.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep.n + sex.n + ext_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_ext.glm.results',
  datasources = connections[c('ninfea')])

# use ds.make to create predicted values for each study separately
model1_ext.fit$output.summary$study1
ds.make(toAssign = '61.13469 + (10.21891 * D3$preg_dep.n) + (-6.09466 * D3$sex.n) + 
        (0.51706 * D3$ext_age_) + (2.67968 * D3$edu_m_d.2) + (3.67934 * D3$edu_m_d.3) +
        (-0.35035 * D3$agebirth_m_y) + (2.46947 * D3$preg_alc.n) + 
        (3.13020 * D3$preg_smk.n) + (0.20870 * D3$prepreg_BMI)', newobj='pred',
        datasources = connections['genr'])

model1_ext.fit$output.summary$study2
ds.make(toAssign = '42.49514 + (4.96924 * D3$preg_dep.n) + (-5.84961 * D3$sex.n) + 
        (0.20581 * D3$ext_age_) + (6.54944 * D3$edu_m_d.2) + (7.15619 * D3$edu_m_d.3) +
        (-0.27483 * D3$agebirth_m_y) + (0.51312 * D3$preg_alc.n) + 
        (5.94478 * D3$preg_smk.n) + (0.52730 * D3$prepreg_BMI)', newobj='pred',
        datasources = connections['dnbc'])

model1_ext.fit$output.summary$study3
ds.make(toAssign = '61.4307 + (4.2339 * D3$preg_dep.n) + (-8.7395 * D3$sex.n) + 
        (0.2672 * D3$ext_age_) + (8.6974 * D3$edu_m_d.2) + (13.7156 * D3$edu_m_d.3) +
        (-0.3899 * D3$agebirth_m_y) + (2.0162 * D3$preg_alc.n) + 
        (4.4891 * D3$preg_smk.n) + (0.2701 * D3$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

model1_ext.fit$output.summary$study4
ds.make(toAssign = '38.34724 + (1.86103 * D3$preg_dep.n) + (-4.44341 * D3$sex.n) + 
        (-0.41828 * D3$ext_age_) + (0.14690 * D3$edu_m_d.2) + (3.18191 * D3$edu_m_d.3) +
        (0.09006 * D3$agebirth_m_y) + (-0.87340 * D3$preg_alc.n) + 
        (0.86821 * D3$preg_smk.n) + (0.13378 * D3$prepreg_BMI)', newobj='pred',
        datasources = connections['abcd'])

ds.glmPredict(glmname = 'model1_ext.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('ninfea')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('ninfea')])
ds.make(toAssign = 'D3$ext_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_ext_ECCN.jpeg",
     width=600, height=700, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals externalising model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_ext_ECCN.jpeg",
     width=600, height=700, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
grid.text("Histogram residuals externalising model", .5, .98, gp=gpar(cex=1))
dev.off()

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_ext.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + sex + ext_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_ext.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + sex + ext_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('ninfea')])

L0_genr <- as.numeric(model1_ext.fit$output.summary$study1[[14]]['logLik'])
L0_dnbc <- as.numeric(model1_ext.fit$output.summary$study2[[14]]['logLik'])
L0_eden <- as.numeric(model1_ext.fit$output.summary$study3[[14]]['logLik'])

model1_ext_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep*sex + ext_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_ext_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep*sex + ext_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_ext_sexint.glm.results',
  datasources = connections[c('ninfea')])

L1_genr <- as.numeric(model1_ext_sexint.fit$output.summary$study1[[14]]['logLik'])
L1_dnbc <- as.numeric(model1_ext_sexint.fit$output.summary$study2[[14]]['logLik'])
L1_eden <- as.numeric(model1_ext_sexint.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L1_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L1_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_ext.glm.fit$output.summary$study1$deviance.resid - model1_ext_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_ext_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_ext_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_ext_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep*ext_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_ext_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep*ext_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_ext_ageint.glm.results',
  datasources = connections[c('ninfea')])

L2_genr <- as.numeric(model1_ext_ageint.fit$output.summary$study1[[14]]['logLik'])
L2_dnbc <- as.numeric(model1_ext_ageint.fit$output.summary$study2[[14]]['logLik'])
L2_eden <- as.numeric(model1_ext_ageint.fit$output.summary$study3[[14]]['logLik'])
L2_ninfea <- (model1_ext_ageint.glm.fit$output.summary$study1$deviance.resid / -2)

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L2_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L2_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_ext.glm.fit$output.summary$study1$deviance.resid - model1_ext_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_ext_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_ext_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$ext_age_ * df_basic$ext_age_', newobj = 'ext_age_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'ext_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_ext_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + ext_age_ + ext_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_ext_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + ext_age_ + ext_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_ext_nonlin.glm.results',
  datasources = connections[c('ninfea')])

L3_genr <- as.numeric(model1_ext_nonlin.fit$output.summary$study1[[14]]['logLik'])
L3_dnbc <- as.numeric(model1_ext_nonlin.fit$output.summary$study2[[14]]['logLik'])
L3_eden <- as.numeric(model1_ext_nonlin.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L3_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L3_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_ext.glm.fit$output.summary$study1$deviance.resid - model1_ext_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_ext_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_ext_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_q', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_q'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_ext_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + ext_age_ + prepreg_BMI_q + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

model1_ext_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + ext_age_ + prepreg_BMI_q + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_ext_nonlin.glm.results',
  datasources = connections[c('ninfea')])

L4_genr <- as.numeric(model1_ext_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])
L4_dnbc <- as.numeric(model1_ext_nonlin_BMI.fit$output.summary$study2[[14]]['logLik'])
L4_eden <- as.numeric(model1_ext_nonlin_BMI.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L4_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L4_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
dev.diff <- model1_ext.glm.fit$output.summary$study1$deviance.resid - model1_ext_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_ext_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_ext_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------------ IIe) Random slope-------------------------------
model1_ext_slope.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "ext_pc_ ~ preg_dep + sex + ext_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1 + ext_age_|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'dnbc', 'eden')]
)

L4_genr <- as.numeric(model1_ext_slope.fit$output.summary$study1[[14]]['logLik'])
L4_dnbc <- as.numeric(model1_ext_slope.fit$output.summary$study2[[14]]['logLik'])
L4_eden <- as.numeric(model1_ext_slope.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L4_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L4_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_ext.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_ext.fit$output.summary$study2$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_dnbc <- diag(solve(v/(d %o% d)))
names(vif_dnbc) <- nam
vif_dnbc

v <- model1_ext.fit$output.summary$study3$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_ext.fit$output.summary$study4$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_abcd <- diag(solve(v/(d %o% d)))
names(vif_abcd) <- nam
vif_abcd

v <- model1_ext.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_ninfea <- diag(solve(v/(d %o% d)))
names(vif_ninfea) <- nam
vif_ninfea

#------------------------------14c) ADHD symptoms-------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$adhd_pc_','df_basic$adhd_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D4', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
ds.dim('df_basic', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
ds.dim('D4', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])
ds.colnames('D4', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

model1_adhd.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep.n + sex.n + adhd_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = TRUE,
  datasources = connections[c('ninfea', 'dnbc', 'eden', 'abcd')]
)

model1_adhd.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep.n + sex.n + adhd_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_adhd.glm.results',
  datasources = connections[c('genr')])

# use ds.make to create predicted values for each study separately
model1_adhd.fit$output.summary$study1
ds.make(toAssign = '56.94249 + (9.36268 * D4$preg_dep.n) + (-8.23868 * D4$sex.n) + 
        (-0.33549 * D4$adhd_age_) + (5.44721 * D4$edu_m_d.2) + (11.00661 * D4$edu_m_d.3) +
        (-0.29274 * D4$agebirth_m_y) + (5.19335 * D4$preg_alc.n) + 
        (6.89789 * D4$preg_smk.n) + (0.32602 * D4$prepreg_BMI)', newobj='pred',
        datasources = connections['ninfea'])

model1_adhd.fit$output.summary$study2
ds.make(toAssign = '41.96444 + (4.40011 * D4$preg_dep.n) + (-6.87719 * D4$sex.n) + 
        (0.30512 * D4$adhd_age_) + (5.92475 * D4$edu_m_d.2) + (6.51170 * D4$edu_m_d.3) +
        (-0.25975 * D4$agebirth_m_y) + (0.66568 * D4$preg_alc.n) + 
        (5.64642 * D4$preg_smk.n) + (0.48005 * D4$prepreg_BMI)', newobj='pred',
        datasources = connections['dnbc'])

model1_adhd.fit$output.summary$study3
ds.make(toAssign = '63.0608 + (1.5127 * D4$preg_dep.n) + (-8.0538 * D4$sex.n) + 
        (0.2885 * D4$adhd_age_) + (10.2556 * D4$edu_m_d.2) + (13.1888 * D4$edu_m_d.3) +
        (-0.5893 * D4$agebirth_m_y) + (1.3015 * D4$preg_alc.n) + 
        (3.9413 * D4$preg_smk.n) + (0.4214 * D4$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

model1_adhd.fit$output.summary$study4
ds.make(toAssign = '24.46298 + (-0.08781 * D4$preg_dep.n) + (-4.94288 * D4$sex.n) + 
        (-0.23796 * D4$adhd_age_) + (1.70393 * D4$edu_m_d.2) + (-0.04523 * D4$edu_m_d.3) +
        (0.03634 * D4$agebirth_m_y) + (-0.72706 * D4$preg_alc.n) + 
        (2.65310 * D4$preg_smk.n) + (0.37710 * D4$prepreg_BMI)', newobj='pred',
        datasources = connections['abcd'])

ds.glmPredict(glmname = 'model1_adhd.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('genr')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('genr')])
ds.make(toAssign = 'D4$adhd_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea', 'abcd')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_adhd_ECCN.jpeg",
     width=600, height=700, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals ADHD model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_adhd_ECCN.jpeg",
     width=600, height=700, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea',  'abcd')])
grid.text("Histogram residuals ADHD model", .5, .98, gp=gpar(cex=1))
dev.off()

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_adhd.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + sex + adhd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('dnbc', 'eden', 'ninfea')]
)

model1_adhd.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + sex + adhd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('genr')])

L0_dnbc <- as.numeric(model1_adhd.fit$output.summary$study1[[14]]['logLik'])
L0_eden <- as.numeric(model1_adhd.fit$output.summary$study2[[14]]['logLik'])
L0_ninfea <- as.numeric(model1_adhd.fit$output.summary$study3[[14]]['logLik'])

model1_adhd_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep*sex + adhd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('dnbc', 'eden', 'ninfea')]
)

model1_adhd_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep*sex + adhd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_adhd_sexint.glm.results',
  datasources = connections[c('genr')])

L1_dnbc <- as.numeric(model1_adhd_sexint.fit$output.summary$study1[[14]]['logLik'])
L1_eden <- as.numeric(model1_adhd_sexint.fit$output.summary$study2[[14]]['logLik'])
L1_ninfea <- as.numeric(model1_adhd_sexint.fit$output.summary$study3[[14]]['logLik'])

# GenR
dev.diff <- model1_adhd.glm.fit$output.summary$study1$deviance.resid - model1_adhd_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_adhd_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_adhd_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L1_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L1_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

#--------------------------- IIb) Interaction with age--------------------------
model1_adhd_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep*adhd_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('dnbc', 'eden', 'ninfea')]
)

model1_adhd_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep*adhd_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_adhd_ageint.glm.results',
  datasources = connections[c('genr')])

L2_dnbc <- as.numeric(model1_adhd_ageint.fit$output.summary$study1[[14]]['logLik'])
L2_eden <- as.numeric(model1_adhd_ageint.fit$output.summary$study2[[14]]['logLik'])
L2_ninfea <- as.numeric(model1_adhd_ageint.fit$output.summary$study3[[14]]['logLik'])

# GenR
dev.diff <- model1_adhd.glm.fit$output.summary$study1$deviance.resid - model1_adhd_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_adhd_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_adhd_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L2_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L2_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$adhd_age_ * df_basic$adhd_age_', newobj = 'adhd_age_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'adhd_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# use ds.make to create the residuals
model1_adhd_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + adhd_age_ + adhd_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('dnbc', 'eden', 'ninfea')]
)

model1_adhd_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + adhd_age_ + adhd_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_adhd_nonlin.glm.results',
  datasources = connections[c('genr')])

L3_dnbc <- as.numeric(model1_adhd_nonlin.fit$output.summary$study1[[14]]['logLik'])
L3_eden <- as.numeric(model1_adhd_nonlin.fit$output.summary$study2[[14]]['logLik'])
L3_ninfea <- as.numeric(model1_adhd_nonlin.fit$output.summary$study3[[14]]['logLik'])

# GenR
dev.diff <- model1_adhd.glm.fit$output.summary$study1$deviance.resid - model1_adhd_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_adhd_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_adhd_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L3_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L3_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# use ds.make to create the residuals
model1_adhd_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + adhd_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('dnbc', 'eden', 'ninfea')]
)

model1_adhd_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + adhd_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_adhd_nonlin.glm.results',
  datasources = connections[c('genr')])

L4_dnbc <- as.numeric(model1_adhd_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])
L4_eden <- as.numeric(model1_adhd_nonlin_BMI.fit$output.summary$study2[[14]]['logLik'])
L4_ninfea <- as.numeric(model1_adhd_nonlin_BMI.fit$output.summary$study3[[14]]['logLik'])

# GenR
dev.diff <- model1_adhd.glm.fit$output.summary$study1$deviance.resid - model1_adhd_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_adhd_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_adhd_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L4_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L4_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))


#------------------------------ IIe) Random slope-------------------------------
model1_adhd_slope.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "adhd_pc_ ~ preg_dep + sex + adhd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1 + adhd_age_|child_id)", 
  REML = FALSE,
  datasources = connections[c('dnbc', 'eden', 'ninfea')]
)

L4_dnbc <- as.numeric(model1_adhd_slope.fit$output.summary$study1[[14]]['logLik'])
L4_eden <- as.numeric(model1_adhd_slope.fit$output.summary$study2[[14]]['logLik'])
L4_ninfea <- as.numeric(model1_adhd_slope.fit$output.summary$study3[[14]]['logLik'])

# DNBC
Chisq <- -2*(as.numeric(L0_dnbc)-as.numeric(L4_dnbc))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L4_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))


#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_adhd.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_adhd.fit$output.summary$study2$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_dnbc <- diag(solve(v/(d %o% d)))
names(vif_dnbc) <- nam
vif_dnbc

v <- model1_adhd.fit$output.summary$study3$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_adhd.fit$output.summary$study4$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_abcd <- diag(solve(v/(d %o% d)))
names(vif_abcd) <- nam
vif_abcd

v <- model1_adhd.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_ninfea <- diag(solve(v/(d %o% d)))
names(vif_ninfea) <- nam
vif_ninfea

#------------------------------14d) ASD symptoms-------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$asd_pc_','df_basic$asd_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D5', 
             datasources = connections[c('genr')])
ds.dim('df_basic', datasources = connections[c('genr')])
ds.dim('D5', datasources = connections[c('genr')])
ds.colnames('D5', datasources = connections[c('genr')])

model1_asd.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "asd_pc_ ~ preg_dep.n + sex.n + asd_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_asd.glm.results',
  datasources = connections[c('genr')])

ds.glmPredict(glmname = 'model1_asd.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('genr')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('genr')])
ds.make(toAssign = 'D5$asd_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_asd_ECCN.jpeg",
     width=500, height=400, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals ASD model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_asd_ECCN.jpeg",
     width=500, height=400, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr')])
grid.text("Histogram residuals ASD model", .5, .98, gp=gpar(cex=1))
dev.off()

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_asd.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "asd_pc_ ~ preg_dep + sex + asd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('genr')])

model1_asd_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "asd_pc_ ~ preg_dep*sex + asd_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_asd_sexint.glm.results',
  datasources = connections[c('genr')])

# GenR
dev.diff <- model1_asd.glm.fit$output.summary$study1$deviance.resid - model1_asd_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_asd_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_asd_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_asd_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "asd_pc_ ~ preg_dep*asd_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_asd_ageint.glm.results',
  datasources = connections[c('genr')])

# GenR
dev.diff <- model1_asd.glm.fit$output.summary$study1$deviance.resid - model1_asd_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_asd_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_asd_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$asd_age_ * df_basic$asd_age_', newobj = 'asd_age_sq', 
        datasources = connections[c('genr')])

ds.dataFrame(c('df_basic', 'asd_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr')])

model1_asd_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "asd_pc_ ~ preg_dep + asd_age_ + asd_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_asd_nonlin.glm.results',
  datasources = connections[c('genr')])

# GenR
dev.diff <- model1_asd.glm.fit$output.summary$study1$deviance.resid - model1_asd_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_asd_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_asd_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr')])

model1_asd_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "asd_pc_ ~ preg_dep + asd_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_asd_nonlin.glm.results',
  datasources = connections[c('genr')])

# GenR
dev.diff <- model1_asd.glm.fit$output.summary$study1$deviance.resid - model1_asd_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_asd_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_asd_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_asd.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

#--------------------------------14e) FM skills---------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n','df_basic$fm_pc_','df_basic$fm_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D6', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.dim('df_basic', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.dim('D6', datasources = connections[c('genr', 'eden')])
ds.colnames('D6', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_fm.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep.n + sex.n + fm_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = TRUE,
  datasources = connections[c('genr', 'eden')]
)

model1_fm.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep.n + sex.n + fm_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_fm.glm.results',
  datasources = connections[c('dnbc', 'ninfea')])

# use ds.make to create predicted values for each study separately
model1_fm.fit$output.summary$study1
ds.make(toAssign = '48.452724 + (-1.309096 * D6$preg_dep.n) + (3.933432 * D6$sex.n) + 
        (8.524187 * D6$fm_age_) + (0.842774 * D6$edu_m_d.2) + (2.608357 * D6$edu_m_d.3) +
        (0.006963 * D6$agebirth_m_y) + (-1.829059 * D6$preg_alc.n) + 
        (0.316878 * D6$preg_smk.n) + (-0.061606 * D6$prepreg_BMI)', newobj='pred',
        datasources = connections['genr'])

model1_fm.fit$output.summary$study2
ds.make(toAssign = '39.06527 + (1.40134 * D6$preg_dep.n) + (9.99957 * D6$sex.n) + 
        (0.02609 * D6$fm_age_) + (-4.65881 * D6$edu_m_d.2) + (-7.06838 * D6$edu_m_d.3) +
        (0.03022 * D6$agebirth_m_y) + (-1.05010 * D6$preg_alc.n) + 
        (-0.05829 * D6$preg_smk.n) + (-0.08279 * D6$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

ds.glmPredict(glmname = 'model1_fm.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('dnbc', 'ninfea')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('dnbc', 'ninfea')])
ds.make(toAssign = 'D6$fm_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_fm_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals FM model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_fm_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
grid.text("Histogram residuals FM model", .5, .98, gp=gpar(cex=1))
dev.off()

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_fm.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep.n + sex.n + fm_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden')]
)

model1_fm.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep + sex + fm_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('dnbc', 'ninfea')])

L0_genr <- as.numeric(model1_fm.fit$output.summary$study1[[14]]['logLik'])
L0_eden <- as.numeric(model1_fm.fit$output.summary$study2[[14]]['logLik'])

model1_fm_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep*sex + fm_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden')]
)

model1_fm_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep*sex + fm_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_fm_sexint.glm.results',
  datasources = connections[c('dnbc', 'ninfea')])

L1_genr <- as.numeric(model1_fm_sexint.fit$output.summary$study1[[14]]['logLik'])
L1_eden <- as.numeric(model1_fm_sexint.fit$output.summary$study2[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L1_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_fm.glm.fit$output.summary$study1$deviance.resid - model1_fm_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_fm_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_fm.glm.fit$output.summary$study2$deviance.resid - model1_fm_sexint.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_sexint.glm.fit$output.summary$study2$df.resid
scale <- model1_fm_sexint.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_fm_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep*fm_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden')]
)

model1_fm_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep*fm_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_fm_ageint.glm.results',
  datasources = connections[c('dnbc', 'ninfea')])

L2_genr <- as.numeric(model1_fm_ageint.fit$output.summary$study1[[14]]['logLik'])
L2_eden <- as.numeric(model1_fm_ageint.fit$output.summary$study2[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L2_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_fm.glm.fit$output.summary$study1$deviance.resid - model1_fm_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_fm_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_fm.glm.fit$output.summary$study2$deviance.resid - model1_fm_ageint.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_ageint.glm.fit$output.summary$study2$df.resid
scale <- model1_fm_ageint.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p
#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$fm_age_ * df_basic$fm_age_', newobj = 'fm_age_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'fm_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_fm_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep + fm_age_ + fm_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden')]
)

#all children have the same age in DNBC (7)
model1_fm_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep + fm_age_ + fm_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_fm_nonlin.glm.results',
  datasources = connections[c('dnbc')])

L3_genr <- as.numeric(model1_fm_nonlin.fit$output.summary$study1[[14]]['logLik'])
L3_eden <- as.numeric(model1_fm_nonlin.fit$output.summary$study2[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L3_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_fm.glm.fit$output.summary$study1$deviance.resid - model1_fm_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_fm_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_fm.glm.fit$output.summary$study2$deviance.resid - model1_fm_nonlin.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_nonlin.glm.fit$output.summary$study2$df.resid
scale <- model1_fm_nonlin.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_fm_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep + fm_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden')]
)

model1_fm_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "fm_pc_ ~ preg_dep + fm_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_fm_nonlin.glm.results',
  datasources = connections[c('dnbc', 'ninfea')])

L4_genr <- as.numeric(model1_fm_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])
L4_eden <- as.numeric(model1_fm_nonlin_BMI.fit$output.summary$study2[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L4_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_fm.glm.fit$output.summary$study1$deviance.resid - model1_fm_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_fm_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_fm.glm.fit$output.summary$study2$deviance.resid - model1_fm_nonlin_BMI.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_fm_nonlin_BMI.glm.fit$output.summary$study2$df.resid
scale <- model1_fm_nonlin_BMI.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_fm.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_fm.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_dnbc <- diag(solve(v/(d %o% d)))
names(vif_dnbc) <- nam
vif_dnbc

v <- model1_fm.fit$output.summary$study2$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_fm.glm.fit$output.summary$study2$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_ninfea <- diag(solve(v/(d %o% d)))
names(vif_ninfea) <- nam
vif_ninfea

#--------------------------------14f) GM skills---------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$gm_pc_','df_basic$gm_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D7', 
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.dim('df_basic', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
ds.dim('D7', datasources = connections[c('genr', 'eden')])
ds.colnames('D7', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_gm.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep.n + sex.n + gm_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = TRUE,
  datasources = connections[c('genr', 'eden', 'ninfea')]
)

model1_gm.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep.n + sex.n + gm_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_gm.glm.results',
  datasources = connections[c('dnbc')])

# use ds.make to create predicted values for each study separately
model1_gm.fit$output.summary$study1
ds.make(toAssign = '59.96276 + (1.83966 * D7$preg_dep.n) + (-0.57167 * D7$sex.n) + 
        (8.16104 * D7$gm_age_) + (5.18458 * D7$edu_m_d.2) + (7.51464 * D7$edu_m_d.3) +
        (-0.39247 * D7$agebirth_m_y) + (-1.37015 * D7$preg_alc.n) + 
        (3.01197 * D7$preg_smk.n) + (0.04667 * D7$prepreg_BMI)', newobj='pred',
        datasources = connections['genr'])

model1_gm.fit$output.summary$study2
ds.make(toAssign = '58.78130 + (1.45551 * D7$preg_dep.n) + (-0.96597 * D7$sex.n) + 
        (0.15038 * D7$gm_age_) + (-0.70220 * D7$edu_m_d.2) + (-3.31300 * D7$edu_m_d.3) +
        (-0.14504 * D7$agebirth_m_y) + (-1.69352 * D7$preg_alc.n) + 
        (2.23294 * D7$preg_smk.n) + (-0.09297 * D7$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

model1_gm.fit$output.summary$study3
ds.make(toAssign = '40.63780 + (1.63767 * D7$preg_dep.n) + (-2.75509 * D7$sex.n) + 
        (-1.23808 * D7$gm_age_) + (0.54310 * D7$edu_m_d.2) + (1.41809 * D7$edu_m_d.3) +
        (0.39767 * D7$agebirth_m_y) + (-0.53439 * D7$preg_alc.n) + 
        (-3.34333 * D7$preg_smk.n) + (-0.39567 * D7$prepreg_BMI)', newobj='pred',
        datasources = connections['ninfea'])

ds.glmPredict(glmname = 'model1_gm.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('dnbc')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('dnbc')])
ds.make(toAssign = 'D7$gm_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_gm_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals GM model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_gm_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])
grid.text("Histogram residuals GM model", .5, .98, gp=gpar(cex=1))
dev.off()

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_gm.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep.n + sex.n + gm_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden', 'ninfea')]
)

model1_gm.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep + sex + gm_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('dnbc')])

L0_genr <- as.numeric(model1_gm.fit$output.summary$study1[[14]]['logLik'])
L0_eden <- as.numeric(model1_gm.fit$output.summary$study2[[14]]['logLik'])
L0_ninfea <- as.numeric(model1_gm.fit$output.summary$study3[[14]]['logLik'])

model1_gm_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep*sex + gm_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden', 'ninfea')]
)

model1_gm_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep*sex + gm_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_gm_sexint.glm.results',
  datasources = connections[c('dnbc')])

L1_genr <- as.numeric(model1_gm_sexint.fit$output.summary$study1[[14]]['logLik'])
L1_eden <- as.numeric(model1_gm_sexint.fit$output.summary$study2[[14]]['logLik'])
L1_ninfea <- as.numeric(model1_gm_sexint.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L1_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L1_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_gm.glm.fit$output.summary$study1$deviance.resid - model1_gm_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_gm_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_gm_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_gm_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep*gm_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden', 'ninfea')]
)

model1_gm_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep*gm_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_gm_ageint.glm.results',
  datasources = connections[c('dnbc')])

L2_genr <- as.numeric(model1_gm_ageint.fit$output.summary$study1[[14]]['logLik'])
L2_eden <- as.numeric(model1_gm_ageint.fit$output.summary$study2[[14]]['logLik'])
L2_ninfea <- as.numeric(model1_gm_ageint.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L2_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L2_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_gm.glm.fit$output.summary$study1$deviance.resid - model1_gm_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_gm_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_gm_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$gm_age_ * df_basic$gm_age_', newobj = 'gm_age_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'gm_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_gm_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep + gm_age_ + gm_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden', 'ninfea')]
)

model1_gm_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep + gm_age_ + gm_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_gm_nonlin.glm.results',
  datasources = connections[c('dnbc')])

L3_genr <- as.numeric(model1_gm_nonlin.fit$output.summary$study1[[14]]['logLik'])
L3_eden <- as.numeric(model1_gm_nonlin.fit$output.summary$study2[[14]]['logLik'])
L3_ninfea <- as.numeric(model1_gm_nonlin.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L3_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# NINFEA
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L3_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_gm.glm.fit$output.summary$study1$deviance.resid - model1_gm_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_gm_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_gm_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

model1_gm_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep + gm_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('genr', 'eden', 'ninfea')]
)

model1_gm_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "gm_pc_ ~ preg_dep + gm_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_gm_nonlin.glm.results',
  datasources = connections[c('dnbc')])

L4_genr <- as.numeric(model1_gm_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])
L4_eden <- as.numeric(model1_gm_nonlin_BMI.fit$output.summary$study2[[14]]['logLik'])
L4_ninfea <- as.numeric(model1_gm_nonlin_BMI.fit$output.summary$study3[[14]]['logLik'])

# GenR
Chisq <- -2*(as.numeric(L0_genr)-as.numeric(L4_genr))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# EDEN
Chisq <- -2*(as.numeric(L0_ninfea)-as.numeric(L4_ninfea))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# DNBC
dev.diff <- model1_gm.glm.fit$output.summary$study1$deviance.resid - model1_gm_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_gm_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_gm_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_gm.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_gm.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_dnbc <- diag(solve(v/(d %o% d)))
names(vif_dnbc) <- nam
vif_dnbc

v <- model1_gm.fit$output.summary$study2$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_gm.fit$output.summary$study3$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_ninfea <- diag(solve(v/(d %o% d)))
names(vif_ninfea) <- nam
vif_ninfea


#-----------------------------------14g) LAN -----------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$lan_pc_','df_basic$lan_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D8', 
             datasources = connections[c('genr', 'eden', 'ninfea')])
ds.dim('df_basic', datasources = connections[c('genr', 'eden', 'ninfea')])
ds.dim('D8', datasources = connections[c('genr', 'eden', 'ninfea')])
ds.colnames('D8', datasources = connections[c('genr', 'eden', 'ninfea')])

model1_lan.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep.n + sex.n + lan_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = TRUE,
  datasources = connections[c('eden')]
)

model1_lan.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep.n + sex.n + lan_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_lan.glm.results',
  datasources = connections[c('genr', 'ninfea')])

# use ds.make to create predicted values for each study separately
model1_lan.fit$output.summary$study1
ds.make(toAssign = '49.50118 + (0.82164 * D8$preg_dep.n) + (4.95189 * D8$sex.n) + 
        (-0.01565 * D8$lan_age_) + (-5.98016 * D8$edu_m_d.2) + (-10.64063 * D8$edu_m_d.3) +
        (-0.05440 * D8$agebirth_m_y) + (-1.35229 * D8$preg_alc.n) + 
        (0.96919 * D8$preg_smk.n) + (-0.09162 * D8$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

ds.glmPredict(glmname = 'model1_lan.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('genr', 'ninfea')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('genr', 'ninfea')])
ds.make(toAssign = 'D8$lan_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'eden', 'ninfea')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_lan_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'eden', 'ninfea')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals language model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_lan_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'eden', 'ninfea')])
grid.text("Histogram residuals language model", .5, .98, gp=gpar(cex=1))
dev.off()
#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_lan.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep.n + sex.n + lan_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_lan.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep + sex + lan_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('genr', 'ninfea')])

L0_eden <- as.numeric(model1_lan.fit$output.summary$study1[[14]]['logLik'])

model1_lan_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep*sex + lan_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_lan_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep*sex + lan_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_lan_sexint.glm.results',
  datasources = connections[c('genr', 'ninfea')])

L1_eden <- as.numeric(model1_lan_sexint.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_lan.glm.fit$output.summary$study1$deviance.resid - model1_lan_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_lan_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_lan.glm.fit$output.summary$study2$deviance.resid - model1_lan_sexint.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_sexint.glm.fit$output.summary$study2$df.resid
scale <- model1_lan_sexint.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_lan_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep*lan_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_lan_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep*lan_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_lan_ageint.glm.results',
  datasources = connections[c('genr', 'ninfea')])

L2_eden <- as.numeric(model1_lan_ageint.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_lan.glm.fit$output.summary$study1$deviance.resid - model1_lan_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_lan_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_lan.glm.fit$output.summary$study2$deviance.resid - model1_lan_ageint.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_ageint.glm.fit$output.summary$study2$df.resid
scale <- model1_lan_ageint.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$lan_age_ * df_basic$lan_age_', newobj = 'lan_age_sq', 
        datasources = connections[c('genr', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'lan_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'eden', 'ninfea')])

model1_lan_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep + lan_age_ + lan_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_lan_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep + lan_age_ + lan_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_lan_nonlin.glm.results',
  datasources = connections[c('genr', 'ninfea')])

L3_eden <- as.numeric(model1_lan_nonlin.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_lan.glm.fit$output.summary$study1$deviance.resid - model1_lan_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_lan_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_lan.glm.fit$output.summary$study2$deviance.resid - model1_lan_nonlin.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_nonlin.glm.fit$output.summary$study2$df.resid
scale <- model1_lan_nonlin.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'eden', 'ninfea')])

model1_lan_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep + lan_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_lan_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "lan_pc_ ~ preg_dep + lan_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_lan_nonlin.glm.results',
  datasources = connections[c('genr', 'ninfea')])

L4_eden <- as.numeric(model1_lan_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_lan.glm.fit$output.summary$study1$deviance.resid - model1_lan_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_lan_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

# NINFEA
dev.diff <- model1_lan.glm.fit$output.summary$study2$deviance.resid - model1_lan_nonlin_BMI.glm.fit$output.summary$study2$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_lan_nonlin_BMI.glm.fit$output.summary$study2$df.resid
scale <- model1_lan_nonlin_BMI.glm.fit$output.summary$study2$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_lan.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_lan.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_lan.glm.fit$output.summary$study2$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_ninfea <- diag(solve(v/(d %o% d)))
names(vif_ninfea) <- nam
vif_ninfea

#-----------------------------------14h) NVI -----------------------------------
#--------------- I) Heteroscedasticity and normality of residuals---------------
# create complete cases dataframes for the model variables (including random effects)
ds.dataFrame(c('preg_dep.n', 'df_basic$nvi_pc_','df_basic$nvi_age_','sex.n',
               'edu_m_d.2', 'edu_m_d.3', 'preg_alc.n', 'preg_smk.n', 
               'df_basic$agebirth_m_y', 'df_basic$prepreg_BMI'), 
             completeCases = TRUE, newobj = 'D9', 
             datasources = connections[c('genr', 'eden', 'abcd')])
ds.dim('df_basic', datasources = connections[c('genr', 'eden', 'abcd')])
ds.dim('D9', datasources = connections[c('genr', 'eden', 'abcd')])
ds.colnames('D9', datasources = connections[c('genr', 'eden', 'abcd')])

model1_nvi.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep.n + sex.n + nvi_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = TRUE,
  datasources = connections[c('eden')]
)

model1_nvi.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep.n + sex.n + nvi_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_nvi.glm.results',
  datasources = connections[c('genr', 'abcd')])

# use ds.make to create predicted values for each study separately
model1_nvi.fit$output.summary$study1
ds.make(toAssign = '54.560178 + (0.552324 * D9$preg_dep.n) + (2.660687 * D9$sex.n) + 
        (-0.002044 * D9$nvi_age_) + (-9.665908 * D9$edu_m_d.2) + (-15.401538 * D9$edu_m_d.3) +
        (0.104215 * D9$agebirth_m_y) + (-2.531787 * D9$preg_alc.n) + 
        (-1.063021 * D9$preg_smk.n) + (-0.239105 * D9$prepreg_BMI)', newobj='pred',
        datasources = connections['eden'])

ds.glmPredict(glmname = 'model1_nvi.glm.results',
              output.type = "response",
              newobj = 'pred',
              datasources = connections[c('genr', 'abcd')])

# use ds.make to create the residuals
ds.make(toAssign = 'pred$fit', newobj = 'pred', 
        datasources = connections[c('genr', 'abcd')])
ds.make(toAssign = 'D9$nvi_pc_ - pred', newobj = 'res', 
        datasources = connections[c('genr', 'eden', 'abcd')])

# check normality of residuals
ds.histogram('res', datasources = connections[c('genr', 'eden', 'abcd')]) # we can also calculate standardised residuals using ds.make

# check homoscedasticity
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'eden', 'abcd')])

# check normality of residuals
jpeg(file="/home/jovyan/results/model_diagnostics/res_pred_plot_nvi_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.histogram('res', datasources = connections[c('genr', 'eden', 'abcd')]) # we can also calculate standardised residuals using ds.make
grid.text("Residuals NVI model", .5, .98, gp=gpar(cex=1))
dev.off()

# check homoscedasticity
jpeg(file="/home/jovyan/results/model_diagnostics/res_distr_plot_nvi_ECCN.jpeg",
     width=600, height=600, pointsize = 20, quality = 100)
ds.scatterPlot(x='pred', y = 'res', 
               method = 'probabilistic',
               datasources = connections[c('genr', 'eden', 'abcd')])
grid.text("Histogram residuals NVI model", .5, .98, gp=gpar(cex=1))
dev.off()

#--------------------------- II) Likelihood ratio test--------------------------
#--------------------------- IIa) Interaction with sex--------------------------
model1_nvi.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep.n + sex.n + nvi_age_ + edu_m_d.2 + edu_m_d.3 + agebirth_m_y + 
  preg_alc.n + preg_smk.n + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_nvi.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep + sex + nvi_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_int.glm.results',
  datasources = connections[c('genr')])

L0_eden <- as.numeric(model1_nvi.fit$output.summary$study1[[14]]['logLik'])

model1_nvi_sexint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep*sex + nvi_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_nvi_sexint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep*sex + nvi_age_ + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_nvi_sexint.glm.results',
  datasources = connections[c('genr')])

L1_eden <- as.numeric(model1_nvi_sexint.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L1_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_nvi.glm.fit$output.summary$study1$deviance.resid - model1_nvi_sexint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_nvi_sexint.glm.fit$output.summary$study1$df.resid
scale <- model1_nvi_sexint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------------- IIb) Interaction with age--------------------------
model1_nvi_ageint.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep*nvi_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_nvi_ageint.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep*nvi_age_ + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_nvi_ageint.glm.results',
  datasources = connections[c('genr')])

L2_eden <- as.numeric(model1_nvi_ageint.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L2_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_nvi.glm.fit$output.summary$study1$deviance.resid - model1_nvi_ageint.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_nvi_ageint.glm.fit$output.summary$study1$df.resid
scale <- model1_nvi_ageint.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#------------------------- IIc) Nonlinear term for age--------------------------
ds.make(toAssign = 'df_basic$nvi_age_ * df_basic$nvi_age_', newobj = 'nvi_age_sq', 
        datasources = connections[c('genr', 'eden')])

ds.dataFrame(c('df_basic', 'nvi_age_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'eden')])

model1_nvi_nonlin.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep + nvi_age_ + nvi_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_nvi_nonlin.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep + nvi_age_ + nvi_age_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_nvi_nonlin.glm.results',
  datasources = connections[c('genr')])

L3_eden <- as.numeric(model1_nvi_nonlin.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L3_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_nvi.glm.fit$output.summary$study1$deviance.resid - model1_nvi_nonlin.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_nvi_nonlin.glm.fit$output.summary$study1$df.resid
scale <- model1_nvi_nonlin.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#--------------------- IId) Nonlinear term for prepreg_BMI----------------------
ds.make(toAssign = 'df_basic$prepreg_BMI * df_basic$prepreg_BMI', newobj = 'prepreg_BMI_sq', 
        datasources = connections[c('genr', 'dnbc', 'eden', 'ninfea')])

ds.dataFrame(c('df_basic', 'prepreg_BMI_sq'),
             newobj = 'df_basic',
             datasources = connections[c('genr', 'eden')])

model1_nvi_nonlin_BMI.fit <- ds.lmerSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep + nvi_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI + (1|child_id)", 
  REML = FALSE,
  datasources = connections[c('eden')]
)

model1_nvi_nonlin_BMI.glm.fit <- ds.glmSLMA(
  dataName = "df_basic",
  formula = "nvi_pc_ ~ preg_dep + nvi_age_ + prepreg_BMI_sq + sex + edu_m_.0 + agebirth_m_y + 
  preg_alc + preg_smk + prepreg_BMI",
  family = "gaussian",
  combine.with.metafor = TRUE,
  newobj = 'model1_nvi_nonlin.glm.results',
  datasources = connections[c('genr')])

L4_eden <- as.numeric(model1_nvi_nonlin_BMI.fit$output.summary$study1[[14]]['logLik'])

# EDEN
Chisq <- -2*(as.numeric(L0_eden)-as.numeric(L4_eden))
(p.val <- pchisq(Chisq, df = 1, lower.tail = FALSE))

# GenR
dev.diff <- model1_nvi.glm.fit$output.summary$study1$deviance.resid - model1_nvi_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid

# this difference now needs to be "scaled". Scale parameter is given by:
df <- model1_nvi_nonlin_BMI.glm.fit$output.summary$study1$df.resid
scale <- model1_nvi_nonlin_BMI.glm.fit$output.summary$study1$deviance.resid/df

# Change in "scaled" deviance may now be estimated as 
scaled.dev <- dev.diff/scale

# This can be treated as a chi squared on 1 degree of freedom
# one degree because df in mod1 is one more than mod2
(1-pchisq(scaled.dev,1)) #p

#---------------------------------- III) VIF -----------------------------------
#install.packages(Hmisc)
v <- model1_nvi.fit$output.summary$study1$vcov
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_eden <- diag(solve(v/(d %o% d)))
names(vif_eden) <- nam
vif_eden

v <- model1_nvi.glm.fit$output.summary$study1$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_genr <- diag(solve(v/(d %o% d)))
names(vif_genr) <- nam
vif_genr

v <- model1_nvi.glm.fit$output.summary$study2$cov.scaled
nam <- dimnames(v)[[1]]
ns <- 1
if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
}
d <- diag(v)^0.5
vif_abcd <- diag(solve(v/(d %o% d)))
names(vif_abcd) <- nam
vif_abcd

#----------------------------------16) Log out----------------------------------
#logout again from server
DSI::datashield.logout(connections)

