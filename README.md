# prenatal-depression-meta-analysis
This repository hosts the R code of the project "**Prenatal maternal depression and child behavioural and developmental outcomes: 
An individual participant data meta-analysis in 75,000 children from the EU Child Cohort Network**".

## Analytical pipeline 
 - **`00_ECCN_analyses_DataSHIELD.R`**: Full analyses for ECCN cohorts (ABCD, DNBC, EDEN, Generation R, and NINFEA) in the DataSHIELD server.
 - **`00_ALSPAC_PREDO_analyses.R`**: Full analyses for the external (i.e. not available in the DataSHIELD server) cohorts (ALSPAC and PREDO).
 - **`00_descriptives_included.R`**: Create descriptive table for the included sample.
 - **`00_descriptives_excluded.R`**: Create descriptive table for the excluded sample.
 - **`00_flowchart.R`**: Flowchart for Figure 1.
 - **`01_meta_analyses.R`**: Meta-analyses for RQ1 (associations between a binary measure of prenatal maternal depression and eight offspring outcomes) + Figures 2 and 3.
 - **`02_meta_sexinteraction.R`**: Meta-analyses for RQ2 (testing sex differences in the associations between prenatal maternal depression and eight offspring outcomes).
 - **`03_meta_analyses_preadj.R`**: Meta-analyses for RQ3 (investigating the role of timing of maternal depression), specifically testing associations between prenatal maternal depression and offspring internalising, externalising, ADHD, and ASD symptoms when adjusting for depression prior to pregnancy.
 - **`03_meta_mediation.R`**: Meta-analyses for RQ3 (investigating the role of timing of maternal depression), specifically testing mediation by postnatal maternal depression in the associations between prenatal maternal depression and offspring internalising, externalising, ADHD, and ASD symptoms.
 - **`03_meta_analyses_cumulative.R`**: Meta-analyses for RQ3 (investigating the role of timing of maternal depression), specifically associations between cumulative maternal depression and offspring internalising, externalising, ADHD, and ASD symptoms.
  - **`04_individual_timepoints.R`**: Individual associations between pre-pregnancy and postnatal depression with offspring outcomes.
  - **`04_sensitivity.R`**: Sensitivity analyses.
    a): Associations between prenatal depression and offspring outcomes when adjusting for minimal covariates
    b): Associations between prenatal depression and offspring outcomes when additionally adjusting for maternal country of birth
    c): Associations between continuous depressive symptoms and offspring outcomes.
  - **`05_REV_analyses_GenR.R`**: Additional analyses for revision LRHE. 
  - **`05_REV_tables.R`**: Tables for revision LRHE. 
