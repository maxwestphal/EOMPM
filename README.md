---
title: "Simulation study: Evaluation of multiple prediction models (EOMPM)"
author: "Max Westphal, Institute for Statistics, University of Bremen, Germany (mwestphal@uni-bremen.de)"
date: "July 6, 2018"
output: html_document
---



## Purpose

This README gives instructions how to use the provided R code to reproduce the simulation study and results of the publication 

**Westphal, M. and Brannath, W. Evaluation of multiple prediction models: a novel view on model selection and performance assessment. Statistical Methods in Medical Research, forthcoming 2019.**

If you refer to our simulation study, please cite this publication. In case you have any questions or suggestions regarding the R code you may contact us via e-mail.


## Instructions

The R code can be divided into three parts: (1) simulation, (2) analysis and (3) application. Accordingly, the main folder **EOMPM** contains three subfolders **1_SIMULATION**, **2_ANALYSIS** and **3_APPLICATION**.

### (1) Simulation

The R code in this part may be used to reproduce the simulation study from the paper. For this purpose, the following scripts have to be executed (in the given order):

(a) **CONFIG/EOMPM_sim0_parameters_*XYZ*.R**: load all relevant packages and functions, initalize simulation parameters (depending on the suffix *XYZ*, see code documentation). 
(b) **EOMPM_sim1_population**: create 'population' registry which contains the population datasets used to compute the true model performances in the next step.
(c) **EOMPM_sim2_training**: create 'training' registry which contains all candidate models fitted on the training data and cross-validation estimates.
(d) **EOMPM_sim3_estimation**: create 'estimation' registry which contains performance and covariance estimates for all candidate models.
(e) **EOMPM_sim4_evaluation**: create 'evaluation' registry which applies the specified model selection rules (based on the validation estimates) and conducts the evaluation study (based on the estimates from the last step).

To reproduce all results from the publication this has to iterated over all parameter settings, i.e. over all provided scripts **EOMPM_sim0_parameters_XYZ.R**. The complete list of 19 scripts can be found in the **CONFIG** subfolder. The script **EOMPM_sim0_parameters_TEST.R** with a small number of simulation runs may be used initially to check if everything work. The most convinient way to conduct all simulations for a given set of parameter configurations is to execute the script **EOMPM_sim0_MAIN.R**.

The reason the estimation step is conducted prior to model selection in the simulation study, is to avoid redundant calculations of the same performance and covariance estimates. Of course, the model selection is independent of the evaluation data.

By default, only 1000 different learning datasets (*nsim.train* parameter) are considered per series of batchtools registries (training - estimation - evaluation) as this provided the best performance and stability for our computer setup.

The scripts **EOMPM_functions_*XYZ*.R** contain all neccessary functions and may be reviewed and tested independently. All registries will be created in the subfolder **DATA**. The results will be created on the fly (i.e. after learning and evaluation respectively) and saved as CSV files to the subfolder **RESULTS**. 

### (2) Analysis

The R scripts here may be used to analyze the (exported) simulation results from part (1). The follwing scripts may reproduce results (figures) presented in the publication. The first two scripts need to be executed initially to prepare some of the analyses and read in and preprocess the simulation results. If only a part of the simulations was conducted, the files to be read in need to be modified accordingly.

(a) **EOMPM_analysis0_prep.R**
(b) **EOMPM_analysis1_input.R**
(c) **EOMPM_analysis2_tasks.R**
(d) **EOMPM_analysis3_power.R**
(e) **EOMPM_analysis4_bias.R**
(f) **EOMPM_analysis5_performance.R**
(g) **EOMPM_analysis6_perturbed.R**

Reading in all simulation results at once may lead to memory problems. In this case some of the analyses will need to be split up manually by the user. At least 16GB of memory are recommended.

### (3) Application

The two scripts **EOMPM_application_A.R** and **EOMPM_application_B.R** allow to reproduce the real data examples from the publications. Further details are provided as comments directly in the R scripts. Note that the results may depend on the *random.seed* (which is initially set to 1 in both examples) but also on the used system (e.g. OS). For the real data examples (and a major part of the simulation study) we used R version 3.4.4 running on a x86-64 system with Windows 10 OS.


## Version history:

(1) 06-07-2018: Initial creation.
(2) 07-06-2019: Minor corrections.

