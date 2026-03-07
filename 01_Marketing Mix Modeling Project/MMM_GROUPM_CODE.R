
library(Robyn)

# Please, check if you have installed the latest version before running this demo. Update if not
# https://github.com/facebookexperimental/Robyn/blob/main/R/DESCRIPTION#L4
packageVersion("Robyn")
# Also, if you're using an older version than the latest dev version, please check older demo.R with
# https://github.com/facebookexperimental/Robyn/blob/vX.X.X/demo/demo.R

## Force multi-core use when running RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = TRUE)
options(future.fork.enable = TRUE)

# Set to FALSE to avoid the creation of files locally
create_files <- TRUE


data("dt_prophet_holidays")
head(dt_prophet_holidays)

# Directory where you want to export results to (will create new folders)
robyn_directory <- "~/Desktop/BA lab/"


################################################################
#### Step 2a: For first time user: Model specification in 4 steps

#### 2a-1: First, specify input variables
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(lubridate)) install.packages("readxl")
library(dplyr)
library(lubridate)
library(readr)
library(readxl)

# import data
data <- read_excel("Group15_GROUPM_DATA.xlsx")


# Load necessary libraries
library(dplyr)


data$NWEEK_squared <- data$NWEEK^2

# Ensure 'data' is indeed a data frame
data <- as.data.frame(data)




# covert date to data
data$WEEKSTARTINGSELLOUT <- as.Date(data$WEEKSTARTINGSELLOUT, format = "%d.%m.%y")
data$WEEKSTARTINGSELLOUT <- format(data$WEEKSTARTINGSELLOUT, "%Y-%m-%d")


##log transformation
data_log_transformed <- data
data_log_transformed$INVESTMENTSTV_GOOGLESEARCH <- data_log_transformed$INVESTMENTSTV * data_log_transformed$INVESTMENTSGOOGLESEARCH
data_log_transformed$INVESTMENTSTV_SOCIALMETA <- data_log_transformed$INVESTMENTSTV * data_log_transformed$INVESTMENTSSOCIALMETA
data_log_transformed$INVESTMENTSSOCIALMETA_YOUTUBE <- data_log_transformed$INVESTMENTSSOCIALMETA * data_log_transformed$INVESTMENTSYOUTUBE
data_log_transformed$INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS <- data_log_transformed$INVESTMENTSSOCIALMETA * data_log_transformed$INVESTMENTSOTHERVIDEOPUBLISHERS
data_log_transformed$INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM <- data_log_transformed$INVESTMENTSSOCIALMETA * data_log_transformed$INVESTMENTSINFLUENCERMARKETINGINSTAGRAM
data_log_transformed$NWEEK_squared <- data_log_transformed$NWEEK^2

data_log_transformed$CHRISTMAS <- as.factor(data_log_transformed$CHRISTMAS)
data_log_transformed$NEWYEARSEVE <- as.factor(data_log_transformed$NEWYEARSEVE)
data_log_transformed$EASTER <- as.factor(data_log_transformed$EASTER)
data_log_transformed$MIDAUGUST <- as.factor(data_log_transformed$MIDAUGUST)
data_log_transformed$MOMSDAY <- as.factor(data_log_transformed$MOMSDAY)
data_log_transformed$BLACKFRIDAY <- as.factor(data_log_transformed$BLACKFRIDAY)


#####MODEL 1
################
################
################

InputCollect <- robyn_inputs(
  
  dt_input <- data,
  
  dt_holidays = dt_prophet_holidays,
  date_var = "WEEKSTARTINGSELLOUT", # date format must be "2020-01-01"
  dep_var = "VALUESALESBRAND", # there should be only one dependent variable
  dep_var_type = "revenue", 
  prophet_vars = c("trend", "season"), 
  prophet_country = "IT", # input country code. Check: dt_prophet_holidays
  context_vars = c("ADSTOCKCOMP1","ADSTOCKCOMP2","WEIGHTEDHANDLINGDISTRIBUTION",
                   "AVERAGEPRICEBRAND","AVERAGEPROMOPRICE",
                   "VALUESALESINPROMOPRICECUT",
                   "VALUESALESINPROMOBUNDLE","PRICEINDEXVSRESTMKT",
                   "SEASONALITYMARKET","LOCKDOWN"), 
  paid_media_spends = c("INVESTMENTSTV","INVESTMENTSYOUTUBE",
                        "INVESTMENTSOTHERVIDEOPUBLISHERS","INVESTMENTSSOCIALMETA",	
                        "INVESTMENTSGOOGLESEARCH","INVESTMENTSINFLUENCERMARKETINGINSTAGRAM"), # mandatory input
  paid_media_vars = c("GRPTV","IMPRESSIONSYOUTUBE",
                      "IMPRESSIONSOTHERVIDEOPUBLISHERS","IMPRESSIONSSOCIALMETA",
                      "INVESTMENTSGOOGLESEARCH",
                      "IMPRESSIONSINFLUENCERMARKETINGINSTAGRAM"), # mandatory.

  
  organic_vars = c("TOTEMS","DISTRIBUTEDSAMPLES","EDITORIALS"),
  
  window_start = "2020-01-05",
  window_end = "2023-12-24",
  adstock = "geometric" # geometric, weibull_cdf or weibull_pdf.
)


## hyperparameter names are based on paid_media_spends names too. See right hyperparameter names:
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)

hyperparameters <- list(
  INVESTMENTSTV_alphas = c(0.5, 3),
  INVESTMENTSTV_gammas = c(0.3, 0.99),  # Slightly increased upper bound for gamma
  INVESTMENTSTV_thetas = c(0.8, 0.99),
  INVESTMENTSYOUTUBE_alphas = c(0.5, 3),
  INVESTMENTSYOUTUBE_gammas = c(0.3, .99),  # Slightly increased upper bound for gamma
  INVESTMENTSYOUTUBE_thetas = c(0.7, 0.95),  # Lower upper bound for theta
  INVESTMENTSOTHERVIDEOPUBLISHERS_alphas = c(0.5, 3),
  INVESTMENTSOTHERVIDEOPUBLISHERS_gammas = c(0.3, 0.99),  # Slightly increased upper bound for gamma
  INVESTMENTSOTHERVIDEOPUBLISHERS_thetas = c(0.8, 0.99),
  INVESTMENTSSOCIALMETA_alphas = c(0.5, 3),
  INVESTMENTSSOCIALMETA_gammas = c(0.3, 1),
  INVESTMENTSSOCIALMETA_thetas = c(0.1, 0.3),
  INVESTMENTSGOOGLESEARCH_alphas = c(0.5, 3),
  INVESTMENTSGOOGLESEARCH_gammas = c(0.3, 1),
  INVESTMENTSGOOGLESEARCH_thetas = c(0.1, 0.3),
  INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_alphas = c(0.5, 3),
  INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_gammas = c(0.3, 1),
  INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_thetas = c(0.8, 0.99),
  TOTEMS_alphas = c(0.5, 3),
  TOTEMS_gammas = c(0.3, 1),
  TOTEMS_thetas = c(0.1, 0.3),
  DISTRIBUTEDSAMPLES_alphas = c(0.5, 3),
  DISTRIBUTEDSAMPLES_gammas = c(0.3, 1),
  DISTRIBUTEDSAMPLES_thetas = c(0.1, 0.3),
  EDITORIALS_alphas = c(0.5, 3),
  EDITORIALS_gammas = c(0.3, 1),
  EDITORIALS_thetas = c(0.1, 0.3),
  train_size = c(0.5, 0.8)
)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}

#### Step 3: Build initial model
update.packages(ask = FALSE, checkBuilt = TRUE)

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = 2, # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = create_files, # this will create files locally
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = create_files # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)

#### SELECT THE BEST OF OUR MODELS

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "3_390_2" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)



################################################################
#### Step 5: Get budget allocation based on the selected model above


# Check media summary for selected model
print(ExportedModel)

# Run ?robyn_allocator to check parameter definition

# NOTE: The order of constraints should follow:
InputCollect$paid_media_spends

# Scenario "max_response": "What's the max. return given certain spend?"
# Example 1: max_response default setting: maximize response for latest month
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = "all", # Default to "all"
  # total_budget = NULL, # When NULL, default is total spend in date_range
  channel_constr_low = 0.7,
  channel_constr_up = 1.5,
  # channel_constr_multiplier = 3,
  scenario = "max_response",
  export = create_files
)
# Print & plot allocator's output
print(AllocatorCollect1)
plot(AllocatorCollect1)


####MODEL 2
#################
#################
#################
InputCollect <- robyn_inputs(
  dt_input = data_log_transformed,
  dt_holidays = dt_prophet_holidays,
  date_var = "WEEKSTARTINGSELLOUT", 
  dep_var = "VALUESALESBRAND", 
  dep_var_type = "revenue", 
  prophet_vars = c("trend","season","holiday"), 
  prophet_country = "IT", 
  context_vars = c("ADSTOCKCOMP1", "ADSTOCKCOMP2", "WEIGHTEDHANDLINGDISTRIBUTION",
                   "VALUESALESINPROMOPRICECUT",
                   "VALUESALESINPROMOBUNDLE", "PRICEINDEXVSRESTMKT",
                   "SEASONALITYMARKET", "LOCKDOWN","DISTRIBUTEDSAMPLES"
                   ), 
  paid_media_spends = c("INVESTMENTSTV", "INVESTMENTSYOUTUBE",
                        "INVESTMENTSOTHERVIDEOPUBLISHERS", "INVESTMENTSSOCIALMETA",
                        "INVESTMENTSGOOGLESEARCH", "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM",
                        "INVESTMENTSTV_GOOGLESEARCH",
                        "INVESTMENTSTV_SOCIALMETA",
                        "INVESTMENTSSOCIALMETA_YOUTUBE", "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS",
                        "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM","INVESTMENTSAMAZONSEARCH"), 
  paid_media_vars = c("GRPTV", "IMPRESSIONSYOUTUBE",
                      "IMPRESSIONSOTHERVIDEOPUBLISHERS", "IMPRESSIONSSOCIALMETA",
                      "INVESTMENTSGOOGLESEARCH",
                      "IMPRESSIONSINFLUENCERMARKETINGINSTAGRAM", "INVESTMENTSTV_GOOGLESEARCH",
                      "INVESTMENTSTV_SOCIALMETA",
                      "INVESTMENTSSOCIALMETA_YOUTUBE", "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS",
                      "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM","INVESTMENTSAMAZONSEARCH"), 
  exclude_vars = c("AVERAGEPRICEBRAND", "AVERAGEPROMOPRICE"),
  organic_vars = c("TOTEMS", "FREESAMPLES", "EDITORIALS","POSITIVEOPINIONS","NEGATIVEOPINIONS","ONLINEREVIEWS","TOTEMDISPLAYNEXTTOCASHIER","TOTEMDISPLAYINSTORE"),
  # continuous_vars = c("NWEEK_squared"),
  window_start = "2020-01-05",
  window_end = "2023-12-24",
  #factor_vars = c("CHRISTMAS","NEWYEARSEVE","EASTER","MIDAUGUST","MOMSDAY","BLACKFRIDAY"),
  context_signs = c("default", "default", "positive","positive","positive", "default","default", "default", "positive"
                    ),
  adstock = "geometric"
)



hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)


hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)


plot_adstock(plot = TRUE)
plot_saturation(plot = TRUE)

## 2. Get correct hyperparameter names:


# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  INVESTMENTSTV_alphas = c(0.5, 3),
  INVESTMENTSTV_gammas = c(0.3, 1),
  INVESTMENTSTV_thetas = c(0.1, 0.99),
  
  INVESTMENTSYOUTUBE_alphas = c(0.5, 3),
  INVESTMENTSYOUTUBE_gammas = c(0.3, 1),
  INVESTMENTSYOUTUBE_thetas = c(0.1, 0.99),
  
  INVESTMENTSOTHERVIDEOPUBLISHERS_alphas = c(0.5, 3),
  INVESTMENTSOTHERVIDEOPUBLISHERS_gammas = c(0.3, 1),
  INVESTMENTSOTHERVIDEOPUBLISHERS_thetas = c(0.1, 0.99),
  
  INVESTMENTSSOCIALMETA_alphas = c(0.5, 3),
  INVESTMENTSSOCIALMETA_gammas = c(0.3, 1),
  INVESTMENTSSOCIALMETA_thetas = c(0.1, 0.99),
  
  INVESTMENTSGOOGLESEARCH_alphas = c(0.5, 3),
  INVESTMENTSGOOGLESEARCH_gammas = c(0.3, 1),
  INVESTMENTSGOOGLESEARCH_thetas = c(0.1, 0.99),
  
  INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_alphas = c(0.5, 3),
  INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_gammas = c(0.3, 1),
  INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_thetas = c(0.1, 0.99),
  
  INVESTMENTSTV_GOOGLESEARCH_alphas = c(0.5, 3),
  INVESTMENTSTV_GOOGLESEARCH_gammas = c(0.3, 1),
  INVESTMENTSTV_GOOGLESEARCH_thetas = c(0.1, 0.99),
  
  INVESTMENTSTV_SOCIALMETA_alphas = c(0.5, 3),
  INVESTMENTSTV_SOCIALMETA_gammas = c(0.3, 1),
  INVESTMENTSTV_SOCIALMETA_thetas = c(0.1, 0.99),
  
  INVESTMENTSSOCIALMETA_YOUTUBE_alphas = c(0.5, 3),
  INVESTMENTSSOCIALMETA_YOUTUBE_gammas = c(0.3, 1),
  INVESTMENTSSOCIALMETA_YOUTUBE_thetas = c(0.1, 0.99),
  
  INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS_alphas = c(0.5, 3),
  INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS_gammas = c(0.3, 1),
  INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS_thetas = c(0.1, 0.99),
  
  INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM_alphas = c(0.5, 3),
  INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM_gammas = c(0.3, 1),
  INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM_thetas = c(0.1, 0.99),
  
  INVESTMENTSAMAZONSEARCH_alphas = c(0.5, 3),
  INVESTMENTSAMAZONSEARCH_gammas = c(0.3, 1),
  INVESTMENTSAMAZONSEARCH_thetas = c(0.1, 0.99),
  
  TOTEMS_alphas = c(0.5, 3),
  TOTEMS_gammas = c(0.3, 1),
  TOTEMS_thetas = c(0.1, 0.99),
  
  FREESAMPLES_alphas = c(0.5, 3),
  FREESAMPLES_gammas = c(0.3, 1),
  FREESAMPLES_thetas = c(0.1, 0.99),
  
  EDITORIALS_alphas = c(0.5, 3),
  EDITORIALS_gammas = c(0.3, 1),
  EDITORIALS_thetas = c(0.1, 0.99),
  
  POSITIVEOPINIONS_alphas = c(0.5, 3),
  POSITIVEOPINIONS_gammas = c(0.3, 1),
  POSITIVEOPINIONS_thetas = c(0.1, 0.99),
  
  NEGATIVEOPINIONS_alphas = c(0.5, 3),
  NEGATIVEOPINIONS_gammas = c(0.3, 1),
  NEGATIVEOPINIONS_thetas = c(0.1, 0.99),
  
  ONLINEREVIEWS_alphas = c(0.5, 3),
  ONLINEREVIEWS_gammas = c(0.3, 1),
  ONLINEREVIEWS_thetas = c(0.1, 0.99),
  
  TOTEMDISPLAYNEXTTOCASHIER_alphas = c(0.5, 3),
  TOTEMDISPLAYNEXTTOCASHIER_gammas = c(0.3, 1),
  TOTEMDISPLAYNEXTTOCASHIER_thetas = c(0.1, 0.99),
  
  TOTEMDISPLAYINSTORE_alphas = c(0.5, 3),
  TOTEMDISPLAYINSTORE_gammas = c(0.3, 1),
  TOTEMDISPLAYINSTORE_thetas = c(0.1, 0.99),
  
  train_size = c(0.5, 0.8)
)


InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)




#### Step 3: Build initial model

OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = TRUE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature. Use with caution.
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
# Read more about convergence rules: ?robyn_converge
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Check time-series validation plot (when ts_validation == TRUE)
# Read more and replicate results: ?ts_validation
if (OutputModels$ts_validation) OutputModels$ts_validation_plot

## Calculate Pareto fronts, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = 2, # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = create_files, # this will create files locally
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = create_files # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)



################################################################


#### Step 4: Select our best model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "5_159_6" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = create_files)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)



# Check media summary for selected model
print(ExportedModel)



# NOTE: The order of constraints should follow:
InputCollect$paid_media_spends



# max_response default setting: maximize response for last year
AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = c("2023-01-01", "2023-12-24"), # Default to "all"
  # total_budget = NULL, # When NULL, default is total spend in date_range
  channel_constr_low = 0.7,
  channel_constr_up = 1.5,
  # channel_constr_multiplier = 3,
  scenario = "max_response",
  export = create_files
)
# Print & plot allocator's output
print(AllocatorCollect1)
plot(AllocatorCollect1)

