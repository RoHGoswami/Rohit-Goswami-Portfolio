### Business Analytics Lab Project
### Rohit Goswami
import os
import warnings
import pandas as pd
import numpy as np

# Robyn Python imports
from robyn import Robyn, RobynInputs, RobynAllocator

# ──────────────────────────────────────────────
# Configuration
# ──────────────────────────────────────────────
CREATE_FILES = True
ROBYN_DIRECTORY = os.path.expanduser("~/Desktop/BA lab/")
os.makedirs(ROBYN_DIRECTORY, exist_ok=True)

# ──────────────────────────────────────────────
# Load holiday data (Robyn built-in)
# ──────────────────────────────────────────────
from robyn.data import load_prophet_holidays

dt_prophet_holidays = load_prophet_holidays()
print(dt_prophet_holidays.head())

# ──────────────────────────────────────────────
# Step 2a-1: Import & prepare input data
# ──────────────────────────────────────────────
data = pd.read_excel("Group15_GROUPM_DATA.xlsx")

# Create NWEEK_squared
data["NWEEK_squared"] = data["NWEEK"] ** 2

# Convert date column
data["WEEKSTARTINGSELLOUT"] = pd.to_datetime(
    data["WEEKSTARTINGSELLOUT"], format="%d.%m.%y"
)
data["WEEKSTARTINGSELLOUT"] = data["WEEKSTARTINGSELLOUT"].dt.strftime("%Y-%m-%d")

# ──────────────────────────────────────────────
# Create interaction terms & log-transformed copy
# ──────────────────────────────────────────────
data_log_transformed = data.copy()

data_log_transformed["INVESTMENTSTV_GOOGLESEARCH"] = (
    data_log_transformed["INVESTMENTSTV"]
    * data_log_transformed["INVESTMENTSGOOGLESEARCH"]
)
data_log_transformed["INVESTMENTSTV_SOCIALMETA"] = (
    data_log_transformed["INVESTMENTSTV"]
    * data_log_transformed["INVESTMENTSSOCIALMETA"]
)
data_log_transformed["INVESTMENTSSOCIALMETA_YOUTUBE"] = (
    data_log_transformed["INVESTMENTSSOCIALMETA"]
    * data_log_transformed["INVESTMENTSYOUTUBE"]
)
data_log_transformed["INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS"] = (
    data_log_transformed["INVESTMENTSSOCIALMETA"]
    * data_log_transformed["INVESTMENTSOTHERVIDEOPUBLISHERS"]
)
data_log_transformed["INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM"] = (
    data_log_transformed["INVESTMENTSSOCIALMETA"]
    * data_log_transformed["INVESTMENTSINFLUENCERMARKETINGINSTAGRAM"]
)
data_log_transformed["NWEEK_squared"] = data_log_transformed["NWEEK"] ** 2

# Convert holiday columns to categorical (factor equivalent)
for col in [
    "CHRISTMAS",
    "NEWYEARSEVE",
    "EASTER",
    "MIDAUGUST",
    "MOMSDAY",
    "BLACKFRIDAY",
]:
    data_log_transformed[col] = data_log_transformed[col].astype("category")


# ══════════════════════════════════════════════
#  MODEL 1
# ══════════════════════════════════════════════

InputCollect = RobynInputs(
    dt_input=data,
    dt_holidays=dt_prophet_holidays,
    date_var="WEEKSTARTINGSELLOUT",
    dep_var="VALUESALESBRAND",
    dep_var_type="revenue",
    prophet_vars=["trend", "season"],
    prophet_country="IT",
    context_vars=[
        "ADSTOCKCOMP1",
        "ADSTOCKCOMP2",
        "WEIGHTEDHANDLINGDISTRIBUTION",
        "AVERAGEPRICEBRAND",
        "AVERAGEPROMOPRICE",
        "VALUESALESINPROMOPRICECUT",
        "VALUESALESINPROMOBUNDLE",
        "PRICEINDEXVSRESTMKT",
        "SEASONALITYMARKET",
        "LOCKDOWN",
    ],
    paid_media_spends=[
        "INVESTMENTSTV",
        "INVESTMENTSYOUTUBE",
        "INVESTMENTSOTHERVIDEOPUBLISHERS",
        "INVESTMENTSSOCIALMETA",
        "INVESTMENTSGOOGLESEARCH",
        "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM",
    ],
    paid_media_vars=[
        "GRPTV",
        "IMPRESSIONSYOUTUBE",
        "IMPRESSIONSOTHERVIDEOPUBLISHERS",
        "IMPRESSIONSSOCIALMETA",
        "INVESTMENTSGOOGLESEARCH",
        "IMPRESSIONSINFLUENCERMARKETINGINSTAGRAM",
    ],
    organic_vars=["TOTEMS", "DISTRIBUTEDSAMPLES", "EDITORIALS"],
    window_start="2020-01-05",
    window_end="2023-12-24",
    adstock="geometric",
)

# ── Hyperparameter names (for reference) ──
InputCollect.hyper_names()

# ── Hyperparameter ranges ──
hyperparameters = {
    "INVESTMENTSTV_alphas": [0.5, 3],
    "INVESTMENTSTV_gammas": [0.3, 0.99],
    "INVESTMENTSTV_thetas": [0.8, 0.99],
    "INVESTMENTSYOUTUBE_alphas": [0.5, 3],
    "INVESTMENTSYOUTUBE_gammas": [0.3, 0.99],
    "INVESTMENTSYOUTUBE_thetas": [0.7, 0.95],
    "INVESTMENTSOTHERVIDEOPUBLISHERS_alphas": [0.5, 3],
    "INVESTMENTSOTHERVIDEOPUBLISHERS_gammas": [0.3, 0.99],
    "INVESTMENTSOTHERVIDEOPUBLISHERS_thetas": [0.8, 0.99],
    "INVESTMENTSSOCIALMETA_alphas": [0.5, 3],
    "INVESTMENTSSOCIALMETA_gammas": [0.3, 1],
    "INVESTMENTSSOCIALMETA_thetas": [0.1, 0.3],
    "INVESTMENTSGOOGLESEARCH_alphas": [0.5, 3],
    "INVESTMENTSGOOGLESEARCH_gammas": [0.3, 1],
    "INVESTMENTSGOOGLESEARCH_thetas": [0.1, 0.3],
    "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_alphas": [0.5, 3],
    "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_gammas": [0.3, 1],
    "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_thetas": [0.8, 0.99],
    "TOTEMS_alphas": [0.5, 3],
    "TOTEMS_gammas": [0.3, 1],
    "TOTEMS_thetas": [0.1, 0.3],
    "DISTRIBUTEDSAMPLES_alphas": [0.5, 3],
    "DISTRIBUTEDSAMPLES_gammas": [0.3, 1],
    "DISTRIBUTEDSAMPLES_thetas": [0.1, 0.3],
    "EDITORIALS_alphas": [0.5, 3],
    "EDITORIALS_gammas": [0.3, 1],
    "EDITORIALS_thetas": [0.1, 0.3],
    "train_size": [0.5, 0.8],
}

InputCollect.set_hyperparameters(hyperparameters)
print(InputCollect)

# Plot exposure fits if available
if len(InputCollect.exposure_vars) > 0:
    for p in InputCollect.modNLS["plots"]:
        p.show()

# ── Step 3: Build initial model ──
OutputModels = InputCollect.run(
    cores=None,  # None defaults to (max available - 1)
    iterations=2000,
    trials=5,
    ts_validation=True,
    add_penalty_factor=False,
)
print(OutputModels)

# Convergence plots
OutputModels.convergence["moo_distrb_plot"].show()
OutputModels.convergence["moo_cloud_plot"].show()

# Time-series validation plot
if OutputModels.ts_validation:
    OutputModels.ts_validation_plot.show()

# ── Pareto outputs ──
OutputCollect = OutputModels.get_outputs(
    pareto_fronts=2,
    csv_out="pareto",
    clusters=True,
    export=CREATE_FILES,
    plot_folder=ROBYN_DIRECTORY,
    plot_pareto=CREATE_FILES,
)
print(OutputCollect)

# ── Select best model (Model 1) ──
select_model = "3_390_2"  # ← Replace with actual model ID from OutputCollect

ExportedModel = OutputCollect.write(select_model, export=CREATE_FILES)
print(ExportedModel)

# One-pager
myOnePager = OutputCollect.onepagers(select_model, export=False)

# ── Step 5: Budget Allocation (Model 1) ──
print(ExportedModel)
print(InputCollect.paid_media_spends)

AllocatorCollect1 = RobynAllocator(
    InputCollect=InputCollect,
    OutputCollect=OutputCollect,
    select_model=select_model,
    channel_constr_low=0.7,
    channel_constr_up=1.5,
    scenario="max_response",
    export=CREATE_FILES,
)
print(AllocatorCollect1)
AllocatorCollect1.plot()


# ══════════════════════════════════════════════
#  MODEL 2
# ══════════════════════════════════════════════

InputCollect2 = RobynInputs(
    dt_input=data_log_transformed,
    dt_holidays=dt_prophet_holidays,
    date_var="WEEKSTARTINGSELLOUT",
    dep_var="VALUESALESBRAND",
    dep_var_type="revenue",
    prophet_vars=["trend", "season", "holiday"],
    prophet_country="IT",
    context_vars=[
        "ADSTOCKCOMP1",
        "ADSTOCKCOMP2",
        "WEIGHTEDHANDLINGDISTRIBUTION",
        "VALUESALESINPROMOPRICECUT",
        "VALUESALESINPROMOBUNDLE",
        "PRICEINDEXVSRESTMKT",
        "SEASONALITYMARKET",
        "LOCKDOWN",
        "DISTRIBUTEDSAMPLES",
    ],
    paid_media_spends=[
        "INVESTMENTSTV",
        "INVESTMENTSYOUTUBE",
        "INVESTMENTSOTHERVIDEOPUBLISHERS",
        "INVESTMENTSSOCIALMETA",
        "INVESTMENTSGOOGLESEARCH",
        "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM",
        "INVESTMENTSTV_GOOGLESEARCH",
        "INVESTMENTSTV_SOCIALMETA",
        "INVESTMENTSSOCIALMETA_YOUTUBE",
        "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS",
        "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM",
        "INVESTMENTSAMAZONSEARCH",
    ],
    paid_media_vars=[
        "GRPTV",
        "IMPRESSIONSYOUTUBE",
        "IMPRESSIONSOTHERVIDEOPUBLISHERS",
        "IMPRESSIONSSOCIALMETA",
        "INVESTMENTSGOOGLESEARCH",
        "IMPRESSIONSINFLUENCERMARKETINGINSTAGRAM",
        "INVESTMENTSTV_GOOGLESEARCH",
        "INVESTMENTSTV_SOCIALMETA",
        "INVESTMENTSSOCIALMETA_YOUTUBE",
        "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS",
        "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM",
        "INVESTMENTSAMAZONSEARCH",
    ],
    exclude_vars=["AVERAGEPRICEBRAND", "AVERAGEPROMOPRICE"],
    organic_vars=[
        "TOTEMS",
        "FREESAMPLES",
        "EDITORIALS",
        "POSITIVEOPINIONS",
        "NEGATIVEOPINIONS",
        "ONLINEREVIEWS",
        "TOTEMDISPLAYNEXTTOCASHIER",
        "TOTEMDISPLAYINSTORE",
    ],
    window_start="2020-01-05",
    window_end="2023-12-24",
    context_signs=[
        "default",
        "default",
        "positive",
        "positive",
        "positive",
        "default",
        "default",
        "default",
        "positive",
    ],
    adstock="geometric",
)

# ── Hyperparameter names ──
InputCollect2.hyper_names()

# ── Hyperparameter ranges (Model 2) ──
hyperparameters2 = {
    "INVESTMENTSTV_alphas": [0.5, 3],
    "INVESTMENTSTV_gammas": [0.3, 1],
    "INVESTMENTSTV_thetas": [0.1, 0.99],
    "INVESTMENTSYOUTUBE_alphas": [0.5, 3],
    "INVESTMENTSYOUTUBE_gammas": [0.3, 1],
    "INVESTMENTSYOUTUBE_thetas": [0.1, 0.99],
    "INVESTMENTSOTHERVIDEOPUBLISHERS_alphas": [0.5, 3],
    "INVESTMENTSOTHERVIDEOPUBLISHERS_gammas": [0.3, 1],
    "INVESTMENTSOTHERVIDEOPUBLISHERS_thetas": [0.1, 0.99],
    "INVESTMENTSSOCIALMETA_alphas": [0.5, 3],
    "INVESTMENTSSOCIALMETA_gammas": [0.3, 1],
    "INVESTMENTSSOCIALMETA_thetas": [0.1, 0.99],
    "INVESTMENTSGOOGLESEARCH_alphas": [0.5, 3],
    "INVESTMENTSGOOGLESEARCH_gammas": [0.3, 1],
    "INVESTMENTSGOOGLESEARCH_thetas": [0.1, 0.99],
    "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_alphas": [0.5, 3],
    "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_gammas": [0.3, 1],
    "INVESTMENTSINFLUENCERMARKETINGINSTAGRAM_thetas": [0.1, 0.99],
    "INVESTMENTSTV_GOOGLESEARCH_alphas": [0.5, 3],
    "INVESTMENTSTV_GOOGLESEARCH_gammas": [0.3, 1],
    "INVESTMENTSTV_GOOGLESEARCH_thetas": [0.1, 0.99],
    "INVESTMENTSTV_SOCIALMETA_alphas": [0.5, 3],
    "INVESTMENTSTV_SOCIALMETA_gammas": [0.3, 1],
    "INVESTMENTSTV_SOCIALMETA_thetas": [0.1, 0.99],
    "INVESTMENTSSOCIALMETA_YOUTUBE_alphas": [0.5, 3],
    "INVESTMENTSSOCIALMETA_YOUTUBE_gammas": [0.3, 1],
    "INVESTMENTSSOCIALMETA_YOUTUBE_thetas": [0.1, 0.99],
    "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS_alphas": [0.5, 3],
    "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS_gammas": [0.3, 1],
    "INVESTMENTSSOCIALMETA_OTHERVIDEOPUBLISHERS_thetas": [0.1, 0.99],
    "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM_alphas": [0.5, 3],
    "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM_gammas": [0.3, 1],
    "INVESTMENTSSOCIALMETA_INFLUENCERMARKETINGINSTAGRAM_thetas": [0.1, 0.99],
    "INVESTMENTSAMAZONSEARCH_alphas": [0.5, 3],
    "INVESTMENTSAMAZONSEARCH_gammas": [0.3, 1],
    "INVESTMENTSAMAZONSEARCH_thetas": [0.1, 0.99],
    "TOTEMS_alphas": [0.5, 3],
    "TOTEMS_gammas": [0.3, 1],
    "TOTEMS_thetas": [0.1, 0.99],
    "FREESAMPLES_alphas": [0.5, 3],
    "FREESAMPLES_gammas": [0.3, 1],
    "FREESAMPLES_thetas": [0.1, 0.99],
    "EDITORIALS_alphas": [0.5, 3],
    "EDITORIALS_gammas": [0.3, 1],
    "EDITORIALS_thetas": [0.1, 0.99],
    "POSITIVEOPINIONS_alphas": [0.5, 3],
    "POSITIVEOPINIONS_gammas": [0.3, 1],
    "POSITIVEOPINIONS_thetas": [0.1, 0.99],
    "NEGATIVEOPINIONS_alphas": [0.5, 3],
    "NEGATIVEOPINIONS_gammas": [0.3, 1],
    "NEGATIVEOPINIONS_thetas": [0.1, 0.99],
    "ONLINEREVIEWS_alphas": [0.5, 3],
    "ONLINEREVIEWS_gammas": [0.3, 1],
    "ONLINEREVIEWS_thetas": [0.1, 0.99],
    "TOTEMDISPLAYNEXTTOCASHIER_alphas": [0.5, 3],
    "TOTEMDISPLAYNEXTTOCASHIER_gammas": [0.3, 1],
    "TOTEMDISPLAYNEXTTOCASHIER_thetas": [0.1, 0.99],
    "TOTEMDISPLAYINSTORE_alphas": [0.5, 3],
    "TOTEMDISPLAYINSTORE_gammas": [0.3, 1],
    "TOTEMDISPLAYINSTORE_thetas": [0.1, 0.99],
    "train_size": [0.5, 0.8],
}

InputCollect2.set_hyperparameters(hyperparameters2)

# ── Step 3: Build initial model (Model 2) ──
OutputModels2 = InputCollect2.run(
    cores=None,
    iterations=2000,
    trials=5,
    ts_validation=True,
    add_penalty_factor=False,
)
print(OutputModels2)

# Convergence plots
OutputModels2.convergence["moo_distrb_plot"].show()
OutputModels2.convergence["moo_cloud_plot"].show()

# Time-series validation plot
if OutputModels2.ts_validation:
    OutputModels2.ts_validation_plot.show()

# ── Pareto outputs (Model 2) ──
OutputCollect2 = OutputModels2.get_outputs(
    pareto_fronts=2,
    csv_out="pareto",
    clusters=True,
    export=CREATE_FILES,
    plot_folder=ROBYN_DIRECTORY,
    plot_pareto=CREATE_FILES,
)
print(OutputCollect2)

# ── Select best model (Model 2) ──
select_model2 = "5_159_6"  # ← Replace with actual model ID from OutputCollect2

ExportedModel2 = OutputCollect2.write(select_model2, export=CREATE_FILES)
print(ExportedModel2)

# One-pager
myOnePager2 = OutputCollect2.onepagers(select_model2, export=False)

# Check media summary
print(ExportedModel2)
print(InputCollect2.paid_media_spends)

# ── Step 5: Budget Allocation (Model 2) ──
AllocatorCollect2 = RobynAllocator(
    InputCollect=InputCollect2,
    OutputCollect=OutputCollect2,
    select_model=select_model2,
    date_range=["2023-01-01", "2023-12-24"],
    channel_constr_low=0.7,
    channel_constr_up=1.5,
    scenario="max_response",
    export=CREATE_FILES,
)
print(AllocatorCollect2)
AllocatorCollect2.plot()
