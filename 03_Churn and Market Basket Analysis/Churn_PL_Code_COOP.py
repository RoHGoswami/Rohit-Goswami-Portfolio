### Churn Prediction

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (
    confusion_matrix,
    accuracy_score,
    classification_report,
    roc_curve,
    roc_auc_score,
    silhouette_samples,
)
import statsmodels.api as sm
import statsmodels.formula.api as smf
import warnings

warnings.filterwarnings("ignore")

# ══════════════════════════════════════════════
#  Load & Clean Data
# ══════════════════════════════════════════════
data_B = pd.read_csv("Dataset_CRM_TypeB.csv")
print(data_B.dtypes)

data_B["date"] = pd.to_datetime(data_B["date"], format="%Y-%m-%d")
data_B["PL_gross_sales"] = (
    data_B["PL_gross_sales"]
    .astype(str)
    .str.replace(",", ".", regex=False)
    .astype(float)
)
data_B["number_item_PL"] = pd.to_numeric(
    data_B["number_item_PL"], errors="coerce"
).astype("Int64")
data_B["number_items_other"] = pd.to_numeric(
    data_B["number_items_other"], errors="coerce"
).astype("Int64")

# ══════════════════════════════════════════════
#  Anomaly Summary
# ══════════════════════════════════════════════
total_rows = len(data_B)
anomalies_summary_B = pd.DataFrame(
    {
        "Anomaly": [
            "Negative values in net_sales",
            "Negative values in gross_sales",
            "Negative values in PL_gross_sales",
            "If number_item_PL is NA, PL_gross_sales != 0",
            "PL_gross_sales > net_sales",
            "PL_gross_sales > gross_sales",
            "net_sales > gross_sales",
            "Both number_item_PL and number_items_other == 0",
        ],
        "Count": [
            (data_B["net_sales"] < 0).sum(),
            (data_B["gross_sales"] < 0).sum(),
            (data_B["PL_gross_sales"] < 0).sum(),
            (data_B["number_item_PL"].isna() & (data_B["PL_gross_sales"] != 0)).sum(),
            (data_B["PL_gross_sales"] > data_B["net_sales"]).sum(),
            (data_B["PL_gross_sales"] > data_B["gross_sales"]).sum(),
            (data_B["net_sales"] > data_B["gross_sales"]).sum(),
            (
                (data_B["number_item_PL"] == 0) & (data_B["number_items_other"] == 0)
            ).sum(),
        ],
    }
)
anomalies_summary_B["Percentage"] = anomalies_summary_B["Count"] / total_rows * 100
print(anomalies_summary_B)

# ── Transactions per month ──
data_B["Month"] = data_B["date"].dt.to_period("M").astype(str)
txn_per_month = data_B.groupby("Month").size().reset_index(name="Transactions")
print(txn_per_month)

fig, ax = plt.subplots(figsize=(12, 4))
ax.plot(txn_per_month["Month"], txn_per_month["Transactions"], "-o")
ax.set_xlabel("Month")
ax.set_ylabel("Number of Transactions")
ax.set_title("Transactions per Calendar Month")
plt.xticks(rotation=45, ha="right")
plt.tight_layout()
plt.show()

# ══════════════════════════════════════════════
#  Customer-Level Aggregation
# ══════════════════════════════════════════════
reference_date = data_B["date"].max()
first_date = data_B["date"].min()


def aggregate_customer(group):
    dates_sorted = group["date"].sort_values()
    unique_dates = dates_sorted.drop_duplicates().sort_values()
    freq = unique_dates.nunique()
    first_purch = group["date"].min()
    last_purch = group["date"].max()
    monetary = group["gross_sales"].sum()
    recency = (reference_date - last_purch).days

    avg_ipt = (last_purch - first_purch).days / (freq - 1) if freq > 1 else np.nan
    diffs = np.diff(unique_dates.values).astype("timedelta64[D]").astype(float)
    std_ipt = np.std(diffs, ddof=1) if freq > 1 and len(diffs) > 1 else np.nan

    avg_ticket = group["gross_sales"].mean()
    std_ticket = group["gross_sales"].std()
    num_other = group["number_items_other"].sum()
    num_pl = group["number_item_PL"].sum()
    avg_item_other = group["number_items_other"].mean()
    avg_item_pl = group["number_item_PL"].mean()
    pl_ratio = (
        avg_item_pl / (avg_item_pl + avg_item_other)
        if (avg_item_pl + avg_item_other) > 0
        else np.nan
    )

    # Most frequent store type
    store_type_variety = (
        group["store_type"].mode().iloc[0] if "store_type" in group.columns else np.nan
    )

    # Average discount
    discount = (1 - group["net_sales"] / group["gross_sales"]).replace(
        [np.inf, -np.inf], np.nan
    )
    avg_discount = discount.mean()

    # Purchase months (30-day intervals)
    day_diffs = (group["date"] - first_purch).dt.days
    purchase_months = (day_diffs // 30).nunique()

    regularity = std_ipt  # same as STD_ipt
    purchased_other_only = (num_pl == 0) and (num_other > 0)

    return pd.Series(
        {
            "Monetary_Value": monetary,
            "Frequency": freq,
            "Recency": recency,
            "First_Purchase": first_purch,
            "Last_Purchase": last_purch,
            "AVG_ipt": avg_ipt,
            "STD_ipt": std_ipt,
            "AVG_Ticket": avg_ticket,
            "STD_Ticket": std_ticket,
            "Number_item_other": num_other,
            "Number_item_PL": num_pl,
            "AVG_item_other_per_transaction": avg_item_other,
            "AVG_item_PL_per_transaction": avg_item_pl,
            "PL_ratio": pl_ratio,
            "Store_Type_Variety": store_type_variety,
            "AVG_Discount": avg_discount,
            "Purchase_Months": purchase_months,
            "Regularity": regularity,
            "Purchased_Other_Only": purchased_other_only,
        }
    )


Customers = data_B.groupby("id_customer").apply(aggregate_customer).reset_index()

# ══════════════════════════════════════════════
#  Churn Identification (rolling CI approach)
# ══════════════════════════════════════════════

# Step 1: Assign 31-day period IDs
data_B["Period_ID"] = ((data_B["date"] - data_B["date"].min()).dt.days // 31).astype(
    int
)

# Step 2: Aggregate per customer × period
Customers_Period = (
    data_B.groupby(["id_customer", "Period_ID"])
    .apply(
        lambda g: pd.Series(
            {
                "Monetary_Value": g["gross_sales"].sum(),
                "Purchases": len(g),
                "AVG_ipt": (
                    (g["date"].max() - g["date"].min()).days / (len(g) - 1)
                    if len(g) > 1
                    else np.nan
                ),
                "STD_ipt": (
                    np.std(
                        np.diff(g["date"].sort_values().drop_duplicates().values)
                        .astype("timedelta64[D]")
                        .astype(float),
                        ddof=1,
                    )
                    if g["date"].nunique() > 1
                    else np.nan
                ),
                "AVG_Ticket": g["gross_sales"].mean(),
                "STD_Ticket": g["gross_sales"].std(),
            }
        )
    )
    .reset_index()
)

# Step 3: Confidence intervals
Customers_Period["Predicted_Monetary_Value"] = Customers_Period["AVG_Ticket"] * (
    31 / Customers_Period["AVG_ipt"]
)
Customers_Period["Std_Error"] = np.sqrt(
    (Customers_Period["STD_Ticket"] ** 2 / Customers_Period["Purchases"])
    + (
        Customers_Period["STD_ipt"] ** 2
        * (Customers_Period["AVG_Ticket"] / (Customers_Period["AVG_ipt"] ** 2)) ** 2
    )
)

Customers_Period["CI_Lower"] = np.where(
    Customers_Period["Purchases"] > 1,
    Customers_Period["Predicted_Monetary_Value"] - 1.96 * Customers_Period["Std_Error"],
    Customers_Period["AVG_Ticket"] * 0.8,
)
Customers_Period["CI_Upper"] = np.where(
    Customers_Period["Purchases"] > 1,
    Customers_Period["Predicted_Monetary_Value"] + 1.96 * Customers_Period["Std_Error"],
    Customers_Period["AVG_Ticket"] * 1.2,
)
Customers_Period["CI_Lower"] = Customers_Period["CI_Lower"].fillna(
    Customers_Period["AVG_Ticket"] * 0.8
)
Customers_Period["CI_Upper"] = Customers_Period["CI_Upper"].fillna(
    Customers_Period["AVG_Ticket"] * 1.2
)

# Step 4: Cumulative / rolling CI
Customers_Period = Customers_Period.sort_values(["id_customer", "Period_ID"])
Customers_Period["Cumulative_Sum_Lower"] = Customers_Period.groupby("id_customer")[
    "CI_Lower"
].cumsum()
Customers_Period["Cumulative_Sum_Upper"] = Customers_Period.groupby("id_customer")[
    "CI_Upper"
].cumsum()
Customers_Period["Cumulative_Count"] = (
    Customers_Period.groupby("id_customer").cumcount() + 1
)

Customers_Period["Rolling_CI_Lower"] = (
    Customers_Period.groupby("id_customer")
    .apply(lambda g: (g["Cumulative_Sum_Lower"] / g["Cumulative_Count"]).shift(1))
    .reset_index(level=0, drop=True)
)
Customers_Period["Rolling_CI_Upper"] = (
    Customers_Period.groupby("id_customer")
    .apply(lambda g: (g["Cumulative_Sum_Upper"] / g["Cumulative_Count"]).shift(1))
    .reset_index(level=0, drop=True)
)

Customers_Period["Combined_CI_Lower"] = Customers_Period[
    ["CI_Lower", "Rolling_CI_Lower"]
].min(axis=1)
Customers_Period["Combined_CI_Upper"] = Customers_Period[
    ["CI_Upper", "Rolling_CI_Upper"]
].max(axis=1)

# Step 5: Underperforming flag
Customers_Period["Underperforming"] = (
    Customers_Period["Monetary_Value"] < Customers_Period["Combined_CI_Lower"]
).astype(int)


# Step 6: Dynamic churn detection (last 3 periods underperforming)
def detect_churn(group):
    group = group.sort_values("Period_ID", ascending=False)
    if len(group) >= 3 and group["Underperforming"].iloc[:3].sum() == 3:
        group["Churned"] = 1
    else:
        group["Churned"] = 0
    return group


Customers_Period = (
    Customers_Period.groupby("id_customer").apply(detect_churn).reset_index(drop=True)
)
Customers_Period = Customers_Period.sort_values(["id_customer", "Period_ID"])

churned_ids = Customers_Period[Customers_Period["Churned"] > 0]["id_customer"].unique()
total_churned = len(churned_ids)
print(f"Total churned customers: {total_churned}")

# Assign churn flag to Customers
Customers["Churn"] = np.where(
    Customers["id_customer"].isin(churned_ids) | (Customers["Recency"] > 31), 1, 0
)
num_churned = (Customers["Churn"] == 1).sum()
print(f"Churned customers (incl. recency > 31): {num_churned}")

# ── Merge with RFM segments ──
merged_customers = pd.read_csv("merged_customers.csv")
Customers = Customers.merge(
    merged_customers[["id_customer", "Final_Segment"]], on="id_customer", how="left"
)
Customers["Final_Segment"] = Customers["Final_Segment"].fillna("Not Clusterized")


# ══════════════════════════════════════════════
#  LOGISTIC REGRESSION
# ══════════════════════════════════════════════

Customers_saved = pd.read_csv("Customers_Final.csv")  # Or use Customers directly

logistic_cols_v1 = [
    "Churn",
    "Frequency",
    "Monetary_Value",
    "AVG_ipt",
    "STD_ipt",
    "AVG_Ticket",
    "Number_item_other",
    "Number_item_PL",
    "AVG_item_other_per_transaction",
    "AVG_item_PL_per_transaction",
    "PL_ratio",
    "AVG_Discount",
    "Purchase_Months",
    "Final_Segment",
    "Recency",
    "STD_Ticket",
    "Store_Type_Variety",
    "Regularity",
]
logistic_data = Customers_saved[logistic_cols_v1].dropna()

# Correlation matrix (numeric only)
numeric_cols = (
    logistic_data.select_dtypes(include=[np.number]).drop(columns=["Churn"]).columns
)
corr_matrix = logistic_data[numeric_cols].corr()
print(corr_matrix)

fig, ax = plt.subplots(figsize=(12, 10))
sns.heatmap(
    corr_matrix, annot=True, fmt=".2f", cmap="coolwarm", ax=ax, cbar_kws={"shrink": 0.8}
)
ax.set_title("Correlation Matrix")
plt.tight_layout()
plt.savefig("correlation_matrix.png", dpi=150)
plt.show()

# ── Reduced feature set (after removing correlated vars) ──
logistic_cols_v2 = [
    "Churn",
    "Frequency",
    "Monetary_Value",
    "AVG_item_PL_per_transaction",
    "PL_ratio",
    "AVG_Discount",
    "Purchase_Months",
    "Final_Segment",
    "STD_Ticket",
    "Store_Type_Variety",
]
logistic_data = Customers_saved[logistic_cols_v2].dropna()

# Scale numeric features
scale_cols = [
    "Frequency",
    "Monetary_Value",
    "AVG_item_PL_per_transaction",
    "STD_Ticket",
]
scaler = StandardScaler()
logistic_data[scale_cols] = scaler.fit_transform(logistic_data[scale_cols])

# Encode categoricals for statsmodels formula
logistic_data["Churn"] = logistic_data["Churn"].astype(int)

formula = (
    "Churn ~ Frequency + Monetary_Value + AVG_item_PL_per_transaction + PL_ratio "
    "+ AVG_Discount + Purchase_Months + C(Final_Segment) + STD_Ticket + C(Store_Type_Variety)"
)
mod = smf.logit(formula, data=logistic_data).fit(disp=0)
print(mod.summary())

# Pseudo R²
r2_log = 1 - mod.deviance / mod.null_deviance
print(f"Pseudo R²: {r2_log:.4f}")

# Odds ratios
odds_ratios = np.exp(mod.params)
print("\nOdds Ratios:")
print(odds_ratios)

# Predicted probabilities
logistic_data["predicted_probabilities"] = mod.predict(logistic_data)

# ── Threshold analysis ──
thresholds = np.arange(0.20, 0.91, 0.05)
metrics = {
    "threshold": [],
    "sensitivity": [],
    "specificity": [],
    "accuracy": [],
    "aper": [],
}

for t in thresholds:
    pred = (logistic_data["predicted_probabilities"] > t).astype(int)
    cm = confusion_matrix(logistic_data["Churn"], pred, labels=[0, 1])
    tn, fp, fn, tp = cm.ravel()
    sens = tp / (tp + fn) if (tp + fn) > 0 else 0
    spec = tn / (tn + fp) if (tn + fp) > 0 else 0
    acc = (tp + tn) / len(logistic_data)
    metrics["threshold"].append(t)
    metrics["sensitivity"].append(sens)
    metrics["specificity"].append(spec)
    metrics["accuracy"].append(acc)
    metrics["aper"].append(1 - acc)

metrics_df = pd.DataFrame(metrics)

fig, ax = plt.subplots(figsize=(8, 5))
ax.plot(
    metrics_df["threshold"], metrics_df["sensitivity"], "r-", lw=2, label="Sensitivity"
)
ax.plot(
    metrics_df["threshold"], metrics_df["specificity"], "g-", lw=2, label="Specificity"
)
ax.plot(metrics_df["threshold"], metrics_df["accuracy"], "b-", lw=2, label="Accuracy")
ax.plot(
    metrics_df["threshold"],
    metrics_df["aper"],
    "m-",
    lw=2,
    label="Misclassification Error",
)
ax.set_xlabel("Threshold")
ax.set_ylabel("Metrics")
ax.set_title("Threshold Analysis")
ax.legend()
plt.tight_layout()
plt.savefig("threshold_analysis.png", dpi=150)
plt.show()

best_acc_thresh = metrics_df.loc[metrics_df["accuracy"].idxmax(), "threshold"]
best_balance_thresh = metrics_df.loc[
    (metrics_df["sensitivity"] - metrics_df["specificity"]).abs().idxmin(), "threshold"
]
print(f"Best Threshold (Max Accuracy): {best_acc_thresh}")
print(f"Best Threshold (Sens-Spec Balance): {best_balance_thresh}")

# Final classification at threshold = 0.2
logistic_data["predicted_class"] = (
    logistic_data["predicted_probabilities"] > 0.2
).astype(int)
cm = confusion_matrix(
    logistic_data["Churn"], logistic_data["predicted_class"], labels=[0, 1]
)
print("Confusion Matrix (Logistic Regression):")
print(cm)

tn, fp, fn, tp = cm.ravel()
print(f"Accuracy:    {(tp + tn) / cm.sum():.4f}")
print(f"Sensitivity: {tp / (tp + fn):.4f}")
print(f"Specificity: {tn / (tn + fp):.4f}")


# ══════════════════════════════════════════════
#  RANDOM FOREST
# ══════════════════════════════════════════════

rf_cols = [
    "Churn",
    "Frequency",
    "Monetary_Value",
    "AVG_ipt",
    "STD_ipt",
    "AVG_Ticket",
    "Number_item_other",
    "Number_item_PL",
    "AVG_item_other_per_transaction",
    "AVG_item_PL_per_transaction",
    "PL_ratio",
    "AVG_Discount",
    "Purchase_Months",
    "Final_Segment",
    "STD_Ticket",
    "Store_Type_Variety",
    "Regularity",
]
rf_data = Customers_saved[rf_cols].dropna()

# Encode categoricals
le_seg = LabelEncoder()
le_store = LabelEncoder()
rf_data["Final_Segment"] = le_seg.fit_transform(rf_data["Final_Segment"])
rf_data["Store_Type_Variety"] = le_store.fit_transform(rf_data["Store_Type_Variety"])

# Scale numeric features
scale_rf = [
    c
    for c in rf_data.columns
    if c not in ["Churn", "Final_Segment", "Store_Type_Variety"]
]
rf_data[scale_rf] = StandardScaler().fit_transform(rf_data[scale_rf])

# Stratified train/test split (80/20)
np.random.seed(123)
X = rf_data.drop(columns=["Churn"])
y = rf_data["Churn"].astype(int)

from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, stratify=y, random_state=123
)

# Fit Random Forest
rf_model = RandomForestClassifier(
    n_estimators=40,
    max_features="sqrt",
    random_state=123,
    n_jobs=-1,
)
rf_model.fit(X_train, y_train)
print(
    f"\nRandom Forest OOB-like Train Accuracy: {rf_model.score(X_train, y_train):.4f}"
)

# Feature importance
feat_imp = pd.DataFrame(
    {
        "Variable": X.columns,
        "Importance": rf_model.feature_importances_,
    }
).sort_values("Importance", ascending=False)
print("\nRandom Forest Feature Importance:")
print(feat_imp.to_string(index=False))

fig, ax = plt.subplots(figsize=(8, 6))
sns.barplot(x="Importance", y="Variable", data=feat_imp, ax=ax)
ax.set_title("Variable Importance (Random Forest)")
plt.tight_layout()
plt.savefig("rf_importance.png", dpi=150)
plt.show()

# Predictions
rf_pred = rf_model.predict(X_test)
cm_rf = confusion_matrix(y_test, rf_pred, labels=[0, 1])
print("\nConfusion Matrix (Random Forest):")
print(cm_rf)

tn, fp, fn, tp = cm_rf.ravel()
print(f"Accuracy:    {(tp + tn) / cm_rf.sum():.4f}")
print(f"Sensitivity: {tp / (tp + fn):.4f}")
print(f"Specificity: {tn / (tn + fp):.4f}")


# ══════════════════════════════════════════════
#  XGBOOST
# ══════════════════════════════════════════════
import xgboost as xgb

xgb_cols = [
    "Churn",
    "Frequency",
    "Monetary_Value",
    "AVG_ipt",
    "STD_ipt",
    "AVG_Ticket",
    "Number_item_other",
    "Number_item_PL",
    "AVG_item_other_per_transaction",
    "AVG_item_PL_per_transaction",
    "PL_ratio",
    "AVG_Discount",
    "Purchase_Months",
    "Final_Segment",
    "STD_Ticket",
    "Store_Type_Variety",
    "Regularity",
]
xgb_data = Customers_saved[xgb_cols].dropna()

# Encode Final_Segment numerically
segment_map = {
    "Highly Active and Recent": 1,
    "Super Customer": 2,
    "Consistently Moderate": 3,
    "Not Clusterized": 4,
    "Low Engagement": 5,
}
xgb_data["Final_Segment"] = xgb_data["Final_Segment"].map(segment_map)
xgb_data["Store_Type_Variety"] = le_store.transform(xgb_data["Store_Type_Variety"])
xgb_data = xgb_data.dropna()

# Scale
scale_xgb = [c for c in xgb_data.columns if c != "Churn"]
xgb_data[scale_xgb] = StandardScaler().fit_transform(xgb_data[scale_xgb])
xgb_data["Churn"] = xgb_data["Churn"].astype(int)

X_xgb = xgb_data.drop(columns=["Churn"])
y_xgb = xgb_data["Churn"]

X_train_xgb, X_test_xgb, y_train_xgb, y_test_xgb = train_test_split(
    X_xgb, y_xgb, test_size=0.2, stratify=y_xgb, random_state=123
)

dtrain = xgb.DMatrix(X_train_xgb, label=y_train_xgb)
dtest = xgb.DMatrix(X_test_xgb, label=y_test_xgb)

xgb_params = {
    "objective": "binary:logistic",
    "eval_metric": "logloss",
    "max_depth": 6,
    "eta": 0.1,
    "gamma": 0,
    "subsample": 0.8,
    "colsample_bytree": 0.8,
    "alpha": 0.1,
    "lambda": 1,
}

xgb_model = xgb.train(
    xgb_params,
    dtrain,
    num_boost_round=150,
    evals=[(dtrain, "train"), (dtest, "test")],
    verbose_eval=10,
)

# Predictions
pred_probs_xgb = xgb_model.predict(dtest)
pred_labels_xgb = (pred_probs_xgb > 0.439).astype(int)

cm_xgb = confusion_matrix(y_test_xgb, pred_labels_xgb, labels=[0, 1])
print("\nConfusion Matrix (XGBoost):")
print(cm_xgb)

tn, fp, fn, tp = cm_xgb.ravel()
print(f"Accuracy:    {(tp + tn) / cm_xgb.sum():.4f}")
print(f"Sensitivity: {tp / (tp + fn):.4f}")
print(f"Specificity: {tn / (tn + fp):.4f}")

# ROC / AUC
auc_xgb = roc_auc_score(y_test_xgb, pred_probs_xgb)
print(f"AUC: {auc_xgb:.4f}")

fpr, tpr, _ = roc_curve(y_test_xgb, pred_probs_xgb)
fig, ax = plt.subplots(figsize=(6, 5))
ax.plot(fpr, tpr, "b-", lw=2, label=f"AUC = {auc_xgb:.4f}")
ax.plot([0, 1], [0, 1], "k--")
ax.set_xlabel("False Positive Rate")
ax.set_ylabel("True Positive Rate")
ax.set_title("ROC Curve (XGBoost)")
ax.legend()
plt.tight_layout()
plt.savefig("roc_xgboost.png", dpi=150)
plt.show()

# Feature importance (XGBoost)
xgb_imp = xgb_model.get_score(importance_type="gain")
xgb_imp_df = pd.DataFrame({"Variable": xgb_imp.keys(), "Importance": xgb_imp.values()})
xgb_imp_df = xgb_imp_df.sort_values("Importance", ascending=False)
print("\nXGBoost Feature Importance:")
print(xgb_imp_df.to_string(index=False))

fig, ax = plt.subplots(figsize=(8, 6))
sns.barplot(x="Importance", y="Variable", data=xgb_imp_df, ax=ax)
ax.set_title("Variable Importance (XGBoost)")
plt.tight_layout()
plt.savefig("xgb_importance.png", dpi=150)
plt.show()


# ══════════════════════════════════════════════
#  Variable Importance Aggregation
# ══════════════════════════════════════════════

# Logistic Regression importance (by p-value rank + |coefficient|)
logit_params = mod.summary2().tables[1].reset_index()
logit_params.columns = ["Variable", "Coef", "StdErr", "z", "P>|z|", "ci_lo", "ci_hi"]
logit_params["Variable"] = logit_params["Variable"].apply(
    lambda x: "Final_Segment"
    if "Final_Segment" in str(x)
    else ("Store_Type_Variety" if "Store_Type_Variety" in str(x) else x)
)
logit_params = logit_params[logit_params["Variable"] != "Intercept"]
logit_imp = (
    logit_params.groupby("Variable")
    .agg(PValueRank=("P>|z|", "min"), Importance=("Coef", lambda x: x.abs().sum()))
    .reset_index()
)
logit_imp["PValueRank"] = pd.cut(
    logit_imp["PValueRank"], bins=[-1, 0.001, 0.01, 0.05, 1], labels=[1, 2, 3, 4]
).astype(int)
logit_imp = logit_imp.sort_values(
    ["PValueRank", "Importance"], ascending=[True, False]
).head(17)
logit_imp["Rank"] = range(1, len(logit_imp) + 1)
logit_imp["Method"] = "Logistic Regression"

# RF importance
rf_imp = feat_imp.head(17).copy()
rf_imp["Rank"] = range(1, len(rf_imp) + 1)
rf_imp["Method"] = "Random Forest"

# XGBoost importance
xgb_imp_ranked = xgb_imp_df.head(17).copy()
xgb_imp_ranked["Rank"] = range(1, len(xgb_imp_ranked) + 1)
xgb_imp_ranked["Method"] = "XGBoost"

# Combine
all_imp = pd.concat(
    [
        logit_imp[["Rank", "Method", "Variable"]],
        rf_imp[["Rank", "Method", "Variable"]],
        xgb_imp_ranked[["Rank", "Method", "Variable"]],
    ],
    ignore_index=True,
)

# Fill missing combos with rank 21
all_vars = all_imp["Variable"].unique()
all_methods = all_imp["Method"].unique()
full_grid = pd.MultiIndex.from_product(
    [all_vars, all_methods], names=["Variable", "Method"]
).to_frame(index=False)
all_imp = full_grid.merge(all_imp, on=["Variable", "Method"], how="left")
all_imp["Rank"] = all_imp["Rank"].fillna(21)

final_ranks = (
    all_imp.groupby("Variable")["Rank"].mean().reset_index(name="AggregateRank")
)
final_ranks = final_ranks.sort_values("AggregateRank")
final_ranks["FinalRank"] = range(1, len(final_ranks) + 1)

importance_wide = all_imp.pivot(
    index="Variable", columns="Method", values="Rank"
).reset_index()
importance_wide = importance_wide.merge(final_ranks, on="Variable")
importance_wide = importance_wide.sort_values("FinalRank")
print("\n=== Final Variable Ranking ===")
print(importance_wide.to_string(index=False))


# ══════════════════════════════════════════════
#  Survival Analysis (Kaplan–Meier)
# ══════════════════════════════════════════════
from lifelines import KaplanMeierFitter
from lifelines.statistics import logrank_test

surv_data = Customers_saved[["Churn", "Purchase_Months", "Final_Segment"]].dropna()
surv_data["Churn"] = surv_data["Churn"].astype(int)

kmf = KaplanMeierFitter()
kmf.fit(surv_data["Purchase_Months"], event_observed=surv_data["Churn"])

fig, ax = plt.subplots(figsize=(8, 5))
kmf.plot_survival_function(ax=ax, ci_show=True)
ax.set_xlabel("Time (Months)")
ax.set_ylabel("Survival Probability")
ax.set_title("Kaplan-Meier Survival Curve")
plt.tight_layout()
plt.savefig("km_curve.png", dpi=150)
plt.show()

# KM by segment
fig, ax = plt.subplots(figsize=(9, 6))
for seg in surv_data["Final_Segment"].unique():
    sub = surv_data[surv_data["Final_Segment"] == seg]
    kmf_seg = KaplanMeierFitter()
    kmf_seg.fit(sub["Purchase_Months"], event_observed=sub["Churn"], label=seg)
    kmf_seg.plot_survival_function(ax=ax, ci_show=True)

ax.set_xlabel("Time (Months)")
ax.set_ylabel("Survival Probability")
ax.set_title("Kaplan-Meier Survival Curves by Final Segment")
ax.legend(title="Customer Segment")
plt.tight_layout()
plt.savefig("km_by_segment.png", dpi=150)
plt.show()


# ══════════════════════════════════════════════
#  Partial Dependence Plots (RF-based)
# ══════════════════════════════════════════════
from sklearn.inspection import PartialDependenceDisplay

fig, ax = plt.subplots(figsize=(7, 5))
PartialDependenceDisplay.from_estimator(
    rf_model, X_train, features=["PL_ratio"], kind="average", ax=ax
)
ax.set_title("Partial Dependence: PL_ratio (Random Forest)")
plt.tight_layout()
plt.savefig("pdp_pl_ratio_rf.png", dpi=150)
plt.show()

fig, ax = plt.subplots(figsize=(7, 5))
PartialDependenceDisplay.from_estimator(
    rf_model, X_train, features=["AVG_item_PL_per_transaction"], kind="average", ax=ax
)
ax.set_title("Partial Dependence: AVG_item_PL_per_transaction (Random Forest)")
plt.tight_layout()
plt.savefig("pdp_avg_pl_rf.png", dpi=150)
plt.show()


# ══════════════════════════════════════════════
#  Descriptive Analysis
# ══════════════════════════════════════════════
Customers_1 = Customers_saved.copy()
Customers_1["Final_Segment"] = Customers_1["Final_Segment"].astype(str)
Customers_1["Purchase_Months"] = Customers_1["Purchase_Months"].astype(str)
Customers_1["Churn"] = Customers_1["Churn"].astype(str)

custom_palette = ["#75BDA7", "#58B6C0"]

# Pie chart of churn distribution
churn_summary = Customers_1.groupby("Churn").size().reset_index(name="Count")
churn_summary["Label"] = churn_summary["Churn"].map(
    {"1": "Churned", "0": "Not Churned"}
)

fig, ax = plt.subplots(figsize=(6, 6))
ax.pie(
    churn_summary["Count"],
    labels=churn_summary["Label"],
    colors=custom_palette,
    autopct="%1.1f%%",
    startangle=90,
    wedgeprops={"edgecolor": "white"},
)
ax.set_title("Proportion of Churned vs Not Churned Customers", fontweight="bold")
plt.tight_layout()
plt.savefig("churn_pie.png", dpi=150)
plt.show()


def plot_variable(data, variable, target="Churn"):
    """Plot a variable by churn status (bar for categorical, density for numeric)."""
    fig, ax = plt.subplots(figsize=(8, 5))
    palette = ["#58B6C0", "#75BDA7"]

    if data[variable].dtype == object:
        ct = pd.crosstab(data[variable], data[target], normalize="index")
        ct.plot(kind="bar", stacked=True, color=palette, ax=ax, alpha=0.8)
        ax.set_ylabel("Proportion")
    else:
        for i, churn_val in enumerate(sorted(data[target].unique())):
            sub = data[data[target] == churn_val][variable].dropna()
            sub.plot(
                kind="kde",
                ax=ax,
                label=f"Churn={churn_val}",
                color=palette[i],
                alpha=0.6,
            )
        ax.set_ylabel("Density")

    ax.set_title(f"Distribution of {variable} by Churn Status")
    ax.set_xlabel(variable)
    ax.legend(title="Churn Status")
    plt.tight_layout()
    plt.show()


# Analyse target variable
target_var = "AVG_item_PL_per_transaction"
variables = [
    "Final_Segment",
    "Purchase_Months",
    "Frequency",
    "STD_Ticket",
    "Monetary_Value",
    "PL_ratio",
    "AVG_item_PL_per_transaction",
]

for var in variables:
    if var == target_var:
        plot_variable(Customers_1, var)
