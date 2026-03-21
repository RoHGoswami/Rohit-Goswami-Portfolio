### RFM COOP

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.metrics import silhouette_score, silhouette_samples

# ══════════════════════════════════════════════
#  Load & Clean Data
# ══════════════════════════════════════════════
data_B = pd.read_csv("Dataset_CRM_TypeB.csv")
print(data_B.dtypes)

# Convert date
data_B["date"] = pd.to_datetime(data_B["date"], format="%Y-%m-%d")

# Replace commas with dots in PL_gross_sales and convert to numeric
data_B["PL_gross_sales"] = (
    data_B["PL_gross_sales"]
    .astype(str)
    .str.replace(",", ".", regex=False)
    .astype(float)
)

# Convert item counts to integer (coerce errors to NaN → fill with 0)
data_B["number_item_PL"] = pd.to_numeric(
    data_B["number_item_PL"], errors="coerce"
).astype("Int64")
data_B["number_items_other"] = pd.to_numeric(
    data_B["number_items_other"], errors="coerce"
).astype("Int64")

print(data_B.head())

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
            "If number_item_PL is NA, PL_gross_sales different from 0",
            "PL_gross_sales > net_sales",
            "PL_gross_sales > gross_sales",
            "net_sales > gross_sales",
            "Both number_item_PL and number_items_other are equal to 0",
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

# Filter out net_sales > gross_sales anomaly
data_B = data_B[data_B["net_sales"] < data_B["gross_sales"]].copy()

reference_date = data_B["date"].max()
first_purchase = data_B["date"].min()

# ══════════════════════════════════════════════
#  RFM Customer-Level Aggregation
# ══════════════════════════════════════════════


def compute_customer_metrics(group):
    """Compute RFM and additional metrics per customer."""
    dates_sorted = group["date"].sort_values()
    unique_dates = dates_sorted.drop_duplicates().sort_values()
    freq = unique_dates.nunique()

    monetary = group["gross_sales"].sum()
    recency = (reference_date - group["date"].max()).days
    first_purch = group["date"].min()
    last_purch = group["date"].max()

    if freq > 1:
        avg_ipt = (last_purch - first_purch).days / (freq - 1)
        diffs = np.diff(unique_dates.values).astype("timedelta64[D]").astype(float)
        std_ipt = np.std(diffs, ddof=1) if len(diffs) > 1 else np.nan
    else:
        avg_ipt = np.nan
        std_ipt = np.nan

    avg_ticket = group["gross_sales"].mean()
    std_ticket = group["gross_sales"].std()
    num_other = group["number_items_other"].sum()
    num_pl = group["number_item_PL"].sum()

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
        }
    )


Customers = data_B.groupby("id_customer").apply(compute_customer_metrics).reset_index()
print(Customers.head())

# ══════════════════════════════════════════════
#  Step 1–2: Recency Analysis & Histogram
# ══════════════════════════════════════════════
print(Customers["Recency"].describe())
print("Recency Std Dev:", Customers["Recency"].std())

recency_quantiles = Customers["Recency"].quantile([0.25, 0.80]).values  # [q25, q80]

fig, ax = plt.subplots(figsize=(10, 5))
ax.hist(
    Customers["Recency"],
    bins=range(int(Customers["Recency"].max()) + 2),
    color="blue",
    edgecolor="black",
    alpha=0.7,
)
ax.axvline(
    recency_quantiles[0], color="red", linestyle="--", linewidth=1, label="25th %tile"
)
ax.axvline(
    recency_quantiles[1], color="green", linestyle="--", linewidth=1, label="80th %tile"
)
ax.set_xlabel("Recency (days)")
ax.set_ylabel("Frequency")
ax.set_title("Distribution of Recency")
ax.legend()
plt.tight_layout()
plt.savefig("recency_distribution.png", dpi=150)
plt.show()

# ── Outlier analysis with different multipliers ──
IQR_recency = recency_quantiles[1] - recency_quantiles[0]
multipliers = np.arange(1, 3.5, 0.5)

outlier_analysis = pd.DataFrame(
    {
        "Multiplier": multipliers,
        "Outliers_Count": [
            (Customers["Recency"] > recency_quantiles[1] + m * IQR_recency).sum()
            for m in multipliers
        ],
    }
)
outlier_analysis["Outlier_Percentage"] = (
    outlier_analysis["Outliers_Count"] / len(Customers) * 100
)

fig, ax = plt.subplots(figsize=(7, 4))
ax.plot(outlier_analysis["Multiplier"], outlier_analysis["Outliers_Count"], "b-o")
ax.set_xlabel("Multiplier")
ax.set_ylabel("Number of Outliers")
ax.set_title("Elbow Method to Identify Optimal Multiplier")
plt.tight_layout()
plt.savefig("elbow_multiplier.png", dpi=150)
plt.show()
print(outlier_analysis)

# ── Recency histogram with upper bound line (multiplier = 2) ──
upper_bound = recency_quantiles[1] + 2 * IQR_recency

fig, ax = plt.subplots(figsize=(10, 5))
ax.hist(
    Customers["Recency"],
    bins=range(int(Customers["Recency"].max()) + 2),
    color="blue",
    edgecolor="black",
    alpha=0.7,
)
ax.axvline(
    recency_quantiles[0], color="red", linestyle="--", linewidth=1, label="25th %tile"
)
ax.axvline(
    recency_quantiles[1], color="green", linestyle="--", linewidth=1, label="80th %tile"
)
ax.axvline(
    upper_bound,
    color="purple",
    linestyle=":",
    linewidth=1,
    label="Upper Bound (2x IQR)",
)
ax.set_xlabel("Recency (days)")
ax.set_ylabel("Frequency")
ax.set_title("Distribution of Recency")
ax.legend()
plt.tight_layout()
plt.show()

# ── Step 3–4: Remove recency outliers ──
upper_bound_2 = recency_quantiles[1] + 2 * IQR_recency
Customers_outliers_2 = Customers[Customers["Recency"] > upper_bound_2].copy()
Customers = Customers[Customers["Recency"] <= upper_bound_2].copy()

# ══════════════════════════════════════════════
#  Step 5: Identify Super Customers
# ══════════════════════════════════════════════
print(Customers["Frequency"].describe())
print(Customers["Monetary_Value"].describe())

freq_IQR = Customers["Frequency"].quantile(0.75) - Customers["Frequency"].quantile(0.25)
monetary_IQR = Customers["Monetary_Value"].quantile(0.75) - Customers[
    "Monetary_Value"
].quantile(0.25)

# Percentile elbow analysis
percentiles = np.arange(0.70, 1.00, 0.01)
dynamic_freq_q = Customers["Frequency"].quantile(percentiles)
dynamic_mon_q = Customers["Monetary_Value"].quantile(percentiles)

percentile_analysis = pd.DataFrame(
    {
        "Percentile": percentiles,
        "Freq_Outliers": [
            (Customers["Frequency"] > thresh).sum() for thresh in dynamic_freq_q
        ],
        "Monetary_Outliers": [
            (Customers["Monetary_Value"] > thresh).sum() for thresh in dynamic_mon_q
        ],
    }
)

fig, ax = plt.subplots(figsize=(8, 4))
ax.plot(
    percentile_analysis["Percentile"],
    percentile_analysis["Freq_Outliers"],
    label="Frequency Outliers",
)
ax.plot(
    percentile_analysis["Percentile"],
    percentile_analysis["Monetary_Outliers"],
    label="Monetary Outliers",
)
ax.set_xlabel("Percentile")
ax.set_ylabel("Number of Outliers")
ax.set_title("Elbow Method for Optimal Percentile")
ax.legend()
plt.tight_layout()
plt.show()

# Boxplots
fig, axes = plt.subplots(1, 2, figsize=(10, 4))
Customers.boxplot(column="Frequency", ax=axes[0])
axes[0].set_title("Boxplot of Frequency")
Customers.boxplot(column="Monetary_Value", ax=axes[1])
axes[1].set_title("Boxplot of Monetary Value")
plt.tight_layout()
plt.show()

# Selected percentile = 0.75
selected_percentile = 0.75
freq_upper_bound = Customers["Frequency"].quantile(selected_percentile)
monetary_upper_bound = Customers["Monetary_Value"].quantile(selected_percentile)

# Multiplier analysis
multipliers2 = np.arange(1, 3.5, 0.5)
outlier_analysis2 = pd.DataFrame({"Multiplier": multipliers2})
outlier_analysis2["Freq_Upper_Bound"] = (
    freq_upper_bound + outlier_analysis2["Multiplier"] * freq_IQR
)
outlier_analysis2["Monetary_Upper_Bound"] = (
    monetary_upper_bound + outlier_analysis2["Multiplier"] * monetary_IQR
)
outlier_analysis2["Freq_Outliers"] = [
    (Customers["Frequency"] > ub).sum() for ub in outlier_analysis2["Freq_Upper_Bound"]
]
outlier_analysis2["Monetary_Outliers"] = [
    (Customers["Monetary_Value"] > ub).sum()
    for ub in outlier_analysis2["Monetary_Upper_Bound"]
]
print(outlier_analysis2)

fig, ax = plt.subplots(figsize=(8, 4))
ax.plot(
    outlier_analysis2["Multiplier"],
    outlier_analysis2["Freq_Outliers"],
    "-o",
    label="Frequency Outliers",
)
ax.plot(
    outlier_analysis2["Multiplier"],
    outlier_analysis2["Monetary_Outliers"],
    "-o",
    label="Monetary Outliers",
)
ax.set_xlabel("Multiplier")
ax.set_ylabel("Number of Outliers")
ax.set_title("Optimal Multiplier for Outlier Detection")
ax.legend()
plt.tight_layout()
plt.show()

# Apply selected multiplier = 1.5
selected_multiplier = 1.5
freq_upper_bound = freq_upper_bound + selected_multiplier * freq_IQR
monetary_upper_bound = monetary_upper_bound + selected_multiplier * monetary_IQR

# Identify super customers
super_customer = Customers[
    (Customers["Frequency"] > freq_upper_bound)
    | (Customers["Monetary_Value"] > monetary_upper_bound)
].copy()
super_customer["anomaly"] = np.where(
    super_customer["Frequency"] > freq_upper_bound, "frequency", "monetary"
)

# Remove super customers from main dataset
Customers = Customers[
    (Customers["Frequency"] <= freq_upper_bound)
    & (Customers["Monetary_Value"] <= monetary_upper_bound)
].copy()

# ══════════════════════════════════════════════
#  RFM Scoring & Segmentation
# ══════════════════════════════════════════════
freq_quartiles = Customers["Frequency"].quantile([0.2, 0.5, 0.8]).values
monetary_quartiles = Customers["Monetary_Value"].quantile([0.2, 0.5, 0.8]).values
recency_quartiles_rfm = Customers["Recency"].quantile([0.2, 0.5, 0.8]).values


def label_monetary(val):
    if val <= monetary_quartiles[0]:
        return "L"
    elif val <= monetary_quartiles[1]:
        return "M"
    else:
        return "H"


def label_frequency(val):
    if val <= freq_quartiles[0]:
        return "L"
    elif val <= freq_quartiles[1]:
        return "M"
    else:
        return "H"


def label_recency(val):
    if val <= recency_quartiles_rfm[0]:
        return "H"  # recent = good
    elif val <= recency_quartiles_rfm[1]:
        return "M"
    else:
        return "L"


Customers["Monetary_Label"] = Customers["Monetary_Value"].apply(label_monetary)
Customers["Frequency_Label"] = Customers["Frequency"].apply(label_frequency)
Customers["Recency_Label"] = Customers["Recency"].apply(label_recency)


def assign_segment(row):
    r, f, m = row["Recency_Label"], row["Frequency_Label"], row["Monetary_Label"]
    # Champions
    if (
        (r == "H" and f == "H" and m == "H")
        or (r == "H" and f == "M" and m == "H")
        or (r == "H" and f == "H" and m == "M")
    ):
        return "Champions"
    # Loyal
    if (r == "M" and f == "H" and m == "H") or (r == "M" and f == "H" and m == "M"):
        return "Loyal"
    # Cannot Lose
    if (
        (r == "L" and f == "M" and m == "H")
        or (r == "L" and f == "H" and m == "H")
        or (r == "L" and f == "H" and m == "M")
    ):
        return "Cannot Lose"
    # Potential Loyalist
    if (
        (r == "H" and f == "M" and m == "M")
        or (r == "H" and f == "M" and m == "L")
        or (r == "H" and f == "L" and m == "M")
        or (r == "H" and f == "H" and m == "L")
    ):
        return "Potential Loyalist"
    # As New
    if (
        (r == "H" and f == "L" and m == "L")
        or (r == "H" and f == "L" and m == "H")
        or (r == "M" and f == "H" and m == "L")
    ):
        return "As New"
    # To Reactivate
    if (r == "M" and f == "L" and m == "H") or (r == "M" and f == "L" and m == "M"):
        return "To Reactivate"
    # Searching for Attention
    if (r == "M" and f == "M" and m == "M") or (r == "M" and f == "M" and m == "L"):
        return "Searching for Attention"
    # About to Sleep
    if (
        (r == "L" and f == "L" and m == "M")
        or (r == "L" and f == "M" and m == "M")
        or (r == "L" and f == "L" and m == "H")
    ):
        return "About to Sleep"
    # Bad
    if (
        (r == "M" and f == "L" and m == "L")
        or (r == "L" and f == "M" and m == "L")
        or (r == "L" and f == "L" and m == "L")
    ):
        return "Bad"
    return "Unclassified"


Customers["Customer_Segment"] = Customers.apply(assign_segment, axis=1)

# Frequency distribution of segments
seg_freq = Customers["Customer_Segment"].value_counts()
seg_pct = (seg_freq / len(Customers) * 100).round(2)
percentage_table = pd.DataFrame(
    {
        "Customer_Type": seg_freq.index,
        "Frequency": seg_freq.values,
        "Percentage": seg_pct.values,
    }
)
percentage_table = percentage_table.sort_values("Percentage", ascending=False)
print(percentage_table)

# ══════════════════════════════════════════════
#  RFM Scoring → K-Means Clustering
# ══════════════════════════════════════════════
score_map = {"H": 3, "M": 2, "L": 1}
Customers["Recency_Score"] = Customers["Recency_Label"].map(score_map)
Customers["Frequency_Score"] = Customers["Frequency_Label"].map(score_map)
Customers["Monetary_Score"] = Customers["Monetary_Label"].map(score_map)

clustering_data = Customers[
    ["Recency_Score", "Frequency_Score", "Monetary_Score"]
].values

# Elbow method
wss = []
K_range = range(1, 11)
for k in K_range:
    km = KMeans(n_clusters=k, n_init=20, random_state=42)
    km.fit(clustering_data)
    wss.append(km.inertia_)

fig, ax = plt.subplots(figsize=(7, 4))
ax.plot(list(K_range), wss, "b-o")
ax.set_xlabel("Number of Clusters K")
ax.set_ylabel("Total Within-Cluster Sum of Squares")
ax.set_title("Elbow Method to Determine the Optimal Number of K")
plt.tight_layout()
plt.savefig("elbow_kmeans.png", dpi=150)
plt.show()

# K-Means with k = 3
k = 3
np.random.seed(123)
kmeans_result = KMeans(n_clusters=k, n_init=20, random_state=123)
kmeans_result.fit(clustering_data)

# Silhouette analysis
sil_vals = silhouette_samples(clustering_data, kmeans_result.labels_)
avg_sil = sil_vals.mean()
print(f"Average Silhouette Width: {avg_sil:.4f}")
if avg_sil > 0.5:
    print("The clustering quality is good with well-defined clusters.")
elif avg_sil > 0.3:
    print("The clustering quality is moderate, with some overlap between clusters.")
else:
    print("The clustering quality is poor; consider revising the number of clusters.")

Customers["Cluster"] = kmeans_result.labels_.astype(str)
print(Customers["Cluster"].value_counts())

# PCA for visualization
pca = PCA(n_components=2)
pca_data = pca.fit_transform(StandardScaler().fit_transform(clustering_data))
plot_df = pd.DataFrame(
    {
        "PC1": pca_data[:, 0],
        "PC2": pca_data[:, 1],
        "Cluster": Customers["Cluster"].values,
    }
)

loadings = pd.DataFrame(
    pca.components_.T,
    columns=["PC1", "PC2"],
    index=["Recency_Score", "Frequency_Score", "Monetary_Score"],
)
print("PCA Loadings:")
print(loadings)

fig, ax = plt.subplots(figsize=(8, 6))
for cl in sorted(plot_df["Cluster"].unique()):
    sub = plot_df[plot_df["Cluster"] == cl]
    ax.scatter(sub["PC1"], sub["PC2"], alpha=0.5, label=f"Cluster {cl}", s=15)
ax.set_xlabel("Overall Engagement Score (PC1)")
ax.set_ylabel("Recency vs. Value Dynamics (PC2)")
ax.set_title("Customer Clustering (PCA)")
ax.legend(loc="lower right")
plt.tight_layout()
plt.savefig("pca_clusters.png", dpi=150)
plt.show()

# Assign meaningful cluster names (adjust mapping based on your cluster centers)
cluster_name_map = {
    "1": "Low Engagement",
    "2": "Consistently Moderate",
    "0": "Highly Active and Recent",
}
Customers["Simplified_Cluster"] = (
    Customers["Cluster"].map(cluster_name_map).fillna("Unclassified")
)

fig, ax = plt.subplots(figsize=(7, 4))
Customers["Simplified_Cluster"].value_counts().plot(kind="bar", color="skyblue", ax=ax)
ax.set_xlabel("Segment")
ax.set_ylabel("Customer Count")
ax.set_title("Distribution of New Customer Segments")
plt.xticks(rotation=45, ha="right")
plt.tight_layout()
plt.show()

# ══════════════════════════════════════════════
#  Split into clusters & build final merged
# ══════════════════════════════════════════════
Cluster_1 = Customers[Customers["Simplified_Cluster"] == "Low Engagement"].copy()
Cluster_2 = Customers[Customers["Simplified_Cluster"] == "Consistently Moderate"].copy()
Cluster_3 = Customers[
    Customers["Simplified_Cluster"] == "Highly Active and Recent"
].copy()

# Filter super_customer to remove top quantile overlap
sc_q_mon = super_customer["Monetary_Value"].quantile(0.8)
sc_q_freq = super_customer["Frequency"].quantile(0.8)
super_customer = super_customer[
    (super_customer["Monetary_Value"] < sc_q_mon)
    & (super_customer["Frequency"] < sc_q_freq)
].copy()

Cluster_1["Final_Segment"] = "Low Engagement"
Cluster_2["Final_Segment"] = "Consistently Moderate"
Cluster_3["Final_Segment"] = "Highly Active and Recent"
super_customer["Final_Segment"] = "Super Customer"

merged_customers = pd.concat(
    [Cluster_1, Cluster_2, Cluster_3, super_customer], ignore_index=True
)
merged_customers.to_csv("merged_customers.csv", index=False)

# ══════════════════════════════════════════════
#  Cluster Summary
# ══════════════════════════════════════════════
cluster_summary = (
    merged_customers.groupby("Final_Segment")
    .agg(
        Avg_Monetary_Value=("Monetary_Value", "mean"),
        AVG_Ticket=("AVG_Ticket", "mean"),
        Avg_Visits=("Frequency", "mean"),
        Avg_Recency=("Recency", "mean"),
        AVG_IPT=("AVG_ipt", "mean"),
        Total_Customers=("Monetary_Value", "count"),
        Avg_Number_Item_Other=("Number_item_other", "mean"),
        Avg_Number_Item_PL=("Number_item_PL", "mean"),
    )
    .reset_index()
)

cluster_summary["PL_on_Tot"] = cluster_summary["Avg_Number_Item_PL"] / (
    cluster_summary["Avg_Number_Item_PL"] + cluster_summary["Avg_Number_Item_Other"]
)
print(cluster_summary)

# ── Boxplot of Number_item_PL across segments ──
fig, ax = plt.subplots(figsize=(8, 5))
merged_customers.boxplot(column="Number_item_PL", by="Final_Segment", ax=ax)
ax.set_xlabel("Cluster")
ax.set_ylabel("Number_item_PL")
ax.set_title("Number_item_PL Distribution Across Clusters")
plt.suptitle("")  # Remove automatic pandas suptitle
plt.xticks(rotation=45, ha="right")
plt.tight_layout()
plt.show()
