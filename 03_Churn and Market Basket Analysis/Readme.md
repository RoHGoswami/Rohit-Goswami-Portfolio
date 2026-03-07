# Customer Relationship Management & Revenue Operations for COOP Italia

## Project Overview
This project was developed for COOP Italia to comprehensively understand customer purchase behavior and loyalty mechanisms. By applying advanced data analytics to transactional datasets, the objective was to extract actionable insights and design tailored, data-driven marketing strategies to improve retention, increase basket size, and optimize Private Label (PL) performance.

## Analytical Methodologies
The project utilized a robust pipeline of statistical and machine learning techniques:
* **RFM Analysis & K-Means Clustering:** Customers were evaluated based on Recency, Frequency, and Monetary value. Outliers were removed using the IQR method to ensure clean data. PCA and K-Means clustering (validated by the Elbow method and Silhouette scores) were then applied to segment the customer base.
* **Market Basket Analysis (MBA):** The Apriori algorithm was deployed to identify distinct consumption dynamics and products frequently bought together. Rules were filtered using strict thresholds (Support > 0.01, Confidence > 0.65, Lift > 1.2) to ensure high predictive strength.
* **Proactive Churn Modeling:** Churn was defined dynamically by benchmarking each customer against their own historical 31-day temporal patterns, rather than using rigid thresholds. Logistic Regression, Random Forest, and XGBoost models were used to identify the variable importance of churn drivers. 

## Key Insights
### 1. Customer Segmentation
The clustering model successfully identified four distinct customer profiles:
* **Super Customers:** Highest average ticket value, frequent, and recent purchasers. The most valuable segment.
* **Highly Active & Recent:** Moderate ticket value but high frequency. Highly engaged and valuable.
* **Consistently Moderate:** Stable revenue stream with moderate ticket values and average frequency.
* **Low Engagement:** Low ticket value, low frequency, and inactive.

### 2. Private Label (PL) Dynamics
Private Label purchase percentage was consistent across all clusters (28%-32%). However, churn models revealed a critical relationship between PL reliance and loyalty:
* **High Churn Risk:** Customers with a high PL ratio but low average PL items per transaction lack broader engagement beyond PLs.
* **High Loyalty:** Customers with a moderate PL ratio but a high volume of PL items per transaction demonstrate stronger overall brand loyalty and lower churn probability.

## Strategic Marketing Recommendations
Based on the analytical findings, four primary marketing pillars were proposed:

**1. "Ready Basket" Initiatives (Driven by MBA)**
* Creation of bundled "ready baskets" (e.g., Breakfast, Dinner, House Cleaning, Personal Care) based directly on associated Apriori rules (e.g., Biscuits & Milk, Cleaning Accessories & Detergents). 
* Customers are incentivized to buy the full bundle to earn extra loyalty points.

**2. Reactivation & Retention Campaigns**
* **Low Engagement:** Deploy personalized reactivation offers (e.g., "Double loyalty points this week").
* **Moderate Engagement:** Introduce gamified incentives for frequency (e.g., milestone rewards for completing 5 purchases in a month).

**3. Strengthen Private Label Engagement**
* Introduce volume-based discounts (e.g., "Buy 3 PL items, get 20% off") and gamified PL challenges to increase the average number of PL items per transaction, actively migrating high-risk customers into the high-loyalty bracket.

**4. Deepen Long-Term Loyalty**
* Reward "Super Customers" with exclusive loyalty tiers (e.g., Gold members receive free delivery) and personalized, spending-based anniversary rewards to secure their high lifetime value.

*Disclaimer: The data for this project has not been provided with the rest of things since it proprietary.
