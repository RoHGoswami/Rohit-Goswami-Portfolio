# Marketing Mix Modeling (MMM): Optimization for Beauty Brand Growth

## 1. Project Overview and Business Context
This project was developed for the Analytics for Business Lab (GroupM) to create a robust Marketing Mix Model (MMM) for a relatively young beauty brand that has gained prominence over the last five years. 

Currently, the brand holds a 2.6% share of a highly seasonal, stable market. The primary business objective is to expand this market share by increasing brand recognition and optimizing media and promotional investments. The MMM was designed to understand the distinct drivers of sales, evaluate the Return on Investment (ROI) of historical media channels, and provide a data-driven budget reallocation strategy to maximize Return on Ad Spend (ROAS).

---

## 2. Data Engineering & Preprocessing
To ensure accurate modeling and reduce dimensionality, extensive data preparation and feature engineering were conducted on the raw dataset:

* **Data Cleaning & Standardization:** Missing values were identified and replaced with 0. The dataset was chronologically aligned to begin from the first week of 2020. Categorical data types were converted into numerical formats to ensure modeling consistency.
* **Macroeconomic Adjustments (Inflation):** An Inflation Index was applied to all currency-based categories to normalize financial data. The 2020 Consumer Price Index (CPI) was used as the base year, applying the formula: `Adjusted Value = Original Value * (CPI in Target Year) / (CPI in Base Year)`.
* **Feature Consolidation:** Highly similar sub-categories were merged to reduce model complexity and evenly spread data. For instance, "Totem Display in store" and "Totem Display next to cashier" were combined into a single `Totems` variable.
* **Competitor Normalization:** Data for the two main competitors ("Mass" and "Pharma") exhibited high skewness and asymmetrical distribution. This was smoothed by calculating the median Gross Rating Point (GRP) value for each competitor per week, which was then used to calculate competitive adstock.

---

## 3. Advanced Modeling Methodology: Robyn
The model was built using **Robyn**, an open-source, semi-automated MMM framework developed by Meta that utilizes ridge regression and evolutionary algorithms to minimize human bias.

### Model Variables
* **Dependent Variable:** `VALUE SALES BRAND`.
* **Paid Media Variables:** Online and offline channels with explicit investments (TV, YouTube, TikTok, Google Search, Amazon Search, Social Meta, Influencers).
* **Context Variables:** Exogenous factors defining the business environment, including seasonality, lockdowns, and competitor activity.

### Adstock Transformation

To capture the diminishing returns and carryover (memory) effects of advertising over time, a **Geometric Adstock** transformation was applied. 
* The mathematical representation used was: $Mt^{\prime} = Mt + \theta Mt-1^{\prime}$.
* Here, $\theta$ (theta) acts as the decay rate hyperparameter controlling the carryover effect, with the half-life determined by $\theta = 0.5 \wedge h$.

### Hyperparameter Optimization
Robyn mapped hyperparameter ranges (theta, gamma, and alpha) using **Bayesian Optimization**. A Gaussian Process modeled the objective function to minimize two critical performance metrics simultaneously:
1. **NRMSE (Normalized Root Mean Square Error):** To minimize prediction error.
2. **DECOMP.RSSD (Decomposition Root Sum of Squared Distance):** To minimize the discrepancy between the share of spend and the share of effect for paid media.

After 5,000 iterations, the chosen model achieved an impressive **Adjusted R-squared of 0.8878** and an **NRMSE of 0.0645**, indicating a highly accurate fit.

### Custom Synergy Calculations (Conditional ROI)
Because Robyn does not natively calculate cross-channel media synergies, interaction terms were manually introduced by multiplying media investment covariates (e.g., TV x YouTube, TV x Google Search). This allowed for the calculation of **Conditional ROI**—the actual ROI when pairs of media are invested in together—preventing the underestimation of interconnected channels.

---

## 4. Key Business Insights

Based on the Response Decomposition and model outputs, several strategic insights were identified:

<img width="836" height="386" alt="image" src="https://github.com/user-attachments/assets/47f5891b-b061-4991-bd94-e46c70b0eedd" />


* **Baseline vs. Incremental Sales:** The vast majority of sales are driven by the baseline trend and seasonality. This indicates robust organic demand—the product would still sell significantly even without active media pushes.
* **Promo Strategies:** Promotional activities are highly effective, particularly during holiday months (January). Specifically, "Promo Price Cuts" drive over 17% of inflation-adjusted sales. However, the model revealed that "Promo Bundles" had an effect coefficient of 0, meaning they do not significantly impact sales and should be discontinued.
* **Competitive Influence:** Competitors' marketing activities have a quantifiable negative impact on brand sales, emphasizing the need for strategic, targeted countermeasures.
* **Distribution Impact:** The `WEIGHTEDHANDLINGDISTRIBUTION` covariate significantly impacts sales. Optimizing inventory management and physical/digital shelf visibility is just as critical as media spend.

---

## 5. Channel Performance & ROI Analysis

Media investments currently contribute to approximately 15% of total sales. Performance varies drastically by channel:

<img width="765" height="417" alt="image" src="https://github.com/user-attachments/assets/6e1b2be8-749d-4bdd-98e8-062504f3b820" />


* **Top Performers (High ROI):** * **Amazon Search:** Exhibited the highest conditional and direct ROI (1.33 conditional). 
  * **Google Search:** Highly effective direct ROI (0.19) and massive conditional ROI (1.62).
  * **TikTok & YouTube:** Both platforms showed strong returns (TikTok ROI: 0.17, YouTube ROI: 0.12), aligning with the brand's target demographic responding well to video-based formats.
* **Media Synergies:** The coordinated interaction between **TV and YouTube** investments proved highly effective, driving approximately 10% of total sales.
* **Saturated & Ineffective Channels:** * **TV Investments:** While generating volume, TV has a low direct ROI (0.04) and has reached the upper bounds of its saturation curve. Additional spend here will lead to cannibalization.
  * **Influencer Marketing (Instagram) & Twitch:** Both channels showed an effect share and ROI of near 0%, indicating completely ineffective spend.

---

## 6. Strategic Budget Allocation

Using the model's saturation curves and Marginal ROAS (mROAS) metrics, the budget was restructured across three scenarios: *Initial* (current), *Bounded* (conservative shift), and *Bounded x3* (aggressive optimization).

**Key Optimization Moves:**
1. **Scale Down TV:** TV spend was reduced from an initial **49.1%** of the budget down to **30.1%** (Bounded) and **19.6%** (Bounded x3). This reallocation actually improved TV's ROAS from 0.04 to 0.06 by removing wasted, saturated spend.
2. **Aggressively Scale Search:** Budget was diverted into high-intent search channels. Google Search was scaled from **15.6%** up to **34%** (Bounded x3). Amazon Search was scaled from **5.8%** to **8%**. 
3. **Optimize Video Platforms:** YouTube investments were safely increased from **20.6%** to **27.1%** to capitalize on the TV x YouTube synergy.
4. **Cut Waste:** Investments in Twitch and Instagram Influencers were reduced to **0%** to eliminate wasted capital.

 *Disclaimer: The dataset used in this project is not included as it is proprietary. However, a managerial report and a report explaining the methodologies involved has been provided for those interested in exploring the methodology and findings in greater depth.*
