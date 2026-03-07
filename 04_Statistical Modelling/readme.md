# Multivariate Assessment of Student Performance 

## Overview
This project analyzes the determinants influencing students' overall exam scores utilizing a multivariate dataset. The primary objective is to identify key factors significantly impacting student performance and provide valuable, data-driven insights through various statistical analysis models.

## Dataset & Preprocessing
The dataset comprises 20 variables related to student performance. 
* **Dependent Variable:** 'EXAM SCORES' was selected as the dependent variable for the modeling.
* **Collinearity:** A collinearity check was performed after converting categorical variables into numerical dummies, revealing extremely negligible values of negative collinearity.
* **Outlier Handling & Stratification:** The normal distribution curve of exam scores showed a comparatively high number of outliers (students who scored extremely well and moved far away from the mean). Because these outliers could not simply be removed, the dataset was divided into two distinct subsets for separate analysis:
  * **Student data low:** Students scoring between 55-79 (6559 observations).
  * **Student data high:** Students scoring between 80-100 (47 observations).

## Methodology
* **Data Transformation:** A log transformation was applied to the dependent variable (Exam Scores) in both datasets.
* **Primary Model:** After multiple iterations, a Linear Mixed Model with random effect and interactions was selected. "Parental Involvement" was utilized as the random effect. 
* **Validation Model:** A Random Forest Algorithm was employed as a non-parametric alternative to validate the results.

## Key Findings
Logistic regression analysis across both datasets revealed several critical determinants of student performance:

### Common Positive Influences
* **Attendance:** Regular class attendance is strongly associated with better academic outcomes.
* **Hours Studied:** The amount of time dedicated to studying positively correlates with improved performance.
* **Parental Involvement:** Higher engagement from parents contributes significantly to student success.
* **Teacher Quality:** The effectiveness of teachers plays a crucial role in shaping achievement. 
* **Interaction Effect:** An interesting interaction emerged suggesting that students with learning disabilities tend to perform better when receiving additional attention and tailored teaching methods from high-quality educators.

### Common Negative Influences
* **Distance from Home:** Living farther from school is associated with lower academic performance, potentially due to longer commute times and fatigue.
* **Learning Disabilities:** While interventions help, learning disabilities remain a significant factor influencing academic performance.

### Differentiators for High-Performing Students
When comparing average students with those in the 80th percentile, the distinguishing factors are:
* **Physical Activity:** Regular physical exercise correlates with higher academic performance, possibly due to improved cognitive function and reduced stress levels.
* **Previous Scores:** Past academic performance strongly predicts future success, reinforcing the idea of cumulative learning effects.
* **Access to Resources:** Better access to educational materials, tutoring, and digital tools enhances student outcomes.

## Statistical Validation
To ensure the robustness of the findings, multiple diagnostic tests were performed:
* **Likelihood Ratio Test (LRT):** Guided the selection of the optimal mixed-effects model by identifying the most significant interaction terms and random effect variables.
* **Breusch-Pagan Test:** Revealed the presence of heteroskedasticity, indicating the variance of errors is not constant.
* **AIC and BIC Tests:** Lower Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC) values indicated a more efficient model fit, reinforcing the selected logistic regression structure.
* **Random Forest Validation:** The non-parametric Random Forest model largely corroborated the logistic regression findings for the "Student Data Low" dataset. However, deviations in the "Student Data High" dataset indicate that different statistical approaches may capture distinct underlying patterns within high-performing student groups.

## Conclusion
The analysis indicates that academic trajectories tend to be self-reinforcing, meaning historically high-performing students are likely to continue excelling. Furthermore, improved access to resources and physical activity can significantly enhance academic performance, even for students who previously scored at an average level. Incorporating Parental Education Level as a random effect also suggests that students with more highly educated parents tend to benefit from additional academic support at home.

## Research Poster
*See the attached PDF for the full academic presentation and visual diagnostics.*

