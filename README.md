📱 Telecom Churn Prediction using R
A machine learning project to identify customers who are likely to churn from a telecom service provider. The project uses logistic regression, K-Nearest Neighbors (KNN), and Naïve Bayes models with a focus on preprocessing, multicollinearity treatment, and predictive performance comparison.

👩‍💻 Author
Ajuna P John

🎯 Objective
To develop a predictive model that accurately identifies telecom customers who are at risk of churn, enabling proactive retention strategies.

📂 Dataset Overview
File Name: Cellphone.xlsx

Observations: 1,000 customers

Features: 20 variables including:

Customer demographics

Account tenure and plan types

Usage statistics

Billing and payment features

Customer service interactions

Target Variable: Churn (Yes/No)

🧹 Data Preprocessing
Converted categorical variables to factors

Detected and removed multicollinearity using VIF

Created dummy variables for multinomial fields (e.g., Contract Type, Internet Service)

Addressed class imbalance with appropriate sampling techniques

📊 Exploratory Data Analysis (EDA)
Plotted distributions for tenure, usage, and customer service calls

Identified churn-prone segments via bar plots and boxplots

Assessed correlation matrix to remove highly correlated predictors

🤖 Modeling Techniques
1️⃣ Logistic Regression
AUC: 0.84

Accuracy: ~84%

Significant predictors: Tenure, Contract Type, Monthly Charges

2️⃣ K-Nearest Neighbors (KNN)
Optimal k = 15 (based on cross-validation)

Accuracy: 91.4%

Best performing model in terms of accuracy

3️⃣ Naïve Bayes
Accuracy: 85.6%

Fast and interpretable, but slightly lower performance

📌 Evaluation Metrics
Confusion Matrix

Accuracy

Sensitivity & Specificity

ROC Curve and AUC

💡 Key Insights
Customers with short tenure, fiber optic internet, and monthly contracts are more likely to churn.

Higher customer service calls and monthly charges are churn indicators.

KNN performed best among all models in terms of predictive accuracy.

🧰 Tech Stack
Language: R

Libraries: caret, class, e1071, ggplot2, car, ROCR, dplyr, corrplot, caTools
