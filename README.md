# 🩸 Blood Request Fulfillment Analytics

[![R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![Status](https://img.shields.io/badge/Status-Completed-success.svg)]()

## 📊 Project Overview
This project focuses on the **Exploratory Data Analysis (EDA)** and **Predictive Modeling** of synthetic blood request data. The objective was to extract actionable insights regarding blood inventory management, fulfillment efficiency, and urgency classification within a healthcare context.

## 💡 Key Highlights
* **Comprehensive EDA:** Analyzed distribution patterns for blood quantity and fulfillment time using histograms, boxplots, and correlation matrices.
* **Clustering Analysis:** Implemented **K-Means Clustering (k=3)** to identify natural patterns in request volumes and fulfillment latency.
* **Predictive Modeling:** Evaluated four classification algorithms to predict blood request urgency:
    * **Naive Bayes**
    * **Decision Tree** (Achieved the best accuracy: **51%**)
    * **K-Nearest Neighbors (KNN)**
    * **Support Vector Machine (SVM)**

## 🛠️ Tech Stack
* **Language:** R
* **Libraries:** `ggplot2`, `corrplot`, `car`, `cluster`, `caTools`, `e1071`, `rpart`, `class`
* **Techniques:** Data Preprocessing, Feature Encoding (One-Hot), K-Means, Decision Trees, Confusion Matrix evaluation.

## 📂 Project Structure
* `requests.csv`: The synthetic dataset (500 observations, 25 variables).
* `project.R`: The complete R source code for analysis and modeling.
* `Blood-Request-Analysis.docx`: Detailed technical report with methodology and findings.

## 📈 Methodology & Findings
The analysis revealed that while most blood requests (53%) are routine, there is a clear distinction in fulfillment dynamics. The **Decision Tree** model outperformed other algorithms by capturing non-linear relationships between request characteristics and urgency levels. The correlation analysis demonstrated that fulfillment time is largely independent of request quantity, suggesting that operational bottlenecks exist elsewhere in the supply chain.

---
*Developed as part of the Programming in R Language course, Faculty of Information Technology, Al al-Bayt University.*
