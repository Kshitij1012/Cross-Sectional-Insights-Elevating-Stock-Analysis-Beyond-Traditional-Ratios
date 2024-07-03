**Cross-Sectional Insights: Elevating Stock Analysis Beyond Traditional Ratios**


Overview
This project explores the effectiveness of traditional financial ratios in stock market analysis, specifically within the Financial Services sector. 
Using a combination of cross-sectional analysis and logistic regression, the study aims to provide a more comprehensive set of tools for making informed investment decisions.


Introduction
Traditional financial ratios have long been used to gauge a company's financial health. However, this project questions whether these metrics provide comprehensive insights for investment decisions. 
Motivated by studies such as "Regression Analysis v. Ratios in the Cross-section Analysis of Financial Statements" by R.H. Berry st.al, this research focuses on the Financial Services sector, using cross-sectional analysis and logistic regression to evaluate the impact of various financial ratios on investment decisions.

Methodology
The project utilizes financial statement data of U.S. stock companies from Kaggle's "200+ Financial Indicators of US stocks (2014-2018)" dataset. The methodology includes:

Data Preprocessing: 
Cleaning and transforming the data using R libraries (readxl, tidyverse, data.table).
Cross-Sectional Analysis: Analyzing key financial ratios (e.g., current ratio, return on assets) and operational metrics.
Logistic Regression: Building a logistic regression model to predict investment decisions (buy or don't buy) based on financial ratios.

Results and Discussion

Cross-Sectional Analysis
Liquidity Ratios: Analysis of current and quick ratios reveals differing liquidity management strategies among top companies.
Profitability Ratios: Examination of net profit margins and return on assets shows contrasting performances, with digital payment companies outperforming traditional banks.
Market-Based Metrics: Evaluation of EPS and P/E ratios reflects investor sentiment and growth expectations.
Debt Management: Study of debt-to-equity, EV/EBITDA, and debt service coverage ratios highlights strategic differences in debt management.

Regression Analysis

Model Setup: Logistic regression model using financial ratios as predictors for the 'Class' variable.
Results: The model shows approximately 61.86% accuracy. Return on assets and debt-to-assets ratios are identified as near-significant predictors of investment decisions.
