# Adaptive Neuro-Fuzzy Inference System (ANFIS) for Cooling Load Prediction

This repository contains an implementation of an Adaptive Neuro-Fuzzy Inference System (ANFIS) model for predicting cooling load in buildings. The model integrates fuzzy logic and neural network capabilities to handle complex and uncertain data.

## Overview

The main objective of this project is to build and evaluate ANFIS models using three different types of membership functions:
- **Sigmoidal**
- **Gaussian**
- **Generalized Bell**

The best model is selected based on the Root Mean Square Error (RMSE) to determine which membership function provides the most accurate cooling load predictions.


## Dataset
The dataset used is the Energy Efficiency Dataset from the UCI Machine Learning Repository, donated on November 29, 2012. It contains 768 samples with 8 main features:
- Relative Compactness (RC)
- Surface Area (SA)
- Wall Area (WA)
- Roof Area (RA)
- Overall Height (OH)
- Orientation (OR)
- Glazing Area (GA)
- Glazing Area Distribution (GAD)

**Target Variable**:
- Cooling Load (CL)

## Methodology
1. Descriptive Analysis of all variables (RC, SA, WA, RA, OH, OR, GA, GAD, and CL).
2. Data Preprocessing:

   - Checked for missing values
   - Selected relevant features using Spearman correlation
   - Normalized input features

3. Data Splitting: Used multiple train-test ratios (50:50, 60:40, 70:30, 80:20, 90:10).
4. ANFIS Modeling:

   - Applied K-Means clustering (2 clusters) to generate premise parameters
   - Used Sigmoidal, Gaussian, and Generalized Bell membership functions
   - Calculated membership degrees, firing strength, and output using standard ANFIS layers
   - Optimized parameters using Least Squares Estimator and Steepest Descent

5. Model Evaluation: Measured performance using RMSE on test sets across all scenarios.
6. Model Selection: Chose the best-performing model based on the lowest RMSE.

## Key Results
Among the three tested models, the ANFIS model using the **Generalized Bell membership function** achieved the best prediction performance with an **RMSE of 2.058887**, indicating high prediction accuracy and effectiveness of the method.
