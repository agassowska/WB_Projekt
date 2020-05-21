---
title: "Comparison of performance of data imputation methods"
subtitle: "in the context of their impact on the prediction efficiency of classification algorithms"
output: pdf_document
author: "Ada Gąssowska, Mateusz Grzyb, Elżbieta Jowik"
---


## Abstract  

[comment]: # (Jeszcze jest stary abstrakt, ale to chyba na koniec ppr)

It is very common for datasets to contain incomplete observations. The analysis conducted by ignoring cases with missing data and considering only complete observations is perceived as naive, inefficient and exposed to bias, as missing values may also convey valuable information. To incorporate these sketchy observations, various methods of handling missing data may be considered, including both less and more sophisticated approaches.  It turns out that the performance of machine learning classification models moderately depends on the type of approach that is applied to the imputation problem while the time of imputation, when using different techniques, is highly diverse. Presented article focuses on one basic technique (median/mode imputation) and five more sophisticated ones, which origin respectively from: mice, VIM, missRanger and softImpute R packages.


The differences in the results of the classification algorithm for the imputation techniques in respect to included measures are slight, nevertheless, the best outcome was obtained for the hotdeck method from the VIM package. As for the time of imputation, it highly depends on the technique complexity, so the basic method turns out to be the fastest one. 

## Introduction

Dealing with missing data is a substantial part of feature engineering as most of the machine learning algorithms do not accept incomplete datasets. Data imputation, which is replacing missing values with a value based on other available cases, is the solution for that problem. However, despite the acknowledged importance of it, in many practical cases it is not handled with proper caution. Basic median/mode imputation is often used, as well as deleting rows with missing data (if the number of missing values is low).   


This paper introduces imputation techniques in the context of their impact on the the time of filling missing data and the prediction efficiency of four classification algorithms (k nearest neighbours, naive bayes, XGBoost and Ranger Random Forest).

## Methodology

All of the algorithms we used for tests, do not accept missing values. To find the best way to handle missing data for each algorithm we decided to perform an experiment. We used six different imputation functions on each one of the 10 benchmarking datasets:

[comment1]: # (Poprawić na końcową liczbę zbiorów!)


* **basic method** using impute() function from imputeMissings package. It imputes missing numeric values with median and categorical variables with mode of other records.
* **mice()** function from mice package. The function uses Predictive Mean Matching to impute missing values.
* **missRanger()** function from missRanger package. The technique uses the ranger package to do fast missing value imputation by chained random forest.
* **hotdeck()** function from VIM package. Each missing value is replaced with an observed response from a “similar” unit.
* **kNN()** function from VIM package. It finds the k closest neighbors to the observation with missing data and then imputes them based on the the non-missing values from its neighbors.
* **softImpute()** function combined with mode imputation}. The first method origins from softImpute package and is applied to impute numeric variables. Mode imputation is used for categorical ones imputation. 


Function using imputeFAMD() from missMDA package was also created but it was unable to perform imputation on all of benchmarking datasets due to issues with convergence. Imputation with use of Amelia package also proved impossible due to occurrence of highly correlated variables in some benchmarking datasets.


To automatize our work we created specialized functions: **split_and_impute()** and  **train_and_test()**. 

  
* The first function divides given dataset into train and test sets (of given size) and imputes it with specified imputer. If we set the "save" argument to TRUE it saves the imputed datasets. It returns imputed train and test sets and the time of imputation. 


  
  
* The second function performs crossvalidation on train set (The default number of folds is 5, but it can by changed) and makes the predictions for test set made with specified learnner, trained on the whole train set (target variable, and name of the positive class is given by the user). Based on mentioned results it calculates AUC, BACC and MCC measures for both crossvalidation and test set testing stages. It also returns plots of ROC curve,AUC, BACC and MCC measures achieved during crossvalidation stage. 




After calling split_and_impute() and test_and_train() functions on each dataset with six different imputers and with four different algorithms we were able to compare performance of each tested imputation function in the context of its impact on the prediction efficiency of the classification alghoritms.

The prediction effectiveness for each alghoritm  was assessed in relation to the area under the ROC curve (AUC[^1]), , balanced accuracy (BACC[^2]), and Matthews correlation coefficient (MCC[^3]) measures. Time of each imputation was also measured, and taken into consideration.



## Results

Each imputation function was performed on ten(!!!! ZMIENIĆ) benchmarking datasets with missing data, divided into test and train sets. We gathered the predictions for test sets from ranger Random Forest, kNN, Naive Bayes, and XGBoost models fitted on train sets, and compared them using AUC, BACC and MCC (see Table1, Figure1, Figure2 and Figure3-ZMIENIĆ JAK BĘDĄ RYSUNKI). We also measured the time of each imputation and compared it (Figure 4). 


It should be mentioned that any differences in results are only due to a change in the imputation technique. Each of them was performed on the same sets with identical divisions into train and test sets. So even small differences should be taken into consideration. 




[^1]: Flach Peter, Hernandez-Orallo Jose, Ferri, Cèsar "A Coherent Interpretation of AUC as a Measure of Aggregated Classification Performance."Proceedings of the 28th International Conference on Machine Learning, ICML 2011
[^2]: Velez, D.R., White, B.C., Motsinger, A.A., Bush, W.S., Ritchie, M.D., Williams, S.M. and Moore, J.H. (2007), A balanced accuracy function for epistasis modeling in imbalanced datasets using multifactor dimensionality reduction. Genet. Epidemiol., 31: 306-315. DOI: 10.1002/gepi.20211
[^3]:Boughorbel, S., Jarray, F., & El-Anbari, M. (2017). Optimal classifier for imbalanced data using Matthews Correlation Coefficient metric. PloS one, 12(6), e0177678. DOI: https://doi.org/10.1371/journal.pone.0177678



