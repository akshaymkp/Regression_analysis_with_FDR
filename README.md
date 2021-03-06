# Regression_analysis_with_FDR
We will create a multiple regression model to predict cars prices and check false discovery rate among variables.  

When running multiple regression models on very large data sets, there are normally thousands of hypothesis tests which are evaluated at the same time. For example in the study of genes or drug testing. When thousands of hypothesis are tested simultaneously, chances of incorrectly classifying a null hypothesis as significant or type I error rate increases.    

For example in a model with 10000 variables and a false discovery rate of 5%, there can be 500 false positive discoveries. This means on average 500 truly null hypothesis will come out to be significant. 

False positive discoveries can be quite misleading and cost intensive given the cost of incurring false positive results. For example in testing the effects of a drug, it may lead to wrong conclusions of the drug effects on some of the genes collected from samples.    

In this particular case, we are using a sample of cars data to predict prices. The manufacturer may spend capital and labor on wrong attributes to control the price even though the variable is a false discovery and doesn't actually affect the price. We will start by building a multiple regression model. After exploratory data analysis we will use stepwise regression to select important variables and then perform regression diagnostics - normality assessment, independence test, constant variance (homoscedasticity) test and multicollinearity. Finally we will check for false discoveries using a threshold of 10% or q value of 0.1. In our improved model, we want to minimize FDR. We will start with q of 0.1 and use Benjamini-Hochberg procedure to check p-values. We got a cut-off of 0.06734374 for p-values, which implies to keep the fdr <= 0.1, we should consider variables with p-value less than or equal to 0.06734374. Using this cut-off value, we were able to reduce the number of variables from 31 to 22.

It is necessary to control for false discovery rates in multiple regression models with high number of features. As the number of features increase, so does the chances of false discoveries.
