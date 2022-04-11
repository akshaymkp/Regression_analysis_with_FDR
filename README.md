# Regression_analysis_with_FDR
We will create a multiple regression model to predict cars prices and check false discovery rate among variables.  

When running multiple regression models on very large data sets, there are normally thousands of hypothesis tests which are evaluated at the same time. For example in the study of genes or drug testing. When thousands of hypothesis are tested simultaneously, chances of incorrectly classifying a null hypothesis as significant or type I error rate increases.    

For example in a model with 10000 variables and a false discovery rate of 5%, there can be 500 false positive discoveries. This means on average 500 truly null hypothesis will come out to be significant. 

False positive discoveries can be quite misleading and cost intensive given the cost of incurring false positive results. For example in testing the effects of a drug, it may lead to wrong conclusions of the drug effects on some of the genes collected from samples.    

In this particular case, we are using a sample of cars data to predict prices. The manufacturer may spend capital and labor on wrong attributes to control the price even though the variable is a false discovery and doesn't actually affect the price.   

So it is necessary to control for false discovery rates in multiple regression models with high number of features. As the number of features increase, so does the chances of false discoveries.
