# jackknife_var_est
Jackknife variance estimator for multiply imputed data under uncongeniality


This repository holds the code for a nonparametric jackknife variance estimator developed for small, multiply imputed datasets where uncongeniality between the analysis and imputation model is of concern. Compared to alternatives in the literature, namely those involving bootstrap resampling, this estimator provides near-nomial coverage with significantly narrower confidence intervals. Moreover, the jackknife estimator is nearly 30x more computationally efficient than bootstrap resampling, making it available to statistical practitioners who may not have sufficient computational power.  
