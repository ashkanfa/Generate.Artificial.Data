# Generate.Artificial.Data
 We generate artificial data by assuming the true response follows an additive linear relationship with true predictors.
 
 𝑦𝑖=Σ𝛽𝑗𝑥𝑖𝑗+ 𝜀𝑖` where 𝜀𝑖 ~ 𝑁(0,1).
 
 The simulation design can be customized considering five factors:
 
 (1) total number of variables p
 
 (2) number of observations n
 
 (3) proportion of true predictors among all variables. true.prop
 
 (4) correlation structure, if we divide variables into true predictors and spurious variables, then we can define a complex correlation structure as the following:
 
 ![alt text](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png "Logo Title Text 1")
 
 which controls not only the magnitude of the correlation within true predictors but also controls the intensity      of the correlation between true predictors and spurious variables. This allows us to monitor the ability of different methodology in      differentiating causation from correlation. 
 
 (5) magnitude of the effects (coefficients).
