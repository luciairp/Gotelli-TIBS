# --------------------------------------
# FUNCTION fit_regression
# required packages: none
# description: fit linear regression to two continuous variables
# inputs: x = vector of real numbers (predictor)
#         y = vector (same length x) of real numbers (response)
# outputs: slope = slope of regression line
#          slope_p = p value for slope
#          int = intercept of regression line
#          int_p = p value for intercept
########################################
fit_regression <- function(x=NULL,y=NULL){

# assign parameter defaults
if (is.null(x) | is.null(y)) {
  x <- runif(10)
  y <- runif(10)
}

# function body

my_model <- summary(lm(y~x))
slope <- my_model$coefficients[2,1]
slope_p <- my_model$coefficients[2,4]
int <- my_model$coefficients[1,1]
int_p <- my_model$coefficients[1,4]
output <- list(slope=slope,slope_p=slope_p,int=int,int_p=int_p)
resids <- my_model$residuals
output <- list(slope=slope,slope_p=slope_p,int=int,int_p=int_p,resids=resids)

#return(print('...checking function: fit_regression()'))
return(output)

} # end of function fit_regression
# --------------------------------------

# fit_regression()
# fit_regression(x = c(2.2, 3.4, 5, 6),y = c(10, 11, 12, 13))
