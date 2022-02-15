## This file should provide following objects, when loaded:
# f : function
# input.f : list of input dimensions, contains list of properties like lower & upper bounds of each dimensions
# output.f : list of output dimensions
# *.f : list of math properties. To be compared with algorithm results
# [print.f] : method to print/plot the function for information

f <- function(x) {
	x1 <- x[,1]*15-5   
	x2 <- x[,2]*15     
	matrix((x2 - 5/(4*pi^2)*(x1^2) + 5/pi*x1 - 6)^2 + 10*(1 - 1/(8*pi))*cos(x1) + 10,ncol=1)
}
input.f = list(
    x1=list(min=0,max=1),
    x2=list(min=0,max=1)
)
output.f = "branin"
mean.f = 53.93

library(testthat)

test = function(algorithm_file) {
    results = run.algorithm(algorithm_file, options=list(`model[x]`=list(x1="Unif(0,1)",x2="Unif(0,1)")),fun=list(input=input.f,output=output.f,fun=f))
    if (!isTRUE(test_that("branin mean",{expect_equal(as.numeric(results$mean),mean.f,tolerance = .1)}))) quit(status=1)
}

