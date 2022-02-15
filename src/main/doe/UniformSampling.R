#help: Random uniform sampling
#tags: uncertainties
#options: sample_size='100'; seed='1'
#input: x=list(min=0,max=1)
#output: y=0.99

#' constructor and initializer of R session
UniformSampling <- function(opts) {
  uniformsampling = new.env()

  uniformsampling$sample_size <- as.integer(opts$sample_size)
  uniformsampling$seed <- as.integer(opts$seed)

  return(uniformsampling)
}

#' first design building. All variables are set in [min,max]
#' @param input variables description (min/max, properties, ...)
#' @param output values of interest description
getInitialDesign <- function(algorithm,input,output) {
  algorithm$input = input
  algorithm$output = output
  set.seed(algorithm$seed)
  X = matrix(runif(algorithm$sample_size * length(input)),ncol=length(input))
  names(X) <- names(input)
  return(from01(X,algorithm$input))
}

#' iterated design building.
#' @param X data frame of current doe variables
#' @param Y data frame of current results
#' @return data frame or matrix of next doe step
getNextDesign <- function(algorithm,X,Y) {
  return(NULL)
}

#' final analysis. Return HTML string
#' @param X data frame of doe variables
#' @param Y data frame of  results
#' @return HTML string of analysis
displayResults <- function(algorithm,X,Y) {
    Y = Y[,1]
    
    algorithm$files <- paste0("hist.png",sep="")
    png(file=algorithm$files,bg="transparent",height=600,width = 600)
    hist(na.omit(Y),xlab=algorithm$output, main=paste("Histogram of" , algorithm$output))
    dev.off()

    html=paste0("<HTML name='summary'>mean=",mean(Y,na.rm=TRUE),"<br/>",
                "standard deviation=",sd(Y,na.rm=TRUE),"<br/>",
                "median=",median(Y,na.rm=TRUE),"<br/>",
                "quantile 0.05=",quantile(Y,0.05,na.rm=TRUE),"<br/>",
                "quantile 0.95=",quantile(Y,0.95,na.rm=TRUE),"<br/>",
                "<img src='", algorithm$files, "' width='600' height='600'/></HTML>")

    m=paste("<mean>",mean(Y,na.rm=TRUE),"</mean>")
    sd=paste("<sd>",sd(Y,na.rm=TRUE),"</sd>")
    sd=paste("<median>",median(Y,na.rm=TRUE),"</median>")
    q05=paste("<q05>",quantile(Y,0.05,na.rm=TRUE),"</q05>")
    q95=paste("<q95>",quantile(Y,0.95,na.rm=TRUE),"</q95>")

    return(paste(html,m,sd,q05,q95,collapse=';'))
}

#' temporary analysis. Return HTML string
#' @param X data frame of doe variables
#' @param Y data frame of  results
#' @returnType String
#' @return HTML string of analysis
displayResultsTmp <- function(algorithm,X,Y) {
    Y = Y[,1]
    
    algorithm$files <- paste0("hist_",nrow(Y),".png",sep="")
    png(file=algorithm$files,bg="transparent",height=600,width = 600)
    hist(na.omit(Y),xlab=algorithm$output, main=paste("Histogram of" , algorithm$output))
    dev.off()

    html=paste0("<HTML name='hist'><img src='", algorithm$files, "' width='600' height='600'/></HTML>")

    return(html)
}

from01 = function(X, inp) {
  nX = names(X)
  for (i in 1:ncol(X)) {
    namei = nX[i]
    X[,i] = X[,i] * (inp[[ namei ]]$max-inp[[ namei ]]$min) + inp[[ namei ]]$min
  }
  return(X)
}

to01 = function(X, inp) {
  nX = names(X)
  for (i in 1:ncol(X)) {
    namei = nX[i]
    X[,i] = (X[,i] - inp[[ namei ]]$min) / (inp[[ namei ]]$max-inp[[ namei ]]$min)
  }
  return(X)
}

##############################################################################################
# @test
# f <- function(X) matrix(apply(X,1,function (x) {
#     x1 <- x[1] * 15 - 5
#     x2 <- x[2] * 15
#     (x2 - 5/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10
# }),ncol=1)
# # f1 = function(x) f(cbind(.5,x))
# 
# options = list(sample_size = 100, seed = 1)
# a = UniformSampling(options)
# 
# X0 = getInitialDesign(a, input=list(x1=list(min=0,max=1),x2=list(min=0,max=1)), "y")
# Y0 = f(X0)
# # X0 = getInitialDesign(a, input=list(x2=list(min=0,max=1)), NULL)
# # Y0 = f1(X0)
# Xi = X0
# Yi = Y0
# 
# finished = FALSE
# while (!finished) {
#     Xj = getNextDesign(a,Xi,Yi)
#     if (is.null(Xj) | length(Xj) == 0) {
#         finished = TRUE
#     } else {
#         Yj = f1(Xj)
#         Xi = rbind(Xi,Xj)
#         Yi = rbind(Yi,Yj)
#     }
# }
# 
# print(displayResults(a,Xi,Yi))