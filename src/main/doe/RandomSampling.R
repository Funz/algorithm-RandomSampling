#help: Random sampling
#tags: uncertainties
#options: sample_size='100'; seed='1'; model[x]='{"x":"Unif(0,1)"}'
#require: jsonlite
#input: x=list(min=0,max=1)
#output: y=0.99

#' constructor and initializer of R session
RandomSampling <- function(opts) {
  randomsampling = new.env()

  randomsampling$sample_size <- as.integer(opts$sample_size)
  randomsampling$seed <- as.integer(opts$seed)
  if (is.list(opts[['model[x]']]))
    randomsampling[['model[x]']] <- opts[['model[x]']]
  else
    randomsampling[['model[x]']] <- jsonlite::fromJSON(opts[['model[x]']])

  return(randomsampling)
}

#' first design building. All variables are set in [min,max]
#' @param input variables description (min/max, properties, ...)
#' @param output values of interest description
getInitialDesign <- function(algorithm,input,output) {
  algorithm$output = output
  algorithm$i = 0
  set.seed(algorithm$seed)
  X = NULL
  for (x in names(algorithm[['model[x]']])) {
    mx = unlist(strsplit(algorithm[['model[x]']][[x]],"::")) # supports evd::GEV(...)
    if (length(mx)>1) {
      library(mx[1])
      mx = mx[-1]
    }
    X = cbind(X,eval(parse(
      text=paste0("r",
                  gsub(       "(",
                       paste0("(",algorithm$sample_size,","),
                       tolower(mx), fixed=T
                       )
                  ))
        ))
  }
  names(X) <- names(algorithm[['model[x]']])
  return(X)
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
    
    algorithm$files <- paste0("hist_",algorithm$i-1,".png",sep="")
    png(file=algorithm$files,bg="transparent",height=600,width = 600)
    hist(Y,xlab=algorithm$output, main=paste("Histogram of" , algorithm$output))
    dev.off()

    html=paste0("<HTML name='summary'>mean=",mean(Y),"<br/>",
                "standard deviation=",sd(Y),"<br/>",
                "median=",median(Y),"<br/>",
                "quantile 0.05=",quantile(Y,0.05),"<br/>",
                "quantile 0.95=",quantile(Y,0.95),"<br/>",
                "<img src='", algorithm$files, "' width='600' height='600'/></HTML>")

    m=paste("<mean>",mean(Y),"</mean>")
    sd=paste("<sd>",sd(Y),"</sd>")
    sd=paste("<median>",median(Y),"</median>")
    q05=paste("<q05>",quantile(Y,0.05),"</q05>")
    q95=paste("<q95>",quantile(Y,0.95),"</q95>")

    return(paste(html,m,sd,q05,q95,collapse=';'))
}

#' temporary analysis. Return HTML string
#' @param X data frame of doe variables
#' @param Y data frame of  results
#' @returnType String
#' @return HTML string of analysis
displayResultsTmp <- function(algorithm,X,Y) {
  displayResults(algorithm,X,Y)
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
# options = list(sample_size = 100, seed = 1, 'model[x]' = '{"x1":"Unif(-1,1)","x2":"Unif(1,2)"}')
# gd = RandomSampling(options)
# 
# X0 = getInitialDesign(gd, input=list(x1=list(min=0,max=1),x2=list(min=0,max=1)), "y")
# Y0 = f(X0)
# # X0 = getInitialDesign(gd, input=list(x2=list(min=0,max=1)), NULL)
# # Y0 = f1(X0)
# Xi = X0
# Yi = Y0
# 
# finished = FALSE
# while (!finished) {
#     Xj = getNextDesign(gd,Xi,Yi)
#     if (is.null(Xj) | length(Xj) == 0) {
#         finished = TRUE
#     } else {
#         Yj = f1(Xj)
#         Xi = rbind(Xi,Xj)
#         Yi = rbind(Yi,Yj)
#     }
# }
# 
# print(displayResults(gd,Xi,Yi))
