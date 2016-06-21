### Stimulated data (Collinearity)

## Set working directory
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

## Import library
library(Hmisc)
library(car)

## Init variables
n = 100
set.seed(1)

## Define functions
writeLine <- function(arg1,arg2){
  write(paste0(arg2,collapse=","),file=arg1,append=TRUE)  
}

corrGraph <- function(dataset,inputMean,inputSD){
  vc <- varclus(~.,
                data=dataset[c('x1','x2','x3')],
                similarity = "spearman",
                trans="abs")
  png(height=1080, width=1080, pointsize=15, file=paste0("./output/Correlation_Mean",inputMean,"_SD",inputSD,".png"))
  plot(vc)
  abline(h=0.3) # threashold 0.7 on graph
  dev.off()
}

anovaToCsv <- function(inputAnova,varOrder,inputSD){
  writeLine("./output/Summarized_output.csv",
            # SD, Variable Order, Chisq, F-sum square, F score, F-residual sum square, Deviance, D-residual deviance, D-Null residual
            c(inputSD,
              varOrder,
              inputAnova$Chisq,
              inputAnova$FSs,
              inputAnova$FF,
              inputAnova$FRsdSs[1],
              inputAnova$DDeviance,
              inputAnova$DRsd,
              inputAnova$DNullRsd[1])
  )
}

interpretModel <- function(model,inputSD){
  
  refOrder = c('x1','x2','x3')
  lrAnova <- data.frame(
                # Likelyhood Ratio Chisquare
                Chisq = Anova(model,test.statistic ="LR")[refOrder,][,1],
                # F-test - Rsd SS, Sum square and F-value
                FRsdSs = Anova(model,test.statistic = "F")$SS[4],
                FSs = Anova(model,test.statistic = "F")[refOrder,]$SS,
                FF = Anova(model,test.statistic ="F")[refOrder,]$F,
                # Deviance - Null resd dev, Deviance and Residual Deviance
                DNullRsd = anova(model)$`Resid. Dev`[1],
                DDeviance = anova(model)[refOrder,]$Deviance,
                DRsd = anova(model)[refOrder,]$`Resid. Dev`
                )
  anovaToCsv(lrAnova,names(model$coefficients)[-1],inputSD)
  return(lrAnova)
}

generateHighlyCorrelatedDataset <- function(nInstance,inputMean,inputSD){
  
  # datasetOutput - x2,x1 is highly correlated while x3 is not correated with any explanatory variables
  datasetOutput <- data.frame(x1=runif(nInstance),x3=runif(nInstance))
  
  # dataset$x(n) = dataset$x(n-1) + random noise (Normal distribution, Mean = inputMean and SD is inputSD)
  datasetOutput$x2 <- datasetOutput$x1 + rnorm(n,mean=inputMean,sd=inputSD)
  
  # Export correlation graph
  corrGraph(datasetOutput,inputMean,inputSD)
  
  return(datasetOutput)
}

experiment <- function(dataset,inputSD){
  
  # Generate equation
  # y = 10 + Ax1 + Bx2 + Cx3 + random noise (Mean = 0, SD = 0.5)
  # Exp1: 10 10 1 -> Only change in Anova deviance
  
  A <- 10
  B <- 10
  C <- 1
  dataset$y <- 10 + (A*dataset$x1) + (B*dataset$x2) + (C*dataset$x3) + rnorm(100,mean = 0, sd = 0.5)
  
  pattern <- e1071::permutations(3)
  for(i in 1:nrow(pattern)){
    #Generate formula
    formulaText <- paste0("y~",paste0(paste0("x",pattern[i,]),collapse="+"))
    # Build glm models
    glmModel <- glm(as.formula(formulaText),data=dataset)
    
    # Intepret models
    interpretModel(glmModel,inputSD)
    
  }
  writeLine("./output/Summarized_output.csv"," ")
  
}

## Main
# Create output folder
dir.create(file.path(paste0(getwd(), '/output/')), showWarnings = FALSE)
# Init header of output file
  writeLine("./output/Summarized_output.csv",paste0(c(
            "Dataset",
            "Var1","Var2","Var3",
            "Chisq-x1","Chisq-x2","Chisq-x3",
            "Fss-x1","Fss-x2","Fss-x3",
            "FF-x1","FF-x2","FF-x3",
            "FFRsdss",
            "D-x1","D-x2","D-x3",
            "DRsdD-x1","DRsdD-x2","DRsdD-x3",
            "DNullRsd"),collapse = ","
  ))

# datasetLow - There is no collinearity among variables
datasetLow <- data.frame(x1=runif(n),x2=runif(n),x3=runif(n))
# datasetHigh - x1,x2 are highly correlated
datasetHigh0.5 <- generateHighlyCorrelatedDataset(100,0,0.5)
datasetHigh0.25 <- generateHighlyCorrelatedDataset(100,0,0.25)
datasetHigh0.1 <- generateHighlyCorrelatedDataset(100,0,0.1)
datasetHigh0.05 <- generateHighlyCorrelatedDataset(100,0,0.05)

experiment(datasetLow,"No Highly Correlated")
experiment(datasetHigh0.05,"SD=0.05")
experiment(datasetHigh0.1,"SD=0.1")
experiment(datasetHigh0.25,"SD=0.25")
experiment(datasetHigh0.5,"SD=0.5")

## EOF
