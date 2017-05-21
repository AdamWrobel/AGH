setwd("E:/1TB_disk/Dane/Projekty/R_projects/AGH/AGH_walidacja_modeli")
library(zoo)

# read data
trainingData <- read.csv("training_data.csv", row.names = 1)
testData <- read.csv("test_data.csv", row.names = 1)

# linear and log transform functions for plots
lTransform <- function (x,mnEDF,mxEDF,mBIN) 0.1*mBIN + (x-mnEDF)/(mxEDF-mnEDF) * 0.8*mBIN
logTransform <- function (x,mnEDF,mxEDF,mBIN) 0.1*mBIN + log(1+x-mnEDF,5)/log(1+mxEDF-mnEDF,5) * 0.8*mBIN

# Smoothing function
GetLoess <- function(agg,avg,alpha=1,impose_monotonicity=FALSE) {
  x <- 1:length(agg[,"r_bin"])
  if (impose_monotonicity) {y <- rev(forceMonotonicity(rev(as.numeric(agg[,"defaultIndicator"][,"avg"]))))}
  else {y <- as.numeric(agg[,"defaultIndicator"][,"avg"])}
  result <- c(sapply(predict(loess(y~x,span=alpha)),function(x) x),rep(NA,max(agg[,"r_bin"])-length(x)))
  return(result)
}

# plots cumulative accuracy profile
CAP <- function(var, data=trainingData, defaultIndicator = "DEFAULT_PAYMENT_NEXT_MONTH") {
  data[,defaultIndicator] <- as.numeric(data[,defaultIndicator])
  data[,var] <- as.numeric(data[,var])
  
  n <- nrow(data)
  n_d <- sum(data[,defaultIndicator])
  data <- data[order(data[,var]), c(var,defaultIndicator)]
  if (mean(cumsum(data[,defaultIndicator])/n_d)<0.5) {data[,"CAP"] <- cumsum(data[rev(rownames(data)),defaultIndicator])/n_d
  } else {data[,"CAP"] <- cumsum(data[,defaultIndicator])/n_d}
  data[,"FRACTION_CTP"] <- 1:n/n
  data[,"RANDOM_MODEL"] <- data[,"FRACTION_CTP"]
  data[,"PERFECT_MODEL"] <- c(1:n_d , rep(n_d, n-n_d))/n_d
  
  plot(x=data[,"FRACTION_CTP"], y=data[,"CAP"], type="l",xlab="Fraction of counterparties", ylab="Fraction of defaulted counterparties", main="Cumulative accuracy profile" )
  lines(x=data[,"FRACTION_CTP"], y=data[,"RANDOM_MODEL"],lty=3)
  lines(x=data[,"FRACTION_CTP"], y=data[,"PERFECT_MODEL"],lty=2)
  legend("bottomright", lty=c(1,2,3,NA), legend=c("actual model","perfect model", "random model", 
                                                  paste("AR = ",round((mean(data[,"CAP"])-mean(data[,"RANDOM_MODEL"]))/(mean(data[,"PERFECT_MODEL"])-mean(data[,"RANDOM_MODEL"])),3))))
}

# plots receiver operating characteristics curve
ROC <- function(var, data=trainingData, defaultIndicator = "DEFAULT_PAYMENT_NEXT_MONTH") {
  n <- nrow(data)
  n_d <- sum(data[,defaultIndicator])
  data[,defaultIndicator] <- as.numeric(data[,defaultIndicator])
  data <- data[order(data[,var]),]
  if (mean(cumsum(data[,defaultIndicator])/n_d)>0.5) {data <- data[order(data[,var], decreasing=TRUE),]}
  ROC <- data.frame(TPR=cumsum(data[,defaultIndicator])/sum(data[,defaultIndicator]), FPR=cumsum(!data[,defaultIndicator])/sum(!data[,defaultIndicator]))
  AUC <- sum(diff(ROC[,"TPR"])*rollmean(ROC[,"FPR"],2))
  plot(x=ROC[,"TPR"], y=ROC[,"FPR"], main="Receiver operating characteristic curve", type="l", xlab="False positive rate", ylab="True positive rate")
  lines(x=c(0,1),y=c(0,1),lty=3)
  legend("bottomright", lty=c(1,3,NA), legend=c("actual model","random model", paste("AUC = ",round(AUC,3))))
}

# plots average default rate per bin
defaultRatePerBin <- function(var,data=trainingData,defaultIndicator = "DEFAULT_PAYMENT_NEXT_MONTH",nbin=10,span_param=1) {
  data[,defaultIndicator] <- as.numeric(as.character(data[,defaultIndicator]))
  data[,var] <- as.numeric(as.character(data[,var]))
  data <- data[ ! is.na(data[,defaultIndicator]) & ! is.na(data[,var]) , c(defaultIndicator,var) ]
  n = nrow(data)
  names(data)[2] <- "crit"
  data[,"rank"] <- rank(data[,"crit"],ties.method="min")
  data[,"bin"] <- pmin(data[,"rank"] %/% (n/nbin) + 1,nbin)
  data[,"defaultIndicator"] <- data[,defaultIndicator]
  aggrDF <- aggregate(cbind(defaultIndicator,crit) ~ bin, data=data, FUN=function(x) c(count=length(x),avg=100*mean(x),max=100*max(x),min=100*min(x)) )
  aggrDF[,"r_bin"] <- 1:nrow(aggrDF)
  names(aggrDF)[3] <- "crit"
  
  aggrDF[,"Smoothed"] <- GetLoess(aggrDF,alpha=span_param)
  aggrDF[,"SmoothedStandardized"] <- (aggrDF[,"Smoothed"] - mean(aggrDF[,"Smoothed"]) ) / sd(aggrDF[,"Smoothed"])
  
  aggrDF[,"defaultIndicator.avg"] <- aggrDF[,"defaultIndicator"][,"avg"]
  
  linearFit <- lm(defaultIndicator.avg ~ r_bin, aggrDF)
  
  aggrDF[,"linear"] <- linearFit$fitted.values
  mxEDF <- max(aggrDF[,"defaultIndicator.avg"])
  mnEDF <- min(aggrDF[,"defaultIndicator.avg"])
  mBIN <- max(aggrDF[,"defaultIndicator"][,"count"])
  aggrDF[,"tMean"] <- sapply(aggrDF[,"defaultIndicator.avg"], function(x) lTransform(x,mnEDF,mxEDF,mBIN))
  aggrDF[,"tSmoothed"] <- sapply(aggrDF[,"Smoothed"], function(x) lTransform(x,mnEDF,mxEDF,mBIN))
  aggrDF[,"tLinear"] <- sapply(aggrDF[,"linear"], function(x) lTransform(x,mnEDF,mxEDF,mBIN))
  
  par(mar=c(5.1, 4.1, 2.1, 14.1), xpd=TRUE)
  DFPlot <- barplot(	aggrDF[,"defaultIndicator"][,"count"],
                     col="white",
                     xlab="Criterion values", 
                     ylab="No. observations", 
                     main=NULL)
  xMin <- min(DFPlot)
  xMax <- max(DFPlot)
  xs <- seq(xMin,xMax,0.01)
  # lines(x = DFPlot, y=aggrDF[,"tLinear"], col='red', lwd=1,lty=5)
  points(x = DFPlot, y=aggrDF[,"tMean"],col="red",pch=20)
  sm <- loess(aggrDF[,"tSmoothed"] ~ DFPlot)
  lines(x=xs,predict(sm,xs),col="red",lwd=1,lty=2)
  
  axis(4, at = seq(lTransform(mnEDF,mnEDF,mxEDF,mBIN),lTransform(mxEDF,mnEDF,mxEDF,mBIN),length.out=8), labels = round(seq(mnEDF,mxEDF,length.out=8),2))
  
  ThresholdsX <- (DFPlot[2:nrow(aggrDF)] + DFPlot[1:(nrow(aggrDF)-1)]) / 2
  ThresholdsY <- round((aggrDF[2:nrow(aggrDF),"crit"][,"min"] + aggrDF[1:(nrow(aggrDF)-1),"crit"][,"max"]) / 2 , 2)
  
  axis(1, at = ThresholdsX , label = ThresholdsY)
  
  legend("right", 	inset=c(-0.45,0), 
         legend = c(
           "Mean default rate", 
           "Smoothed default rate",
           "No. observations"),
         lwd=c(NA,1,NA), lty=c(NA,5,NA), pch=c(20,NA,0),
         col=c("red","red","black"), bty="n" )
}

# calculate transformed variables
variableTransformBins <- function (var, data=trainingData,defaultIndicator = "DEFAULT_PAYMENT_NEXT_MONTH",nbin=10) {
  data <- data[is.finite(rowSums(data[,c(var,defaultIndicator)])) , c(var,defaultIndicator)]
  data[,"rownames"] <- rownames(data)
  names(data) <- c("crit","defaultIndicator","rownames")
  n = nrow(data)
  data[,"crit"] <- as.numeric(as.character(data[,"crit"]))
  data[,"rank"] <- rank(data[,"crit"],ties.method="min", na.last = "keep")
  data[,"bin"] <- pmin(data[,"rank"] %/% (n/nbin) + 1,nbin)
  aggrDF <- aggregate(cbind(defaultIndicator,crit) ~ bin, data=data, FUN=function(x) c(count=length(x),avg=mean(x),min=min(x),max=max(x)))
  aggrDF[,"r_bin"] <- 1:nrow(aggrDF)
  aggrDF[,"Smoothed"] <- GetLoess(aggrDF)
  aggrDF[,"SmoothedStandardized"] <- (aggrDF[,"Smoothed"] - mean(aggrDF[,"Smoothed"])) / sd(aggrDF[,"Smoothed"])
  aggrDF[,"LThrs"] <- aggrDF[,"crit"][,"min"]
  aggrDF[,"UThrs"] <- aggrDF[,"crit"][,"max"]
  SmoothedDF <- aggrDF[,c("bin","SmoothedStandardized","LThrs","UThrs")]
  data <- merge(data,SmoothedDF, all.x=TRUE)[ , c("SmoothedStandardized","LThrs","UThrs","rownames")]
  data <- data[order(as.numeric(data[,"rownames"])),]
  names(data)[1] <- var
  names(data)[2] <- paste("LThrs",var,sep="_")
  names(data)[3] <- paste("UThrs",var,sep="_")
  return (data[,var]) 
}

# list variables to transform
varNames <- c('LIMIT_BAL','AGE','BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6','PAY_AMT1','PAY_AMT2','PAY_AMT3','PAY_AMT4','PAY_AMT5','PAY_AMT6')

# transform variables via bins
trainingData_transformed <- trainingData
trainingData_transformed[,varNames] <- sapply(varNames, function(x) variableTransformBins(x,data=trainingData))

# plot univariate graphs
CAP("PAY_AMT1")
ROC("PAY_AMT1")
defaultRatePerBin("PAY_AMT1")

# estimate the full logit model
fullModel <-glm(DEFAULT_PAYMENT_NEXT_MONTH ~ 1 + LIMIT_BAL + SEX + EDUCATION + MARRIAGE +
                  AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 +
                  BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
                  PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                family=binomial(link="logit"),data=trainingData_transformed)

# model summary
summary(fullModel)

# calculate model predictions (score/PD)		
trainingData_transformed[,"model_score"] <- predict(fullModel,trainingData_transformed)
trainingData_transformed[,"predicted_PD"] <- predict(fullModel,trainingData_transformed,type="response")

# has the performance improved?
CAP("model_score",data=trainingData_transformed)
ROC("model_score",data=trainingData_transformed)
defaultRatePerBin("model_score",data=trainingData_transformed)			   
