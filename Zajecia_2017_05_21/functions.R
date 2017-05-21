
# var <- 'AGE'
# data <- trainingData
# defaultIndicator <- "DEFAULT_PAYMENT_NEXT_MONTH"
# nbin=10
# alpha_input = 1

variableTransformBins_2 <- function (var, data=trainingData,defaultIndicator = "DEFAULT_PAYMENT_NEXT_MONTH",
                                     nbin=10, plot_on = 0, alpha_input = 1,return_full = 0) {
  data <- data[is.finite(rowSums(data[,c(var,defaultIndicator)])) , c(var,defaultIndicator)]
  data[,"rownames"] <- rownames(data)
  names(data) <- c("crit","defaultIndicator","rownames")
  n = nrow(data)
  data[,"crit"] <- as.numeric(as.character(data[,"crit"]))
  data[,"rank"] <- rank(data[,"crit"],ties.method="min", na.last = "keep")
  data[,"bin"] <- pmin(data[,"rank"] %/% (n/nbin) + 1,nbin)
  aggrDF <- aggregate(cbind(defaultIndicator,crit) ~ bin, data=data, FUN=function(x) c(count=length(x),avg=mean(x),min=min(x),max=max(x)))
  aggrDF[,"r_bin"] <- 1:nrow(aggrDF)
  aggrDF[,"Smoothed"] <- GetLoess(aggrDF, alpha = alpha_input)
  aggrDF[,"SmoothedStandardized"] <- (aggrDF[,"Smoothed"] - mean(aggrDF[,"Smoothed"])) / sd(aggrDF[,"Smoothed"])
  aggrDF[,"LThrs"] <- aggrDF[,"crit"][,"min"]
  aggrDF[,"UThrs"] <- aggrDF[,"crit"][,"max"]
  
  # plots
  if(plot_on == 1){
    plot <- aggrDF %>% as.matrix %>% as.data.frame() %>% 
      ggplot(aes(x = bin)) + geom_point(aes(y = defaultIndicator.avg, colour = 'Average DR'), size = 2) + 
      geom_line(aes(y = Smoothed, colour = 'Smoothed')) + ylab(var) + scale_x_continuous(breaks = c(1:10))
    print(plot)}
  
  SmoothedDF <- aggrDF[,c("bin","SmoothedStandardized","LThrs","UThrs")]
  data_full <- merge(data,aggrDF, all.x=TRUE, by = 'bin') %>% data.frame
  data <- merge(data,SmoothedDF, all.x=TRUE)[ , c("SmoothedStandardized","LThrs","UThrs","rownames")]
  data <- data[order(as.numeric(data[,"rownames"])),]
  names(data)[1] <- var
  names(data)[2] <- paste("LThrs",var,sep="_")
  names(data)[3] <- paste("UThrs",var,sep="_")
  if(return_full == 1) { return(data_full)} else {return (data[,var])}
}






compare_result <- function(nbin = 10, alpha_input = 1,link_function = 'logit'){

test_model_input <- trainingData
test_model_input[,varNames] <- 
  sapply(varNames, function(x) variableTransformBins_2(x,nbin=nbin, alpha_input = alpha_input))


fullModel_alt <-
  glm(DEFAULT_PAYMENT_NEXT_MONTH ~ 1 + LIMIT_BAL + SEX + EDUCATION + MARRIAGE +
        AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 +
        BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
        PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
      family=binomial(link=link_function), data = test_model_input)
summary(fullModel_alt)

test_model_input[,"model_score"] <- predict(fullModel,trainingData_transformed)
test_model_input[,"predicted_PD"] <- predict(fullModel,trainingData_transformed,type="response")
test_model_input[,"model_score_alt"] <- predict(fullModel_alt,test_model_input)
test_model_input[,"predicted_PD_alt"] <- predict(fullModel_alt,test_model_input,type="response")

print(
  ggplot(test_model_input, aes(x = predicted_PD_alt, y = predicted_PD)) + 
  geom_point() + geom_smooth() + 
    ggtitle(paste('nbin =',nbin,'alpha_loess =',alpha_input, 'link funtion', link_function))
)

# print(
# ggplot(test_model_input) + 
#   geom_density(aes(predicted_PD_alt, fill = 'Alternative'))+
#   geom_density(aes(predicted_PD, fill = 'Initial proposal'), alpha = 0.5)
# )
print(
ggplot(test_model_input, aes(predicted_PD_alt-predicted_PD)) + 
  geom_density(fill = 'chartreuse2') + 
  ggtitle(paste('nbin =',nbin,'alpha_loess =',alpha_input, 'link funtion', link_function))
)
}