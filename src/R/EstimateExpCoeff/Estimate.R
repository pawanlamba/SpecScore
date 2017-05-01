
# Read Data
data <- read.csv('data.csv',sep='\t',stringsAsFactors=F,na.strings=c('','NULL','\\N','null','n/a','None'),strip.white=T)

##estimating staring variables
b = 0.7782*8/max(data$x)

model <- nls('y ~ a - a*(exp(-b*x))',data=data, start=c(a=1,b=b))
sm <- summary(model)
print(sm)

coeff <- as.data.frame(sm$coefficients)
cat(sprintf("Equation Parameters Estimated with Model\nEquation format :a - a* exp(-b*x)\na:%s\nb:%s\n\n",coeff$Estimate[rownames(coeff)=='a'],coeff$Estimate[rownames(coeff)=='b']))
data$predicted <- predict(model)
data$normalized <- data$predicted/max(data$predicted)*0.99
write.table(data,file='output.csv',row.names=F,col.names=T,quote=F)
cat('Printing Output:\n')
print(data)
print('Done')
