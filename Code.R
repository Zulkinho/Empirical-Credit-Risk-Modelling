#importing files - accomodation and food service activities

#test dataframe
test_data <- read.csv("data/Test_accomodation and food service activities.csv", header = TRUE, sep = ";")
#standardizing data with scale function 
test_data[5:48]<-scale(as.numeric(unlist(test_data[5:48])))
test_data$default <- ifelse(test_data$default == "0", yes="non-defaulted", no="defaulted")
test_data$default <- as.factor(test_data$default)
str(test_data)
View(test_data)

#test dataframe
training_data <- read.csv("data/Train_accomodation and food service activities.csv", header = TRUE, sep = ";")
#standardizing data with scale function ki centrira podatke in jih s tem standardizira 
training_data[5:48]<-scale(as.numeric(unlist(training_data[5:48])))
training_data$default <- ifelse(test=training_data$default == "0,00", yes="non-defaulted", no="defaulted")
training_data$default <- as.factor(training_data$default)
str(training_data)
View(training_data)


#i)

library(ROSE)
table(training_data$default)
test_data.under <- ovun.sample(default~., data=training_data, 
                                  p=0.5, seed=1, 
                                  method="under")$data

table(test_data.under$default)
gr <- factor(unlist(test_data.under[49]))

library(pca3d)
pca <- princomp(test_data.under[5:48], center = TRUE,scale. = TRUE)
pca3d(as.matrix(pca$scores), group=gr)

#on the plot we can see how the data is distributed over firs 3 components colours represent the default status of defaulted costumer class 


#ii)
library(randomForest)
library(ggplot2)
library(cowplot)
set.seed(44)

rf_model1 <- randomForest(training_data$default ~ training_data$f01+ training_data$f02+training_data$f03+training_data$f04+training_data$f05+training_data$f06+training_data$f07+training_data$f08+training_data$f09+training_data$f10+training_data$f11+training_data$f12+training_data$f13+training_data$f14+training_data$f15+training_data$f16+training_data$f17+training_data$f18+training_data$f19+training_data$f21+training_data$f22+training_data$f23+training_data$f24+training_data$f25+training_data$f26+training_data$f27+training_data$f28+training_data$f29+training_data$f30+training_data$f31+training_data$f32+training_data$f33+training_data$f35+training_data$f36+training_data$f37+training_data$f38+training_data$f39+training_data$f40+training_data$f41+training_data$f42+training_data$f43+training_data$f44+training_data$f46+training_data$f47 , data = training_data, proximity=TRUE)
rf_model1

oob.error.data <- data.frame(
  Trees=rep(1:nrow(rf_model1$err.rate), times=3),
  Type=rep(c("OOB", "non-defaulted", "defaulted"), each=nrow(rf_model1$err.rate)),
  Error=c(rf_model1$err.rate[,"OOB"], 
          rf_model1$err.rate[,"non-defaulted"], 
          rf_model1$err.rate[,"defaulted"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(training_data$default ~ training_data$f01+ training_data$f02+training_data$f03+training_data$f04+training_data$f05+training_data$f06+training_data$f07+training_data$f08+training_data$f09+training_data$f10+training_data$f11+training_data$f12+training_data$f13+training_data$f14+training_data$f15+training_data$f16+training_data$f17+training_data$f18+training_data$f19+training_data$f21+training_data$f22+training_data$f23+training_data$f24+training_data$f25+training_data$f26+training_data$f27+training_data$f28+training_data$f29+training_data$f30+training_data$f31+training_data$f32+training_data$f33+training_data$f35+training_data$f36+training_data$f37+training_data$f38+training_data$f39+training_data$f40+training_data$f41+training_data$f42+training_data$f43+training_data$f44+training_data$f46+training_data$f47 , data = training_data, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

rf_model1 <- randomForest(training_data$default ~ training_data$f01+ training_data$f02+training_data$f03+training_data$f04+training_data$f05+training_data$f06+training_data$f07+training_data$f08+training_data$f09+training_data$f10+training_data$f11+training_data$f12+training_data$f13+training_data$f14+training_data$f15+training_data$f16+training_data$f17+training_data$f18+training_data$f19+training_data$f21+training_data$f22+training_data$f23+training_data$f24+training_data$f25+training_data$f26+training_data$f27+training_data$f28+training_data$f29+training_data$f30+training_data$f31+training_data$f32+training_data$f33+training_data$f35+training_data$f36+training_data$f37+training_data$f38+training_data$f39+training_data$f40+training_data$f41+training_data$f42+training_data$f43+training_data$f44+training_data$f46+training_data$f47 , data = training_data, mtry=7, proximity=TRUE, ntree=300)
rf_model1


top_factors<-rf_model1$importance[order(rf_model1$importance[,1],decreasing=TRUE),]
top_factors

#iii)

top_factors_data<- training_data[,c("f07", "f32", "f08", "f16", "f44", "f36", "f31", "f43", "f47", "f29","default")]
str(top_factors_data)
View(top_factors_data)

library(pastecs)
stat_data<-stat.desc(top_factors_data[,c(1:10)])
stat_data<-round(stat_data, 2)
stat_data

summary((top_factors_data))

# top_factors_data$default <- ifelse(top_factors_data$default == "non-defaulted", yes=0, no=1)
# library(vioplot)
# vioplot(top_factors_data$default, horizontal=TRUE, main="Defaulter vs non-defaulted class distribution")
# hist(top_factors_data$default, breaks=4)



