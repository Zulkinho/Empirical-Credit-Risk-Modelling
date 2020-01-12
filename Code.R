#importing files - accomodation and food service activities

#test dataframe
test_data <- read.csv("data/Test_accomodation and food service activities.csv", header = TRUE, sep = ";")
#standardizing data with scale function 
test_data[5:48]<-scale(as.numeric(unlist(test_data[5:48])))
View(test_data)

test_data$default <- ifelse(test_data$default == "0,00", yes="non-defaulted", no="defaulted")
test_data$default <- as.factor(test_data$default)
str(test_data)

#test dataframe
training_data <- read.csv("data/Train_accomodation and food service activities.csv", header = TRUE, sep = ";")
#standardizing data with scale function ki centrira podatke in jih s tem standardizira 
training_data[5:48]<-scale(as.numeric(unlist(training_data[5:48])))
View(training_data)
str(training_data)

training_data$default <- ifelse(test=training_data$default == "0,00", yes="non-defaulted", no="defaulted")
training_data$default <- as.factor(training_data$default)

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

rf_model1 <- randomForest(training_data$default ~ training_data$f01+ training_data$f02+training_data$f03+training_data$f04+training_data$f05+training_data$f06+training_data$f07+training_data$f08+training_data$f09+training_data$f10+training_data$f11 , data = training_data, proximity=TRUE)
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
  temp.model <- randomForest(training_data$default ~ training_data$f01+ training_data$f02+training_data$f03+training_data$f04+training_data$f05+training_data$f06+training_data$f07+training_data$f08+training_data$f09+training_data$f10+training_data$f11 , data = training_data, mtry=i, ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

rf_model1 <- randomForest(training_data$default ~ training_data$f01+ training_data$f02+training_data$f03+training_data$f04+training_data$f05+training_data$f06+training_data$f07+training_data$f08+training_data$f09+training_data$f10+training_data$f11 , data = training_data, proximity=TRUE,ntree=100)
rf_model1


