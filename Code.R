#importing files - accomodation and food service activities

#test dataframe
test_data <- read.csv("data/Test_accomodation and food service activities.csv", header = TRUE, sep = ";")
#standardizing data with scale function 
test_data[5:48]<-scale(as.numeric(unlist(test_data[5:48])))
View(test_data)

#test dataframe
training_data <- read.csv("data/Train_accomodation and food service activities.csv", header = TRUE, sep = ";")
#standardizing data with scale function ki centrira podatke in jih s tem standardizira 
training_data[5:48]<-scale(as.numeric(unlist(training_data[5:48])))
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
pca3d(as.matrix(pca$scores, group=gr))


