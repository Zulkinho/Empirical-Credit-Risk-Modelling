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
