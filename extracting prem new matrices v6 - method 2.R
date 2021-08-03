library(data.table)
library(janitor)
setwd("C:/Users/zdobreva/Desktop/HPPF 2020/HPPF 2020/Summer project/Analysis in R")
load("Data/contact_all.rdata")
country<-"KEN"
m <- data.matrix(contact_all[[country]])

population_data_import<-read.csv("Data/Population data/Population data age bins.csv", header = TRUE)
str(population_data_import)
population_data_import2<-data.table(t(population_data_import))
population_data_import2<-row_to_names(population_data_import2,row_number = 1, remove_row = TRUE)
str(population_data_import2)
population_data<- data.table(age=population_data_import2$age,total=as.numeric(population_data_import2$Kenya))
pop_m<-data.matrix(population_data)
vector<-c(pop_m[-(17:21),])
v<-data.matrix(vector)

vector1<-t(vector)
mat1<-matrix(NA,nrow = 16, ncol = 16)
mat1[,1]<-m[,1]*vector[1]
mat1[,2]<-m[,2]*vector[2]
mat1[,3]<-m[,3]*vector[3]
mat1[,4]<-m[,4]*vector[4]
mat1[,5]<-m[,5]*vector[5]
mat1[,6]<-m[,6]*vector[6]
mat1[,7]<-m[,7]*vector[7]
mat1[,8]<-m[,8]*vector[8]
mat1[,9]<-m[,9]*vector[9]
mat1[,10]<-m[,10]*vector[10]
mat1[,11]<-m[,11]*vector[11]
mat1[,12]<-m[,12]*vector[12]
mat1[,13]<-m[,13]*vector[13]
mat1[,14]<-m[,14]*vector[14]
mat1[,15]<-m[,15]*vector[15]
mat1[,16]<-m[,16]*vector[16]

prem_new <- matrix(NA, nrow = 3, ncol = 3)
colnames(prem_new) <- c("0-17", "18-55", "56-80")
rownames(prem_new) <- c("0-17", "18-55", "56-80")

prem_new[1,1]<-(sum(mat1[1:4,1])+sum(mat1[1:4,2])+sum(mat1[1:4,3])+sum(mat1[1:4,4]))/sum(pop_m[1:4,1])
prem_new[2,1]<-(sum(mat1[4:11,1])+sum(mat1[4:11,2])+sum(mat1[4:11,3])+sum(mat1[4:11,4]))/sum(pop_m[1:4,1])
prem_new[3,1]<-(sum(mat1[12:16,1])+sum(mat1[12:16,2])+sum(mat1[12:16,3])+sum(mat1[12:16,4]))/sum(pop_m[1:4,1])

prem_new[1,2]<-(sum(mat1[1:4,4])+sum(mat1[1:4,5])+sum(mat1[1:4,6])+sum(mat1[1:4,7])+sum(mat1[1:4,8])+sum(mat1[1:4,9])+sum(mat1[1:4,10])+sum(mat1[1:4,11]))/sum(pop_m[4:11,1])
prem_new[2,2]<-(sum(mat1[4:11,4])+sum(mat1[4:11,5])+sum(mat1[4:11,6])+sum(mat1[4:11,7])+sum(mat1[4:11,8])+sum(mat1[4:11,9])+sum(mat1[4:11,10])+sum(mat1[4:11,11]))/sum(pop_m[4:11,1])
prem_new[3,2]<-(sum(mat1[12:16,4])+sum(mat1[12:16,5])+sum(mat1[12:16,6])+sum(mat1[12:16,7])+sum(mat1[12:16,8])+sum(mat1[12:16,9])+sum(mat1[12:16,10])+sum(mat1[12:16,11]))/sum(pop_m[4:11,1])

prem_new[1,3]<-(sum(mat1[1:4,12])+sum(mat1[1:4,13])+sum(mat1[1:4,14])+sum(mat1[1:4,15])+sum(mat1[1:4,16]))/sum(pop_m[12:16,1])
prem_new[2,3]<-(sum(mat1[4:11,12])+sum(mat1[4:11,13])+sum(mat1[4:11,14])+sum(mat1[4:11,15])+sum(mat1[4:11,16]))/sum(pop_m[12:16,1])
prem_new[3,3]<-(sum(mat1[12:16,12])+sum(mat1[12:16,13])+sum(mat1[12:16,14])+sum(mat1[12:16,15])+sum(mat1[12:16,16]))/sum(pop_m[12:16,1])
prem_new
