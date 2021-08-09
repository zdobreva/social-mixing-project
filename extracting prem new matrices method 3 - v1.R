library(data.table)
library(janitor)
setwd("C:/Users/zdobreva/Desktop/HPPF 2020/HPPF 2020/Summer project/Analysis in R")
load("Data/contact_all.rdata")
country<-"KEN"
m <- data.matrix(contact_all[[country]])

population_data_import<-read.csv("Data/Population data/Population Data by age.csv", header = TRUE)
population_data<-population_data<- data.table(age=population_data_import$age,total=population_data_import$Cameroon)

mat1<-matrix(NA,nrow = 16, ncol = 81)
mat1[,1:5]<-m[,1]
mat1[,6:10]<-m[,2]
mat1[,11:15]<-m[,3]
mat1[,16:20]<-m[,4]
mat1[,21:25]<-m[,5]
mat1[,26:30]<-m[,6]
mat1[,31:35]<-m[,7]
mat1[,36:40]<-m[,8]
mat1[,41:45]<-m[,9]
mat1[,46:50]<-m[,10]
mat1[,51:55]<-m[,11]
mat1[,56:60]<-m[,12]
mat1[,61:65]<-m[,13]
mat1[,66:70]<-m[,14]
mat1[,71:75]<-m[,15]
mat1[,76:80]<-m[,16]

mat2<-matrix(NA, nrow=80, ncol=80)

mat2[1:5,]<-mat1[1,1:80]*(1/5)          
mat2[6:10,]<-mat1[2,1:80]*(1/5)          
mat2[11:15,]<-mat1[3,1:80]*(1/5)
mat2[16:20,]<-mat1[4,1:80]*(1/5)
mat2[21:25,]<-mat1[5,1:80]*(1/5)
mat2[26:30,]<-mat1[6,1:80]*(1/5)
mat2[31:35,]<-mat1[7,1:80]*(1/5)
mat2[36:40,]<-mat1[8,1:80]*(1/5)
mat2[41:45,]<-mat1[9,1:80]*(1/5)
mat2[46:50,]<-mat1[10,1:80]*(1/5)
mat2[51:55,]<-mat1[11,1:80]*(1/5)
mat2[56:60,]<-mat1[12,1:80]*(1/5)
mat2[61:65,]<-mat1[13,1:80]*(1/5)
mat2[66:70,]<-mat1[14,1:80]*(1/5)
mat2[71:75,]<-mat1[15,1:80]*(1/5)
mat2[76:80,]<-mat1[16,1:80]*(1/5)

mat3<-matrix(NA,nrow = 3, ncol = 80)
mat3[1,]<-colSums(mat2[1:18,])
mat3[2,]<-colSums(mat2[19:56,])
mat3[3,]<-colSums(mat2[57:80,])

pop_mat<-matrix(data=NA,nrow = 80,ncol = 1)
pop_mat[1:5,1]<-as.numeric(population_data[1,2]/sum(population_data[1:18,2]))
pop_mat[6:10,1]<-as.numeric(population_data[6,2]/sum(population_data[1:18,2]))
pop_mat[11:15,1]<-as.numeric(population_data[11,2]/sum(population_data[1:18,2]))
pop_mat[16:18,1]<-as.numeric(population_data[16,2]/sum(population_data[1:18,2]))
pop_mat[19:20,1]<-as.numeric(population_data[19,2]/sum(population_data[19:56,2]))
pop_mat[21:25,1]<-as.numeric(population_data[21,2]/sum(population_data[19:56,2]))
pop_mat[26:30,1]<-as.numeric(population_data[26,2]/sum(population_data[19:56,2]))
pop_mat[31:35,1]<-as.numeric(population_data[31,2]/sum(population_data[19:56,2]))
pop_mat[36:40,1]<-as.numeric(population_data[36,2]/sum(population_data[19:56,2]))
pop_mat[41:45,1]<-as.numeric(population_data[41,2]/sum(population_data[19:56,2]))
pop_mat[46:50,1]<-as.numeric(population_data[46,2]/sum(population_data[19:56,2]))
pop_mat[51:55,1]<-as.numeric(population_data[51,2]/sum(population_data[19:56,2]))
pop_mat[56,1]<-as.numeric(population_data[56,2]/sum(population_data[19:56,2]))
pop_mat[57:60,1]<-as.numeric(population_data[57,2]/sum(population_data[57:80,2]))
pop_mat[61:65,1]<-as.numeric(population_data[61,2]/sum(population_data[57:80,2]))
pop_mat[66:70,1]<-as.numeric(population_data[66,2]/sum(population_data[57:80,2]))
pop_mat[71:75,1]<-as.numeric(population_data[71,2]/sum(population_data[57:80,2]))
pop_mat[76:80,1]<-as.numeric(population_data[76,2]/sum(population_data[57:80,2]))

mat4<-matrix(NA, nrow = 3, ncol=3)
mat4[1,1]<-sum(mat3[1,1:18]*pop_mat[1:18,1])
mat4[2,1]<-sum(mat3[2,1:18]*pop_mat[1:18,1])
mat4[3,1]<-sum(mat3[3,1:18]*pop_mat[1:18,1])

mat4[1,2]<-sum(mat3[1,19:56]*pop_mat[19:56,1])
mat4[2,2]<-sum(mat3[2,19:56]*pop_mat[19:56,1])
mat4[3,2]<-sum(mat3[3,19:56]*pop_mat[19:56,1])

mat4[1,3]<-sum(mat3[1,57:80]*pop_mat[57:80,1])
mat4[2,3]<-sum(mat3[2,57:80]*pop_mat[57:80,1])
mat4[3,3]<-sum(mat3[3,57:80]*pop_mat[57:80,1])

prem_new<-mat4
colnames(prem_new) <- c("0-17", "18-55", "56-80")
rownames(prem_new) <- c("0-17", "18-55", "56-80")
prem_new

#Making the matrix symmetric
age_groups <- data.table(
  age_low = c(0, 18, 56),
  age_high = c(17, 55, 80)
)
age_groups[, "name"] <- paste0(age_groups[, age_low], "-", age_groups[, age_high])


N_new <- sapply(
  1:nrow(age_groups),
  function(i, age_groups, population_data){
    sum(population_data[age >= age_groups[i, age_low] & age <= age_groups[i, age_high], total])
  }, age_groups, population_data
)

population_matrix=prem_new;
for(i in 1:nrow(population_matrix)){
  for(j in 1:ncol(population_matrix)){
    population_matrix[i,j] = (population_matrix[i,j] + population_matrix[j,i]*(N_new[j]/N_new[i]))/2
  }
}
population_matrix
