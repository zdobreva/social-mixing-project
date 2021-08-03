setwd("C:/Users/zdobreva/Desktop/HPPF 2020/HPPF 2020/Summer project/Analysis in R")
load("Data/contact_all.rdata")
country<-"KEN"
m <- data.matrix(contact_all[[country]])
library(data.table)

population_data_import<- read.csv("Data/Population data/Population Data by age.csv")
population_data<- data.table(age=population_data_import$age,total=population_data_import$Kenya)
pop_m<-matrix(data=NA,nrow = 16,ncol=1)
pop_m[1,1]<-sum(population_data[1:5,2])
pop_m[2,1]<-sum(population_data[6:10,2])
pop_m[3,1]<-sum(population_data[11:15,2])
pop_m[4,1]<-sum(population_data[16:20,2])
pop_m[5,1]<-sum(population_data[21:25,2])
pop_m[6,1]<-sum(population_data[26:30,2])
pop_m[7,1]<-sum(population_data[31:35,2])
pop_m[8,1]<-sum(population_data[36:40,2])
pop_m[9,1]<-sum(population_data[41:45,2])
pop_m[10,1]<-sum(population_data[46:50,2])
pop_m[11,1]<-sum(population_data[51:55,2])
pop_m[12,1]<-sum(population_data[56:60,2])
pop_m[13,1]<-sum(population_data[61:65,2])
pop_m[14,1]<-sum(population_data[66:70,2])
pop_m[15,1]<-sum(population_data[71:75,2])
pop_m[16,1]<-sum(population_data[76:80,2])


#'Country codes:
#'Cameroon = CMR
#'Cote d'Ivoire = CIV
#'DRC = ZAR
#'Egypt = EGY
#'Ethiopia = ETH
#'Ghana = GHA
#'Guinea = GIN
#'Kenya = KEN
#'Liberia = LBR
#'Mozambique = MOZ
#'Nigeria = NGA
#'Senegal = SEN
#'South Africa = ZAF
#'Sudan = SDN
#'Tunisia = TUN
#'Uganda = UGA
#'Zambia = ZMB
#'Zimbabwe = ZWE
#'}
#'Creating a 3 by 3 matrix to input contacts into
prem_new <- matrix(1:9, nrow = 3, ncol = 3)
colnames(prem_new) <- c("0-17", "18-55", "56-80")
rownames(prem_new) <- c("0-17", "18-55", "56-80")

#Reducing participant bins-------
mat1<-matrix(data=NA, nrow = 16, ncol=3)
mat1[1,1]<-(m[1,1]*pop_m[1,1]+m[1,2]*pop_m[2,1]+m[1,3]*pop_m[3,1]+m[1,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[1,2]<-(m[1,4]*pop_m[4,1]+m[1,5]*pop_m[5,1]+m[1,6]*pop_m[6,1]+m[1,7]*pop_m[7,1]+m[1,8]*pop_m[8,1]+m[1,9]*pop_m[9,1]+m[1,10]*pop_m[10,1]+m[1,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[1,3]<-(m[1,12]*pop_m[12,1]+m[1,13]*pop_m[13,1]+m[1,14]*pop_m[14,1]+m[1,15]*pop_m[15,1]+m[1,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[2,1]<-(m[2,1]*pop_m[1,1]+m[2,2]*pop_m[2,1]+m[2,3]*pop_m[3,1]+m[2,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[2,2]<-(m[2,4]*pop_m[4,1]+m[2,5]*pop_m[5,1]+m[2,6]*pop_m[6,1]+m[2,7]*pop_m[7,1]+m[2,8]*pop_m[8,1]+m[2,9]*pop_m[9,1]+m[2,10]*pop_m[10,1]+m[2,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[2,3]<-(m[2,12]*pop_m[12,1]+m[2,13]*pop_m[13,1]+m[2,14]*pop_m[14,1]+m[2,15]*pop_m[15,1]+m[2,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[3,1]<-(m[3,1]*pop_m[1,1]+m[3,2]*pop_m[2,1]+m[3,3]*pop_m[3,1]+m[3,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[3,2]<-(m[3,4]*pop_m[4,1]+m[3,5]*pop_m[5,1]+m[3,6]*pop_m[6,1]+m[3,7]*pop_m[7,1]+m[3,8]*pop_m[8,1]+m[3,9]*pop_m[9,1]+m[3,10]*pop_m[10,1]+m[3,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[3,3]<-(m[3,12]*pop_m[12,1]+m[3,13]*pop_m[13,1]+m[3,14]*pop_m[14,1]+m[3,15]*pop_m[15,1]+m[3,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[4,1]<-(m[4,1]*pop_m[1,1]+m[4,2]*pop_m[2,1]+m[4,3]*pop_m[3,1]+m[4,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[4,2]<-(m[4,4]*pop_m[4,1]+m[4,5]*pop_m[5,1]+m[4,6]*pop_m[6,1]+m[4,7]*pop_m[7,1]+m[4,8]*pop_m[8,1]+m[4,9]*pop_m[9,1]+m[4,10]*pop_m[10,1]+m[4,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[4,3]<-(m[4,12]*pop_m[12,1]+m[4,13]*pop_m[13,1]+m[4,14]*pop_m[14,1]+m[4,15]*pop_m[15,1]+m[4,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[6,1]<-(m[6,1]*pop_m[1,1]+m[6,2]*pop_m[2,1]+m[6,3]*pop_m[3,1]+m[6,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[6,2]<-(m[6,4]*pop_m[4,1]+m[6,5]*pop_m[5,1]+m[6,6]*pop_m[6,1]+m[6,7]*pop_m[7,1]+m[6,8]*pop_m[8,1]+m[6,9]*pop_m[9,1]+m[6,10]*pop_m[10,1]+m[6,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[6,3]<-(m[6,12]*pop_m[12,1]+m[6,13]*pop_m[13,1]+m[6,14]*pop_m[14,1]+m[6,15]*pop_m[15,1]+m[6,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[5,1]<-(m[5,1]*pop_m[1,1]+m[5,2]*pop_m[2,1]+m[5,3]*pop_m[3,1]+m[5,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[5,2]<-(m[5,4]*pop_m[4,1]+m[5,5]*pop_m[5,1]+m[5,6]*pop_m[6,1]+m[5,7]*pop_m[7,1]+m[5,8]*pop_m[8,1]+m[5,9]*pop_m[9,1]+m[5,10]*pop_m[10,1]+m[5,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[5,3]<-(m[5,12]*pop_m[12,1]+m[5,13]*pop_m[13,1]+m[5,14]*pop_m[14,1]+m[5,15]*pop_m[15,1]+m[5,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[7,1]<-(m[7,1]*pop_m[1,1]+m[7,2]*pop_m[2,1]+m[7,3]*pop_m[3,1]+m[7,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[7,2]<-(m[7,4]*pop_m[4,1]+m[7,5]*pop_m[5,1]+m[7,6]*pop_m[6,1]+m[7,7]*pop_m[7,1]+m[7,8]*pop_m[8,1]+m[7,9]*pop_m[9,1]+m[7,10]*pop_m[10,1]+m[7,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[7,3]<-(m[7,12]*pop_m[12,1]+m[7,13]*pop_m[13,1]+m[7,14]*pop_m[14,1]+m[7,15]*pop_m[15,1]+m[7,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[8,1]<-(m[8,1]*pop_m[1,1]+m[8,2]*pop_m[2,1]+m[8,3]*pop_m[3,1]+m[8,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[8,2]<-(m[8,4]*pop_m[4,1]+m[8,5]*pop_m[5,1]+m[8,6]*pop_m[6,1]+m[8,7]*pop_m[7,1]+m[8,8]*pop_m[8,1]+m[8,9]*pop_m[9,1]+m[8,10]*pop_m[10,1]+m[8,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[8,3]<-(m[8,12]*pop_m[12,1]+m[8,13]*pop_m[13,1]+m[8,14]*pop_m[14,1]+m[8,15]*pop_m[15,1]+m[8,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[9,1]<-(m[9,1]*pop_m[1,1]+m[9,2]*pop_m[2,1]+m[9,3]*pop_m[3,1]+m[9,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[9,2]<-(m[9,4]*pop_m[4,1]+m[9,5]*pop_m[5,1]+m[9,6]*pop_m[6,1]+m[9,7]*pop_m[7,1]+m[9,8]*pop_m[8,1]+m[9,9]*pop_m[9,1]+m[9,10]*pop_m[10,1]+m[9,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[9,3]<-(m[9,12]*pop_m[12,1]+m[9,13]*pop_m[13,1]+m[9,14]*pop_m[14,1]+m[9,15]*pop_m[15,1]+m[9,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[10,1]<-(m[10,1]*pop_m[1,1]+m[10,2]*pop_m[2,1]+m[10,3]*pop_m[3,1]+m[10,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[10,2]<-(m[10,4]*pop_m[4,1]+m[10,5]*pop_m[5,1]+m[10,6]*pop_m[6,1]+m[10,7]*pop_m[7,1]+m[10,8]*pop_m[8,1]+m[10,9]*pop_m[9,1]+m[10,10]*pop_m[10,1]+m[10,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[10,3]<-(m[10,12]*pop_m[12,1]+m[10,13]*pop_m[13,1]+m[10,14]*pop_m[14,1]+m[10,15]*pop_m[15,1]+m[10,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[11,1]<-(m[11,1]*pop_m[1,1]+m[11,2]*pop_m[2,1]+m[11,3]*pop_m[3,1]+m[11,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[11,2]<-(m[11,4]*pop_m[4,1]+m[11,5]*pop_m[5,1]+m[11,6]*pop_m[6,1]+m[11,7]*pop_m[7,1]+m[11,8]*pop_m[8,1]+m[11,9]*pop_m[9,1]+m[11,10]*pop_m[10,1]+m[11,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[11,3]<-(m[11,12]*pop_m[12,1]+m[11,13]*pop_m[13,1]+m[11,14]*pop_m[14,1]+m[11,15]*pop_m[15,1]+m[11,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[12,1]<-(m[12,1]*pop_m[1,1]+m[12,2]*pop_m[2,1]+m[12,3]*pop_m[3,1]+m[12,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[12,2]<-(m[12,4]*pop_m[4,1]+m[12,5]*pop_m[5,1]+m[12,6]*pop_m[6,1]+m[12,7]*pop_m[7,1]+m[12,8]*pop_m[8,1]+m[12,9]*pop_m[9,1]+m[12,10]*pop_m[10,1]+m[12,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[12,3]<-(m[12,12]*pop_m[12,1]+m[12,13]*pop_m[13,1]+m[12,14]*pop_m[14,1]+m[12,15]*pop_m[15,1]+m[12,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[13,1]<-(m[13,1]*pop_m[1,1]+m[13,2]*pop_m[2,1]+m[13,3]*pop_m[3,1]+m[13,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[13,2]<-(m[13,4]*pop_m[4,1]+m[13,5]*pop_m[5,1]+m[13,6]*pop_m[6,1]+m[13,7]*pop_m[7,1]+m[13,8]*pop_m[8,1]+m[13,9]*pop_m[9,1]+m[13,10]*pop_m[10,1]+m[13,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[13,3]<-(m[13,12]*pop_m[12,1]+m[13,13]*pop_m[13,1]+m[13,14]*pop_m[14,1]+m[13,15]*pop_m[15,1]+m[13,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[14,1]<-(m[14,1]*pop_m[1,1]+m[14,2]*pop_m[2,1]+m[14,3]*pop_m[3,1]+m[14,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[14,2]<-(m[14,4]*pop_m[4,1]+m[14,5]*pop_m[5,1]+m[14,6]*pop_m[6,1]+m[14,7]*pop_m[7,1]+m[14,8]*pop_m[8,1]+m[14,9]*pop_m[9,1]+m[14,10]*pop_m[10,1]+m[14,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[14,3]<-(m[14,12]*pop_m[12,1]+m[14,13]*pop_m[13,1]+m[14,14]*pop_m[14,1]+m[14,15]*pop_m[15,1]+m[14,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[15,1]<-(m[15,1]*pop_m[1,1]+m[15,2]*pop_m[2,1]+m[15,3]*pop_m[3,1]+m[15,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[15,2]<-(m[15,4]*pop_m[4,1]+m[15,5]*pop_m[5,1]+m[15,6]*pop_m[6,1]+m[15,7]*pop_m[7,1]+m[15,8]*pop_m[8,1]+m[15,9]*pop_m[9,1]+m[15,10]*pop_m[10,1]+m[15,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[15,3]<-(m[15,12]*pop_m[12,1]+m[15,13]*pop_m[13,1]+m[15,14]*pop_m[14,1]+m[15,15]*pop_m[15,1]+m[15,16]*pop_m[16,1])/sum(pop_m[12:16,1])

mat1[16,1]<-(m[16,1]*pop_m[1,1]+m[16,2]*pop_m[2,1]+m[16,3]*pop_m[3,1]+m[16,4]*pop_m[4,1])/sum(pop_m[1:4,1])
mat1[16,2]<-(m[16,4]*pop_m[4,1]+m[16,5]*pop_m[5,1]+m[16,6]*pop_m[6,1]+m[16,7]*pop_m[7,1]+m[16,8]*pop_m[8,1]+m[16,9]*pop_m[9,1]+m[16,10]*pop_m[10,1]+m[16,11]*pop_m[11,1])/sum(pop_m[4:11,1])
mat1[16,3]<-(m[16,12]*pop_m[12,1]+m[16,13]*pop_m[13,1]+m[16,14]*pop_m[14,1]+m[16,15]*pop_m[15,1]+m[16,16]*pop_m[16,1])/sum(pop_m[12:16,1])
mat1

#Reducing contact bins-------
prem_new[1,1]<-(mat1[1,1]*pop_m[1,1]+mat1[2,1]*pop_m[2,1]+mat1[3,1]*pop_m[3,1]+mat1[4,1]*pop_m[4,1])/sum(pop_m[1:4,1])
prem_new[1,2]<-(mat1[1,2]*pop_m[1,1]+mat1[2,2]*pop_m[2,1]+mat1[3,2]*pop_m[3,1]+mat1[4,2]*pop_m[4,1])/sum(pop_m[1:4,1])
prem_new[1,3]<-(mat1[1,3]*pop_m[1,1]+mat1[2,3]*pop_m[2,1]+mat1[3,3]*pop_m[3,1]+mat1[4,3]*pop_m[4,1])/sum(pop_m[1:4,1])

prem_new[2,1]<-(mat1[4,1]*pop_m[4,1]+mat1[5,1]*pop_m[5,1]+mat1[6,1]*pop_m[6,1]+mat1[7,1]*pop_m[7,1]+mat1[8,1]*pop_m[8,1]+mat1[9,1]*pop_m[9,1]+mat1[10,1]*pop_m[10,1]+mat1[11,1]*pop_m[11,1])/sum(pop_m[4:11,1])
prem_new[2,2]<-(mat1[4,2]*pop_m[4,1]+mat1[5,2]*pop_m[5,1]+mat1[6,2]*pop_m[6,1]+mat1[7,2]*pop_m[7,1]+mat1[8,2]*pop_m[8,1]+mat1[9,2]*pop_m[9,1]+mat1[10,2]*pop_m[10,1]+mat1[11,2]*pop_m[11,1])/sum(pop_m[4:11,1])
prem_new[2,3]<-(mat1[4,3]*pop_m[4,1]+mat1[5,3]*pop_m[5,1]+mat1[6,3]*pop_m[6,1]+mat1[7,3]*pop_m[7,1]+mat1[8,3]*pop_m[8,1]+mat1[9,3]*pop_m[9,1]+mat1[10,3]*pop_m[10,1]+mat1[11,3]*pop_m[11,1])/sum(pop_m[4:11,1])

prem_new[3,1]<-(mat1[12,1]*pop_m[12,1]+mat1[13,1]*pop_m[13,1]+mat1[14,1]*pop_m[14,1]+mat1[15,1]*pop_m[15,1]+mat1[16,1]*pop_m[16,1])/sum(pop_m[12:16,1])
prem_new[3,2]<-(mat1[12,2]*pop_m[12,1]+mat1[13,2]*pop_m[13,1]+mat1[14,2]*pop_m[14,1]+mat1[15,2]*pop_m[15,1]+mat1[16,2]*pop_m[16,1])/sum(pop_m[12:16,1])
prem_new[3,3]<-(mat1[12,3]*pop_m[12,1]+mat1[13,3]*pop_m[13,1]+mat1[14,3]*pop_m[14,1]+mat1[15,3]*pop_m[15,1]+mat1[16,3]*pop_m[16,1])/sum(pop_m[12:16,1])

prem_new
