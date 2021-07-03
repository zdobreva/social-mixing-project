load("Data/contact_all.rdata")
country<-"CMR"
m <- data.matrix(contact_all[[country]])

#'Creating a 3 by 3 matrix to input contacts into
prem_new <- matrix(1:9, nrow = 3, ncol = 3)
colnames(prem_new) <- c("0-17", "18-55", "56-80")
rownames(prem_new) <- c("0-17", "18-55", "56-80")

#'Using Prem 2020 synthetic matrix for "CMR" Cameroon which has the following participant columns:
#'V1: 0-5 age V2: 6-10 age V3: 11-15 age V4: 16-20 age v5: 21-25 age etc.
#'I need age groups that cross these bounaries - 0-17, 18-55, 56+
#'To obtain contacts for the age group 0-17, I sum the contacts in each of the 0-5, 6-10, 11-15 and 16-20 columns across rows 1-3 (i.e. age 0 to 15)
#'Assuming that the each cell reports on the "average" number of contacts for each age group, 
#'the contacts for participants ages 16 and 17 should average the same as for the 16-20 group
#'To add in the additional contacts with ages 16-17, I add two fifths of the row 4 contacts to the respective age group
#'This is stored in vector, e.g. "a"
a <- c(sum(m[1:3,1],2/5*m[4,1]),sum(m[1:3,2],2/5*m[4,2]),sum(m[1:3,3],2/5*m[4,3]),sum(m[1:3,4],2/5*m[4,4]))

#'The ultimate input for the matrix is a weighted average of the mean contacts of 0-15 participants and 16-17 participants
prem_new[1,1] <- sum(15/17*mean(a[1],a[2],a[3]),2/17*a[4])

#'The same process is repeated for the other combinations of participant and contact age groups
#' participants 0-17 and contacts 18-55. Note: three fifths of row 4 are now added for 18-20 ages
b <- c(sum(m[5:11,1],3/5*m[4,1]),sum(m[5:11,2],3/5*m[4,2]),sum(m[5:11,3],3/5*m[4,3]),sum(m[5:11,4],3/5*m[4,4]))
prem_new[2,1] <- sum(15/17*mean(b[1],b[2],b[3]),2/17*b[4])

#' participants 0-17 and contacts 56+
d <- c(sum(m[12:16,1]),sum(m[12:16,2]),sum(m[12:16,3]),sum(m[12:16,4]))
prem_new[3,1] <- sum(15/17*mean(d[1:3]),2/17*d[4])

#'participants 18-55 and contacts 0-17
e<- c(sum(m[1:3,4],2/5*m[4,4]),sum(m[1:3,5],2/5*m[4,5]),sum(m[1:3,6],2/5*m[4,6]),sum(m[1:3,7],2/5*m[4,7]),sum(m[1:3,8],2/5*m[4,8]),sum(m[1:3,9],2/5*m[4,9]),sum(m[1:3,10],2/5*m[4,10]),sum(m[1:3,11],2/5*m[4,11]))
prem_new[1,2] <- sum(3/38*e[1],35/38*mean(e[2:8]))

#'participants 18-55 and contacts 18-55
f<- c(sum(m[5:11,4],3/5*m[4,4]),sum(m[5:11,5],3/5*m[4,5]),sum(m[5:11,6],3/5*m[4,6]),sum(m[5:11,7],3/5*m[4,7]),sum(m[5:11,8],3/5*m[4,8]),sum(m[5:11,9],3/5*m[4,9]),sum(m[5:11,10],3/5*m[4,10]),sum(m[5:11,11],3/5*m[4,11]))
prem_new[2,2] <- sum(3/38*f[1],35/38*mean(f[2:8]))

#'participants 18-55 and contacts 56+
g<- c(sum(m[12:16,4]),sum(m[12:16,5]),sum(m[12:16,6]),sum(m[12:16,7]),sum(m[12:16,8]),sum(m[12:16,9]),sum(m[12:16,10]),sum(m[12:16,11]))
prem_new[3,2] <- sum(3/38*g[1],35/38*mean(g[2:8]))

#'participants 56+ and contacts 0-17
h<- c(sum(m[1:3,12],2/5*m[4,12]),sum(m[1:3,13],2/5*m[4,13]),sum(m[1:3,14],2/5*m[4,14]),sum(m[1:3,15],2/5*m[4,15]),sum(m[1:3,16],2/5*m[4,16]))
prem_new[1,3] <- mean(h[1:5])

#'participants 56+ and contacts 18-55
i<- c(sum(m[5:11,12],3/5*m[4,12]),sum(m[5:11,13],3/5*m[4,13]),sum(m[5:11,14],3/5*m[4,14]),sum(m[5:11,15],3/5*m[4,15]),sum(m[5:11,16],3/5*m[4,16]))
prem_new[2,3] <- mean(i[1:5])

#'participants 56+ and contacts 56+
j<- c(sum(m[12:16,12]),sum(m[12:16,13]),sum(m[12:16,14]),sum(m[12:16,15]),sum(m[12:16,16]))
prem_new[3,3] <- mean(j)

prem_new
