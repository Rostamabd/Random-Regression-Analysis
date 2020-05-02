### A function to standardize the time values and create the polynomial values
### Written by Rostam Abdollahi-Arpanahi, 2015

rm(list=ls())
options(digits=3, width=65)

#### Import data, this example test-dat data has been created by renumf90 program

Data <- read.table("renf90.dat",header=T)
colnames(Data) <- c("milk","htd","milk_time","DIM","AgeDam","subject","add_aniam",
"fat","pro","L1","L2","L3","L4","L5","L6")

dim(Data)

#### days in milk
Age <- sort(unique(Data[,4]))

###standardized time values (-1,1)
w <- -1+2*((Age-min(Age))/(max(Age)-min(Age )))

### polynomials of standardized time values
M <- matrix(c(w^0,w^1,w^2,w^3,w^4,w^5),ncol=6)
dim(M)


###compute the first six degree of legender polynomial
source("legendre_Coeff.R")

#### Phi matrix
Phi = matrix(data=c(0),nrow=length(w),ncol=7)
for(i in 1:length(w)){
  p = w[i]
  Phi[i,] = Legdre(p) }


Phi <- round(Phi,3)
dim(Phi)

### Save polynomial legender coefficient
write.table(Phi, file = "plyleg.txt",quote = F, sep = " ", row.names = F,
            col.names = T)

### read solutions derived by BLUPf90 family programs
Solutions <- read.table("solutions",header=T,row.names = NULL)

dim(Solutions)
head(Solutions)

### extract rows which are related to RR-coeff-BVs
BVCoeff <- Solutions[Solutions[,2]>=10,]

dim(BVCoeff)
head(BVCoeff)

#### choose animals IDs
animID <- unique(BVCoeff[,3])
length(animID)



BV_All_Day <- matrix(NA,nrow=length(animID),ncol=length(Age))
for(i in 1:length(animID)) {
	cat("animal",i,"\n")
	BV_All_Day[i,] <- Phi[,1:3]%*%(BVCoeff[BVCoeff[,3]==animID[i],4])
	
}

BV <- rowSums(BV_All_Day)

### Save BV results 
ID_RR_BV <- cbind(animID,BV)
colnames(ID_RR_BV) <- c("ID","BV")

 
### read original pedigree to find the original IDs
ped <- read.table("renadd06.ped",header=F)

colnames(Ped) <- c("ID","sire","dam","1","2","3","4","nprog","9","ID_Orig")

ID_Ped <- Ped[,c(1,10)]

### merge original ID and new IDs plus breeding values

all_GEBV <- merge(ID_Ped,ID_RR_BV,by="ID",all.x = TRUE)

### Save the final results
write.table(all_RRBV , file = "all_RR_BV.txt",quote = F, sep = " ", row.names = F,
            col.names = T)



