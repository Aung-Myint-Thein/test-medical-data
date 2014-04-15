rm(list=ls())
set.seed(123442323)
source("scripts/functions.R")

############# import the bills data
bills <- read.csv("data/1 Core Data/Hospital Bills Final Set v2.csv")
bills <- bills[!is.na(bills$HOSPITALBILL), ]
bills <- bills[!bills$ID=="C080032649",]
bills[, "AGE"] <- 2014 - bills[, "YMDOB"]
bills[, "AGEGROUP"] <- apply(bills, 1, function(row) get.age.group(as.numeric(row["AGE"])))
bills[, "DURATIONOFSTAY"] <- as.numeric(difftime(as.Date(bills[, "DATEDISCHARGE"], "%d/%m/%Y"), as.Date(bills[, "DATEOFADM"], "%d/%m/%Y"), units="days"))
bills[, "DURATIONOFSTAY"] <- ifelse(is.na(bills[, "DURATIONOFSTAY"]), 0, bills[, "DURATIONOFSTAY"])
bills[, "YEAROFADM"] <- apply(bills, 1, function(row) as.numeric(format(as.Date(row["DATEOFADM"], "%d/%m/%Y"), "%Y")))

## getting if the diagnosis code are in ICD 9 or ICD 10
bills[, "cleaned"] <- clean.diagnosus(as.character(bills[, "DIAGNOSUS"]))
bills[, "ICD9"] <- apply(bills, 1, function(row) length(grep("^[[:upper:]]+$", row["cleaned"], value=FALSE)))
bills[, "ICD10"] <- apply(bills, 1, function(row) ifelse(row["ICD9"]==1, 0, 1))
bills <- bills[, !names(bills) %in% c("cleaned")]

## sort by ID and HRN
bills <- bills[order(bills$ID, bills$HRN),]

hospitals <- read.csv("hospitals.csv")
bills <- merge(bills, hospitals, by="HOSPITAL", all.x=T, sort=F)

ICD9  <- read.csv("data/ICD9.csv", stringsAsFactors=F)
ICD10 <- read.csv("data/ICD10.csv", stringsAsFactors=F)
bills[, "DIAGNOSISGROUP"] <- apply(bills, 1, function(row) get.diagnosis.group(as.character(row["DIAGNOSISCODE"]), row["ICD9"], row["ICD10"], ICD9, ICD10))

diagroupcode <- data.frame(DIAGNOSISGROUPCODE=c(1:23), DIAGNOSISGROUP=unique(bills$DIAGNOSISGROUP))
bills <- merge(bills, diagroupcode, by=c("DIAGNOSISGROUP"), all.x=T, sort=F)

############## end of bills data

############## import the predict data
predict <- read.csv("data/1 Core Data/201404105314-Prudential_Submissions_v3.csv")
#predict[, "AGE"] <- 2014 - as.numeric(substr(as.character(predict[, "YMDOB"]), 1, 4))
predict[, "AGE"] <- 2014 - predict[, "YMDOB"]
predict[, "AGEGROUP"] <- apply(predict, 1, function(row) get.age.group(as.numeric(row["AGE"])))
predict[, "DURATIONOFSTAY"] <- as.numeric(difftime(as.Date(predict[, "DATEDISCHARGE"], "%d/%m/%Y"), as.Date(predict[, "DATEOFADM"], "%d/%m/%Y"), units="days"))
predict[, "YEAROFADM"] <- apply(predict, 1, function(row) as.numeric(format(as.Date(row["DATEOFADM"], "%d/%m/%Y"), "%Y")))

## getting if the diagnosis code are in ICD 9 or ICD 10
predict[, "cleaned"] <- clean.diagnosus(as.character(predict[, "DIAGNOSUS"]))
predict[, "ICD9"] <- apply(predict, 1, function(row) length(grep("^[[:upper:]]+$", row["cleaned"], value=FALSE)))
predict[, "ICD10"] <- apply(predict, 1, function(row) ifelse(row["ICD9"]==1, 0, 1))
predict <- predict[, !names(predict) %in% c("cleaned")]

## sort by ID and HRN
predict <- predict[order(predict$ID, predict$HRN),]

predict <- merge(predict, hospitals, by="HOSPITAL", all.x=T, sort=FALSE)
predict[, "DIAGNOSISGROUP"] <- apply(predict, 1, function(row) get.diagnosis.group(as.character(row["DIAGNOSISCODE"]), row["ICD9"], row["ICD10"], ICD9, ICD10))

for(i in 1:nrow(diagroupcode)){
  variable.name <- gsub(" ", ".", as.character(diagroupcode[i,2]))
  variable.name <- gsub(",", "", variable.name)
  variable.name <- gsub("-", "", variable.name)
  predict[, variable.name] <- ifelse(predict[, "DIAGNOSISGROUP"] == as.character(diagroupcode[i,2]), 1, 0)
}

typeofhospcode <- data.frame(typeofhospcode=c(1:13), TYPEOFHOSP=unique(hospitals$TYPEOFHOSP))

for(i in 1:nrow(typeofhospcode)){
  variable.name <- gsub(" ", ".", as.character(typeofhospcode[i,2]))
  variable.name <- gsub(",", "", variable.name)
  variable.name <- gsub("-", "", variable.name)
  predict[, variable.name] <- ifelse(predict[, "TYPEOFHOSP"] == as.character(typeofhospcode[i,2]), 1, 0)
}

wardtypes <- data.frame(no=c(1:15),WARDTYPE=union(sort(unique(bills$WARDTYPE)), sort(unique(predict$WARDTYPE))))

for(i in 1:nrow(wardtypes)){
  variable.name <- paste("Ward.", as.character(wardtypes[i,2]), sep="")
  predict[, variable.name] <- ifelse(predict[, "WARDTYPE"] == as.character(wardtypes[i,2]), 1, 0)
}

############## end of predict data

############## Preparation for classification exercise
IDS <- unique(predict$ID)
bills[, "ADM2013"] <- apply(bills, 1, function(row) ifelse(as.character(row["ID"]) %in% IDS, 1, 0))
rm(IDS)
############## End of Preparation for classification exercise


WardType <- read.csv("data/1 Core Data/Reference - WardType.csv")

trainDataDiagnosis <- tolower(unique(bills$DIAGNOSUS))
predictDataDiagnosis <- tolower(unique(predict$DIAGNOSUS))

setdiff(predictDataDiagnosis, trainDataDiagnosis)
setdiff(trainDataDiagnosis, predictDataDiagnosis)

## exploraton for age
summary(bills$AGE)
boxplot(bills$AGE)
barplot(table(bills$AGE))

hist(bills[bills$HOSPITALBILL < 2000, "HOSPITALBILL"])

hist(bills[bills$HOSPITALBILL < 20, "HOSPITALBILL"])

hist(bills[bills$HOSPITALBILL > 500 & bills$HOSPITALBILL < 2000, "HOSPITALBILL"])

table(bills[bills$HOSPITALBILL < 20, "HOSPITALBILL"])

head(bills[bills$HOSPITALBILL == 0, ])

bills <- bills[order(bills$ID, bills$yyyy, bills$DATEOFEVENT, bills$HOSPITALBILL, decreasing=F),]

length(unique(predict$ID))
length(unique(bills$ID))
length(intersect(unique(predict$ID), unique(bills$ID)))
length(intersect(unique(bills$ID), unique(predict$ID)))
length(setdiff(unique(bills$ID), unique(predict$ID)))
length(setdiff(unique(predict$ID), unique(bills$ID)))


head(bills, 30)

big.ID <- bills[bills$ID=="C000003554",]
sum(big.ID$HOSPITALBILL)

length(unique(big.ID$HRN))

aggregate(HOSPITALBILL ~ HRN , big.ID, sum)

bills2 <- aggregate(HOSPITALBILL ~ HRN , bills, sum)
bills2 <- bills2[order(bills2$HRN),]

bills3 <- aggregate(HOSPITALBILL ~ ID + HRN , bills, sum)
bills3 <- bills3[order(bills3$HRN),]

bills4 <- aggregate(HOSPITALBILL ~ ID + HRN + GENDER + BILLCAT + DIAGNOSUS + AGE , bills, sum)
bills4 <- bills4[order(bills4$HRN),]

bills5 <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSUS + BILLCAT , bills, sum)
bills5 <- bills5[order(bills5$HRN),]

tb <- bills[1:12, ]

tb2 <- aggregate(HOSPITALBILL ~ ID + HRN , tb, sum)
tb3 <- aggregate(HOSPITALBILL ~ ID + HRN + GENDER + BILLCAT + DIAGNOSUS + AGE , tb, sum)
tb4 <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSUS + BILLCAT , tb, sum)

for(i in 2:nrow(bills4)){
  if(bills4[i-1, 2] == uni2[i, 2]){
    print(uni2[i,])
  }
}

bills2[table(bills3[,2]) > 1,]

ndates1 <- difftime(as.Date(bills[, "DATEDISCHARGE"], "%d/%m/%Y"),as.Date(bills[, "DATEOFADM"], "%d/%m/%Y"), units="auto")
head(ndates1)
ndates1[1]- ndates1[2]

plot(table(bills$DURATIONOFSTAY))

plot(table(predict$DURATIONOFSTAY))

plot(table(bills$AGE))

plot(table(predict$AGE))


## order the data
trainD <- bills[order(bills$ID, bills$yyyy, bills$HRN, bills$DATEOFADM, bills$DATEDISCHARGE, bills$BILLCAT, bills$WARDTYPE),]

onlyBILLCAT  <- bills[bills[,"BILLCAT"] != "", c(1,4,10,11)]
onlyWARDTYPE <- bills[bills[,"WARDTYPE"] != "", c(1,4,10,11)]
bothBILLCATWARDTYPE <- bills[bills[,"BILLCAT"] != "" & bills[,"WARDTYPE"] != "", c(1,4,10,11)]
neiBILLCATWARDTYPE  <- bills[bills[,"BILLCAT"] == "" & bills[,"WARDTYPE"] == "", c(1,4,10,11)]

unique <- union(union(union(onlyWARDTYPE, bothBILLCATWARDTYPE), onlyBILLCAT), neiBILLCATWARDTYPE)

## starting to test the regression

trainData <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSUS + BILLCAT + DURATIONOFSTAY, bills, mean)
trainData <- trainData[order(trainData$HRN),]
trainData[, "inpatient"] <- ifelse(trainData[, "BILLCAT"] == "IN", 1, 0)
trainData[, "outpatient"] <- ifelse(trainData[, "BILLCAT"] == "OU", 1, 0)
trainData[, "surgpatient"] <- ifelse(trainData[, "BILLCAT"] == "DY", 1, 0)

estimation_data_ids <- sample.int(nrow(trainData), 5000)
estimation_data <- trainData[estimation_data_ids, ]

non_estimation_data <- setdiff(1:nrow(trainData),estimation_data_ids)
test_data <- trainData[non_estimation_data, ]


#estimation_data[, "logbill"] <- ifelse(estimation_data[, "HOSPITALBILL"] == 0, 0, log(estimation_data[, "HOSPITALBILL"]))
estimation_data[, "sqrtbill"] <- sqrt(estimation_data[, "HOSPITALBILL"])
estimation_data[, "qutbill"] <- estimation_data[, "HOSPITALBILL"]^(1/2)
hist(estimation_data[, "HOSPITALBILL"])
hist(estimation_data[, "logbill"])
hist(estimation_data[, "qutbill"])

#lm1 <- lm(qutbill ~ AGE + GENDER + BILLCAT + DURATIONOFSTAY, data=estimation_data)
lm1 <- lm(qutbill ~ AGE + GENDER + BILLCAT + DURATIONOFSTAY + inpatient + outpatient + surgpatient, data=estimation_data)

#family=poisson()

prediction <- predict(lm1, type="response", newdata=test_data[1:4000, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "inpatient", "outpatient", "surgpatient")])
hist(prediction)
hist((prediction)^2)

tes <- cbind(test_data[1:4000,], (prediction)^2)
#library(Metrics)

rmsle(tes$HOSPITALBILL, tes[,"(prediction)^2"])

sort(setdiff(unique(bills$HOSPITAL), unique(predict$HOSPITAL)))
sort(setdiff(unique(predict$HOSPITAL), unique(bills$HOSPITAL)))

## getting all the hospital mean bill
hospiData <- aggregate(HOSPITALBILL ~ HOSPITAL + WARDTYPE + BILLCAT, bills, mean)
hospiData <- hospiData[order(hospiData$HOSPITAL),]

hospiData[hospiData$HOSPITALBILL==0,]

qplot(factor(AGEGROUP), HOSPITALBILL, data=bills, geom=c("boxplot"),
      fill=factor(AGEGROUP), xlab="", ylab="Bill") 

## according to age group, we can see how the trend of the bills 
LVGK <- ggplot(data=trainData[trainData$HOSPITALBILL<50000,], aes(x=AGE, y=qutbill))
LVGK + geom_point(shape = 1, alpha = .8) + facet_grid(.~ AGEGROUP) + geom_smooth(method="lm")

LVGK + geom_point(shape = 1, alpha = .8) + facet_grid(.~ code) + geom_smooth(method="lm")

LVGK + geom_point(shape = 1, alpha = .8) + facet_grid(.~ BILLCAT) + geom_smooth(method="lm")

ggplot(trainData[trainData$HOSPITALBILL<50000,], aes(x=factor(DIAGNOSISGROUPCODE), y=qutbill)) + geom_boxplot()

ggplot(trainData[trainData$HOSPITALBILL<50000,], aes(x=factor(BILLCAT), y=qutbill)) + geom_boxplot()


p <- ggplot(data=trainData[trainData$HOSPITALBILL<50000,], aes(x=AGE, y=qutbill, group=HRN))
p + geom_line()

## this plot can be used for report
p <- ggplot(data=trainData[trainData$HOSPITALBILL<50000,], aes(factor(BILLCAT), qutbill))
p + geom_boxplot(aes(fill = factor(AGEGROUP)))

## H cluster to see how many clusters should we see
Hierarchical_Cluster_distances <- dist(trainData[, c(3:5,7:9,14)], method="euclidean")
Hierarchical_Cluster <- hclust(Hierarchical_Cluster_distances, method="ward")
# Display dendogram
plot(Hierarchical_Cluster, main = NULL, sub=NULL, labels = 1:nrow(trainData), xlab="Our Observations", cex.lab=1, cex.axis=1) 
# Draw dendogram with red borders around the 3 clusters
rect.hclust(Hierarchical_Cluster, k=6, border="red") 

kmeans_clusters <- kmeans(trainData[, c(3:5,8:9,14)],centers=6, iter.max=1000, algorithm="Lloyd")

cluster_memberships_kmeans <- kmeans_clusters$cluster 
cluster_ids_kmeans <- unique(cluster_memberships_kmeans)



Hierarchical_Cluster_distances <- dist(trainData[, c(5)], method="euclidean")
Hierarchical_Cluster <- hclust(Hierarchical_Cluster_distances, method="ward")
# Display dendogram
plot(Hierarchical_Cluster, main = NULL, sub=NULL, labels = 1:nrow(trainData), xlab="Our Observations", cex.lab=1, cex.axis=1) 


testtt <- bills

