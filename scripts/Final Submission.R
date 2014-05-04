rm(list=ls())
set.seed(123442323)
source("scripts/functions.R")

############# import the bills data

load("Rdata/bills.Rdata")

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
hospitals4 <- read.csv("hospitals4.csv")
bills <- merge(bills, hospitals, by="HOSPITAL", all.x=T, sort=F)

ICD9  <- read.csv("data/ICD9.csv", stringsAsFactors=F)
ICD10 <- read.csv("data/ICD10.csv", stringsAsFactors=F)
bills[, "DIAGNOSISGROUP"] <- apply(bills, 1, function(row) get.diagnosis.group(as.character(row["DIAGNOSISCODE"]), row["ICD9"], row["ICD10"], ICD9, ICD10))

diagroupcode <- data.frame(DIAGNOSISGROUPCODE=c(1:23), DIAGNOSISGROUP=unique(bills$DIAGNOSISGROUP))
bills <- merge(bills, diagroupcode, by=c("DIAGNOSISGROUP"), all.x=T, sort=F)

bills[, "qutbill"] <- (bills[, "HOSPITALBILL"])^(1/4)
bills[, "logbill"] <- (log(bills[, "HOSPITALBILL"]+1))

bills <- merge(bills, hospitals4, by="HOSPITAL", all.x=T, sort=FALSE)
bills[, "isPrivate"] <- apply(bills, 1, function(row) ifelse(row["TYPEOFHOSPB"] == "Private Hospital", 1, 0))

bills[, "AVEPERDAY"] <- apply(bills, 1, function(row) ifelse(as.numeric(row["isPrivate"]) == 1, 3075, 961.5))

############## end of bills data

############## import the predict data

load("Rdata/predict.Rdata")

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

predict <- merge(predict, hospitals4, by="HOSPITAL", all.x=T)
predict[, "isPrivate"] <- apply(predict, 1, function(row) ifelse(row["TYPEOFHOSPB"] == "Private Hospital", 1, 0))

predict[, "AVEPERDAY"] <- apply(predict, 1, function(row) ifelse(as.numeric(row["isPrivate"]) == 1, 3075, 961.5))
############## end of predict data

############## Preparation for classification exercise
IDS <- unique(predict$ID)
bills[, "ADM2013"] <- apply(bills, 1, function(row) ifelse(as.character(row["ID"]) %in% IDS, 1, 0))
rm(IDS)
############## End of Preparation for classification exercise

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

trainData <- bills

trainData[, "logbillb"] <- apply(trainData, 1, function(row) ifelse(row["WARDTYPE"] == "", log(170), as.numeric(row["logbill"])))

trainData[, "Cancer.Specialist"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Cancer Specialist", 1, 0)
trainData[, "Community.Hospital"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Community Hospital", 1, 0)
trainData[, "Dental"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Dental", 1, 0)
trainData[, "ENT"] <- ifelse(trainData[, "TYPEOFHOSP"] == "ENT", 1, 0)
trainData[, "Eye"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Eye", 1, 0)
trainData[, "Kidney"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Kidney", 1, 0)
trainData[, "Others"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Others", 1, 0)
trainData[, "Oversea.Hospital"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Oversea Hospital", 1, 0)
trainData[, "Private.Hospital"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Private Hospital", 1, 0)
trainData[, "Public.Hospital"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Public Hospital", 1, 0)
trainData[, "Public.Specialist"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Public Specialist", 1, 0)
trainData[, "Specialist"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Specialist", 1, 0)
trainData[, "Surgery"] <- ifelse(trainData[, "TYPEOFHOSP"] == "Surgery", 1, 0)

for(i in 1:nrow(diagroupcode)){
  variable.name <- gsub(" ", ".", as.character(diagroupcode[i,2]))
  variable.name <- gsub(",", "", variable.name)
  variable.name <- gsub("-", "", variable.name)
  trainData[, variable.name] <- ifelse(trainData[, "DIAGNOSISGROUP"] == as.character(diagroupcode[i,2]), 1, 0)
}

for(i in 1:nrow(wardtypes)){
  variable.name <- paste("Ward.", as.character(wardtypes[i,2]), sep="")
  trainData[, variable.name] <- ifelse(trainData[, "WARDTYPE"] == as.character(wardtypes[i,2]), 1, 0)
}


for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(logbillb ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY + DURATIONOFSTAY*AVEPERDAY +
                       Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                       Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P 
                     #Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, exp(prediction)-1)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}

submission4 <- submission4[, c(2:6,1,7:13, 77)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]
submission4["HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(as.numeric(row["HOSPITALBILL"]) < 0, 0, as.numeric(row["HOSPITALBILL"])))
