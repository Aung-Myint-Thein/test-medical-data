### testing with association
## testing 1 : if wardtype == "", logbill = log(170) but will use bills data. RMSLE 1.39841
## testing 1 : bills data. RMSLE    1.67038
## testing 2 : if wardtype == "", logbill = log(170) but will use bills data and itemNo as new variable. RMSLE 1.61055
## testing 2 : bills data and itemNo as new variable. RMSLE 1.61055

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

trainData <- bills

trainData[, "logbillb"] <- apply(trainData, 1, function(row) ifelse(row["WARDTYPE"] == "", log(170), as.numeric(row["logbill"])))

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

#write.csv(submission4, "Final_Submission_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)


############# testing for hosp type and bill cat
## test 1 : using logbill and bills data and AGEGROUP + TYPEOFHOSPB RMSLE : 1.87898
## test 2 : using logbillb and bills data and AGEGROUP + TYPEOFHOSPB RMSLE : 1.69048
## test 3 : using logbillb and bills data and BILLCAT + TYPEOFHOSPB RMSLE : 1.62366
## test 3 : using logbill and bills data and BILLCAT + TYPEOFHOSPB RMSLE : 1.80316

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

trainData <- bills

trainData[, "logbillb"] <- apply(trainData, 1, function(row) ifelse(row["WARDTYPE"] == "", log(170), as.numeric(row["logbill"])))

for(i in 1:4){
  for(j in 0:1){
    print(paste(i, j, sep=" "))
    lmfunction <- lm(logbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY + ItemNo +
                       Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                       Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                       Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData[trainData$BILLCAT == BILLCATCODE[i,2] & trainData$isPrivate == j,])
    
    test_data <- predict[predict$BILLCAT == BILLCATCODE[i,2] & predict$isPrivate == j,]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, exp(prediction)-1)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}

submission4 <- submission4[, c(2:6,1,7:13, 76)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]
submission4["HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(as.numeric(row["HOSPITALBILL"]) < 0, 0, as.numeric(row["HOSPITALBILL"])))

#submission4[, "HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(row["WARDTYPE"] == "", 280, as.numeric(row["HOSPITALBILL"])))

write.csv(submission4, "submission18g_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)
