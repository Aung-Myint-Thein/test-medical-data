### testing with association
## testing 1 : if trainData wardtype == "", logbill = log(281) RMSLE 1.52757
BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

trainData[, "logbillb"] <- apply(trainData, 1, function(row) ifelse(row["WARDTYPE"] == "", log(281), as.numeric(row["logbill"])))

for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(logbillb ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY +
                       Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                       Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                       Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, exp(prediction)-1)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}

submission4 <- submission4[, c(2:6,1,7:13, 73)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]
submission4["HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(as.numeric(row["HOSPITALBILL"]) < 0, 0, as.numeric(row["HOSPITALBILL"])))

#submission4[, "HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(row["WARDTYPE"] == "", 280, as.numeric(row["HOSPITALBILL"])))

write.csv(submission4, "submission17a_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)


####################### to test for  wardtype=="", another linear regression
## Submission 17b using logbill RMSLE : 1.75814
## Submission 17c using qutbill RMSLE : 1.76022

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

trainData.WithWT    <- trainData[trainData$WARDTYPE != "",]
trainData.WithoutWT <- trainData[trainData$WARDTYPE == "",]

predict.WithWT    <- predict[predict$WARDTYPE != "",]
predict.WithoutWT <- predict[predict$WARDTYPE == "",]


for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(logbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY +
                       Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                       Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                       Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData.WithWT[trainData.WithWT$AGEGROUP == i & trainData.WithWT$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict.WithWT[predict.WithWT$AGEGROUP == i & predict.WithWT$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, exp(prediction)-1)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}

for(i in 1:4){
  for(j in 1:4){
    #print(paste(i, j, sep=" "))
    #print(nrow(trainData.WithoutWT[trainData.WithoutWT$AGEGROUP == i & trainData.WithoutWT$BILLCAT == BILLCATCODE[j,2],]))
    
    if(i == 1 & j == 3){
      lmfunction <- lm(logbill ~ AGE + DURATIONOFSTAY + YEAROFADM +
                         Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                         Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                         Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                       , data=trainData.WithoutWT[trainData.WithoutWT$AGEGROUP == i & trainData.WithoutWT$BILLCAT == BILLCATCODE[j,2],])
      
      test_data <- predict.WithoutWT[predict.WithoutWT$AGEGROUP == i & predict.WithoutWT$BILLCAT == BILLCATCODE[j,2],]
      
      prediction <- predict(lmfunction, type="response", newdata=test_data)
      tes <- cbind(test_data, exp(prediction)-1)
      colnames(tes)[ncol(tes)] <- "prediction"
      
      submission4 <- rbind(submission4, tes)
    }else{
      lmfunction <- lm(logbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY +
                         Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                         Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                         Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                       , data=trainData.WithoutWT[trainData.WithoutWT$AGEGROUP == i & trainData.WithoutWT$BILLCAT == BILLCATCODE[j,2],])
      
      test_data <- predict.WithoutWT[predict.WithoutWT$AGEGROUP == i & predict.WithoutWT$BILLCAT == BILLCATCODE[j,2],]
      
      prediction <- predict(lmfunction, type="response", newdata=test_data)
      tes <- cbind(test_data, exp(prediction)-1)
      colnames(tes)[ncol(tes)] <- "prediction"
      
      submission4 <- rbind(submission4, tes)
    }
  }
}


submission4 <- submission4[, c(2:6,1,7:13, 73)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]
submission4["HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(as.numeric(row["HOSPITALBILL"]) < 0, 0, as.numeric(row["HOSPITALBILL"])))

#submission4[, "HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(row["WARDTYPE"] == "", 280, as.numeric(row["HOSPITALBILL"])))

write.csv(submission4, "submission17b_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)







