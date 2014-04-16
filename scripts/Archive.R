######################################## Using bills and using only 2 types of hospital
## RMSLE 1.61527

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
bills[, "qutbill"] <- (bills[, "HOSPITALBILL"])^(1/4)
submission4 <- data.frame()

for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(qutbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM +
                       Private.Hospital + Public.Hospital + 
                       Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                       Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, (prediction)^4)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}
submission4 <- submission4[, c(2:6,1,7:13, 63)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]

write.csv(submission4, "submission52_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)


######################################## Using Bills data rather than TrainData
## RMSLE 1.70416

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()
trainData <- bills

## run the codes to get hosp type and ward type

for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(qutbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM +
                       Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                       Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                       Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, (prediction)^4)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}

submission4 <- submission4[, c(2:6,1,7:13, 73)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]

write.csv(submission4, "submission53_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)


## Testing 

##########################
BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(qutbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM +
                       Cancer.Specialist + ENT + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                       Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                       Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                     , data=trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, (prediction)^4)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission4 <- rbind(submission4, tes)
  }
}