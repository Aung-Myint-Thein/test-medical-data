## first submission 30 Mar 2014
trainData[, "qutbill"] <- trainData[, "HOSPITALBILL"]^(1/4)
lm10 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=trainData)
pre <- predict
pre[, "DIAGNOSISGROUP"] <- apply(pre, 1, function(row) ifelse(row["DIAGNOSISGROUP"] %in% as.character(unique(trainData$DIAGNOSISGROUP)), row["DIAGNOSISGROUP"], "Others" ))
prediction <- predict(lm10, type="response", newdata=pre[, c("AGEGROUP", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
hist(prediction)
hist((prediction)^4)
tes <- cbind(pre, (prediction)^4)
submission1 <- tes[, c(2,3,4,5,1,6,7,8:12,22)]
colnames(submission1)[13] <- "HOSPITALBILL"


## second submission 2 Apr 2014
## testing with age group
pre <- predict

lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 1,])
test_data1 <- pre[pre$AGEGROUP == 1,]
test_data1[, "DIAGNOSISGROUP"] <- apply(test_data1, 1, function(row) ifelse(row["DIAGNOSISGROUP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 4,"DIAGNOSISGROUP"])), row["DIAGNOSISGROUP"], "Others" ))
prediction1 <- predict(lm1, type="response", newdata=test_data1[, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes1 <- cbind(pre[pre$AGEGROUP == 1, ], (prediction1)^4)
colnames(tes1)[ncol(tes1)] <- "prediction"

lm2 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 2,])
test_data2 <- pre[pre$AGEGROUP == 2,]
test_data2[, "TYPEOFHOSP"] <- apply(test_data2, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 2,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
prediction2 <- predict(lm2, type="response", newdata=test_data2[test_data2$AGEGROUP == 2, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes2 <- cbind(test_data2, (prediction2)^4)
colnames(tes2)[ncol(tes2)] <- "prediction"

lm3 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 3,])
prediction3 <- predict(lm3, type="response", newdata=pre[pre$AGEGROUP == 3, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes3 <- cbind(pre[pre$AGEGROUP == 3, ], (prediction3)^4)
colnames(tes3)[ncol(tes3)] <- "prediction"

lm4 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 4,])
test_data4 <- pre[pre$AGEGROUP == 4,]
test_data4[, "TYPEOFHOSP"] <- apply(test_data4, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 4,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
test_data4[, "DIAGNOSISGROUP"] <- apply(test_data4, 1, function(row) ifelse(row["DIAGNOSISGROUP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 4,"DIAGNOSISGROUP"])), row["DIAGNOSISGROUP"], "Others" ))
prediction4 <- predict(lm4, type="response", newdata=test_data4[, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes4 <- cbind(test_data4, (prediction4)^4)
colnames(tes4)[ncol(tes4)] <- "prediction"

tesmain <- rbind(tes1, tes2, tes3, tes4)

#submission2 <- tesmain[, c(2,3,4,5,1,6:12, 22)]
submission2 <- tesmain[, c(2,3,4,5,1,6:12, 57)]
colnames(submission2)[13] <- "HOSPITALBILL"

write.csv(submission2, "submission2_Aung_Myint_Thein.csv")

## third submission 4 Apr 2014
## testing with BILL TYPE and AGE GROUP total 16 group

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission3 <- data.frame()

for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(qutbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM +
                Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                Public.Specialist + Specialist + Surgery +
                Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
              , data=trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],])
    
    test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2],]
    
    prediction <- predict(lmfunction, type="response", newdata=test_data)
    tes <- cbind(test_data, (prediction)^4)
    colnames(tes)[ncol(tes)] <- "prediction"
    
    submission3 <- rbind(submission3, tes)
  }
}

submission3 <- submission3[, c(2:6,1,7:13, 58)]
colnames(submission3)[ncol(submission3)] <- "HOSPITALBILL"
submission3 <- submission3[order(submission3$UID),]

write.csv(submission3, "submission3_Aung_Myint_Thein.csv", row.names=FALSE)



################### Submission 15 Apr 1.59033

## testing with BILL TYPE and AGE GROUP total 16 group and adding WARDTYPE

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

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

write.csv(submission4, "submission4_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)

################### Submission 15 Apr Version 2 

## testing with BILL TYPE and AGE GROUP total 16 group and adding WARDTYPE
## testing with hospitals2.csv will give 0.5728
## need to change to 9

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
TYPEOFHOSP <- data.frame(TYPE=c(1:9), TYPEOFHOSP=unique(trainData$TYPEOFHOSP))
submission4 <- data.frame()

for(i in 1:4){
  for(j in 1:4){
    for(k in 1:9){
      
      trainDataforlm <- trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2] & trainData$TYPEOFHOSP == TYPEOFHOSP[k,2],]
      
      print(paste(i, j, k, nrow(trainDataforlm), sep=" "))
      
      lmfunction <- lm(qutbill ~ AGE +  GENDER + DURATIONOFSTAY + YEAROFADM +
                         Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                         Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                       , data=trainDataforlm)
      
      test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2] & predict$TYPEOFHOSP == TYPEOFHOSP[k,2],]
      
      prediction <- predict(lmfunction, type="response", newdata=test_data)
      tes <- cbind(test_data, (prediction)^4)
      colnames(tes)[ncol(tes)] <- "prediction"
      
      submission4 <- rbind(submission4, tes)
    }
  }
}

submission4 <- submission4[, c(2:6,1,7:13, 69)]
colnames(submission4)[ncol(submission4)] <- "HOSPITALBILL"
submission4 <- submission4[order(submission4$UID),]

write.csv(submission4, "submission5_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)

######################################## Submission 16 Apr 2014
## for wardtype blank, if I assign 280, I get 1.58006
## for wardtype blank, if I assign 0, I get 1.95017

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

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

submission4[, "HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(row["WARDTYPE"] == "", 280, as.numeric(row["HOSPITALBILL"])))

write.csv(submission4, "submission16b_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)


### testing with association
## without defeault 280 1.58929
## with default 280 1.57557
## with new trainData and 280 1.56895
BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
submission4 <- data.frame()

for(i in 1:4){
  for(j in 1:4){
    lmfunction <- lm(qutbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY +
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

submission4[, "HOSPITALBILL"] <- apply(submission4, 1, function(row) ifelse(row["WARDTYPE"] == "", 280, as.numeric(row["HOSPITALBILL"])))

write.csv(submission4, "submission16d_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)

