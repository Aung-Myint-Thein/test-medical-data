a <- rep(1:20)
a <- a/20

logitresult <- data.frame(threashold=a, hitrate=a, truepositiverate=a, truenegativerate=a)
colnames(logitresult) <- c("threashold", "Hit rate", "True positive rate", "True negative rate")

trainData[, "isZero"] <- apply(trainData, 1, function(row) ifelse(as.numeric(row["HOSPITALBILL"]) == 0, 1, 0))

## run the codes to get estimation and predict data

logreg_solution <- glm(isZero ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY + ItemNo +
                         Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                         Public.Specialist + Specialist + Surgery + Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P +
                         Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                       , family=binomial(link="logit"),  data=estimation_data)
test_Probability_class1_log <- predict(logreg_solution, type="response", newdata=predict)

test_prediction_class_log   <- 1*as.vector(test_Probability_class1_log >= 0.5)
submission4 <- cbind(submission4, test_prediction_class_log)
submission4[, "ZB"] <- apply(submission4, 1, function(row) ifelse(row["test_prediction_class_log"] == 1, 0, as.numeric(row["HOSPITALBILL"])))

submission <- submission4[, c(1:13, 16)]
colnames(submission)[14] <- "HOSPITALBILL"

write.csv(submission, "submission17h2_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)

for(i in 1:length(a)){
  
  test_prediction_class_log   <- 1*as.vector(test_Probability_class1_log > a[i])
  
  ## hit rate
  logitresult[i, 2] <- 100*sum(test_prediction_class_log==test_predict_data[, "isZero"])/length(test_predict_data[, "isZero"])
  
  ##
  #sum((!test_predict_data[, "ADM2013"]))/nrow(test_predict_data)
  
  ## confusion matrix
  conf_matrix = matrix(rep(0,4),ncol=2)
  conf_matrix[1,1]<- 100*sum(test_prediction_class_log*test_predict_data[, "isZero"])/sum(test_predict_data[, "isZero"])
  conf_matrix[1,2]<- 100*sum((!test_prediction_class_log)*test_predict_data[, "isZero"])/sum(test_predict_data[, "isZero"])
  conf_matrix[2,2]<- 100*sum((!test_prediction_class_log)*(!test_predict_data[, "isZero"]))/sum((!test_predict_data[, "isZero"]))
  conf_matrix[2,1]<- 100*sum((test_prediction_class_log)*(!test_predict_data[, "isZero"]))/sum((!test_predict_data[, "isZero"]))
  conf_matrix = round(conf_matrix,2)
  
  colnames(conf_matrix) <- c("Predicted 1", "Predicted 0")
  rownames(conf_matrix) <- c("Actual 1", "Actual 0")
  
  conf_matrix
  
  logitresult[i, 3] <- conf_matrix[1,1]
  logitresult[i, 4] <- conf_matrix[2,2]
}

test_data_long <- melt(logitresult, id="threashold")

ggplot(data=test_data_long,
       aes(x=threashold, y=value, colour=variable)) +
  geom_line() + ylab("Accuracy Performance (%)") + xlab("Probability Threshold") + labs(title = "Fig 3. Logistic regression performance") + theme_bw()


UID <- submission4[, "UID"]
final_submission[, "HOSPITALBILL"] <- apply(final_submission, 1, function(row) 
  ifelse(row["UID"] %in% UID, as.numeric(submission4[submission4$UID == row["UID"], "HOSPITALBILL"]), 0))

write.csv(final_submission[, c(1:14)], "submission_best3_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)




















################### Submission 15 Apr Version 2 

## testing with BILL TYPE and AGE GROUP total 16 group and adding WARDTYPE
## testing with hospitals2.csv will give 0.5728
## need to change to 9

BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
TYPEOFHOSP <- data.frame(TYPE=c(1:9), TYPEOFHOSP=unique(trainData$TYPEOFHOSP))
submission4 <- data.frame()

#trainData <- bills

trainData[, "logbill"] <- apply(trainData, 1, function(row) ifelse(row["WARDTYPE"] == "", log(170), as.numeric(row["logbill"])))

for(i in 1:4){
  for(j in 1:4){
    for(k in 1:9){
      
      trainDataforlm <- trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2] & trainData$TYPEOFHOSP == TYPEOFHOSP[k,2],]
      
      print(paste(i, j, k, nrow(trainDataforlm), length(unique(trainDataforlm$GENDER)), sep=" "))
      
      if(length(unique(trainDataforlm$GENDER)) < 2 | nrow(trainDataforlm) < 200){
        trainDataforlm <- trainData[trainData$AGEGROUP == i & trainData$BILLCAT == BILLCATCODE[j,2],]
        lmfunction <- lm(logbill ~ AGE +  GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY + DURATIONOFSTAY*AVEPERDAY*factor(isPrivate) + 
                           Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P 
                           #Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                         , data=trainDataforlm)
      }else{
        ## default function
        lmfunction <- lm(logbill ~ AGE +  GENDER + DURATIONOFSTAY + YEAROFADM + AGE*GENDER + GENDER*DURATIONOFSTAY + DURATIONOFSTAY*AVEPERDAY*factor(isPrivate) +
                           Ward. + Ward.A + Ward.B + Ward.C + Ward.D + Ward.E + Ward.F + Ward.G + Ward.H + Ward.I + Ward.K + Ward.M + Ward.N + Ward.O + Ward.P 
                           #Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                         , data=trainDataforlm)
      }
      
      test_data <- predict[predict$AGEGROUP == i & predict$BILLCAT == BILLCATCODE[j,2] & predict$TYPEOFHOSP == TYPEOFHOSP[k,2],]
      
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

print(summary(submission4$HOSPITALBILL))

hist(submission4$HOSPITALBILL)

write.csv(submission4, "submission_best4a_Aung_Myint_Thein.csv", quote=FALSE, row.names=FALSE)
