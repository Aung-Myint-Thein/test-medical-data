trainData <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSISCODE  + BILLCAT + DURATIONOFSTAY + TYPEOFHOSP , bills, mean)
trainData <- trainData[order(trainData$HRN),]

trainData <- merge(trainData, unique(bills[, c("ID", "HRN", "DIAGNOSISGROUP", "AGEGROUP", "DATEOFADM", "DIAGNOSISGROUPCODE", "ADM2013")]), by=c("ID", "HRN"), sort=F, all.x=T)
trainData[, "YEAROFADM"] <- apply(trainData, 1, function(row) as.numeric(format(as.Date(row["DATEOFADM"], "%d/%m/%Y"), "%Y")))
trainData[, "qutbill"] <- trainData[, "HOSPITALBILL"]^(1/4)
trainData[, "sqrtbill"]<- trainData[, "HOSPITALBILL"]^(1/2)

#diagroupcode <- data.frame(DIAGNOSISGROUPCODE=c(1:23), DIAGNOSISGROUP=unique(trainData$DIAGNOSISGROUP))
#trainData <- merge(trainData, diagroupcode, by=c("DIAGNOSISGROUP"), all.x=T, sort=F)

trainData[, "DIAGNOSISGROUPCLUSTER"] <- apply(trainData, 1, function(row) ifelse(row["DIAGNOSISGROUPCODE"] %in% c(1,2,5,7,16,17,21), "A", ifelse(row["DIAGNOSISGROUPCODE"] %in% c(4,8,11,12,13,14,18,19,23), "B", "C")))

toscale <- trainData[, c("DURATIONOFSTAY", "YEAROFADM")]
toscale_scaled <- apply(toscale,2, function(r) {if (sd(r)!=0) res=(r-mean(r))/sd(r) else res=0*r; res})
colnames(toscale_scaled) <- c("DURATIONOFSTAYB", "YEAROFADMB")
trainData <- cbind(trainData, toscale_scaled)

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

## for formula
string <- ""
for(i in 1:nrow(diagroupcode)){
  code <- gsub(" ", ".", as.character(diagroupcode[i, 2]))
  code <- gsub(",", "", code)
  code <- gsub("-", "", code)
  string <- paste(string, "+", code , sep=" ")
}

ggplot(trainData[trainData$HOSPITALBILL<50000,], aes(x=factor(DIAGNOSISGROUPCODE), y=qutbill)) + geom_boxplot()

set.seed(123442323)

estimation_data_ids <- sample.int(nrow(trainData), 12000)
estimation_data <- trainData[estimation_data_ids, ]

non_estimation_data <- setdiff(1:nrow(trainData),estimation_data_ids)
test_data <- trainData[non_estimation_data, ]

test_data_ids <- sample(rownames(test_data), 2000)
test_predict_data <- test_data[test_data_ids, ]

#iteration 1
lm1 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data)
prediction <- predict(lm1, type="response", newdata=test_predict_data[, c("AGEGROUP", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes <- cbind(test_predict_data, (prediction)^4)
rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"]) # 1.102479

## testing with normalized duration and year of adm but it has the same score as unnormalized
lm1 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + factor(BILLCAT) + DURATIONOFSTAYB + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADMB , data=estimation_data)
prediction <- predict(lm1, type="response", newdata=test_predict_data[, c("AGEGROUP", "GENDER", "BILLCAT", "DURATIONOFSTAYB", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADMB")])
tes <- cbind(test_predict_data, (prediction)^4)
rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"]) # 1.102479

lm1.11 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(DIAGNOSISGROUP) + YEAROFADM +
            Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
            Public.Specialist + Specialist + Surgery, data=estimation_data)
prediction <- predict(lm1.11, type="response", newdata=test_predict_data)
tes <- cbind(test_predict_data, (prediction)^4)
rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"]) # 1.102479

## iteration 3.1 
rmsle(tes[tes$BILLCAT == "DY", "HOSPITALBILL"], tes[tes$BILLCAT == "DY","(prediction)^4"]) ## 1.033707

lm3.1.1 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data[estimation_data$BILLCAT=="DY",])
prediction <- predict(lm3.1.1, type="response", newdata=test_predict_data[test_predict_data$BILLCAT=="DY", c("AGEGROUP", "GENDER", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes3.1.1 <- cbind(test_predict_data[test_predict_data$BILLCAT=="DY",], (prediction)^4)
rmsle(tes3.1.1$HOSPITALBILL, tes3.1.1[,"(prediction)^4"]) ## 0.9803987

lm3.1.2 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data[estimation_data$BILLCAT=="OU",])
test_data2 <- test_predict_data[test_predict_data$BILLCAT == "OU",]
test_data2[, "DIAGNOSISGROUP"] <- apply(test_data2, 1, function(row) ifelse(row["DIAGNOSISGROUP"] %in% as.character(unique(estimation_data[estimation_data$BILLCAT == "OU","DIAGNOSISGROUP"])), row["DIAGNOSISGROUP"], "Others" ))
prediction <- predict(lm3.1.2, type="response", newdata=test_data2)
tes3.1.2 <- cbind(test_predict_data[test_predict_data$BILLCAT=="OU",], (prediction)^4)
rmsle(tes3.1.2$HOSPITALBILL, tes3.1.2[,"(prediction)^4"]) ## 1.19964

lm3.1.3 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data[estimation_data$BILLCAT=="IN",])
prediction <- predict(lm3.1.3, type="response", newdata=test_predict_data[test_predict_data$BILLCAT=="IN", c("AGEGROUP", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes3.1.3 <- cbind(test_predict_data[test_predict_data$BILLCAT=="IN",], (prediction)^4)
rmsle(tes3.1.3$HOSPITALBILL, tes3.1.3[,"(prediction)^4"]) ## 0.9876847

lm3.1.4 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data[estimation_data$BILLCAT=="",])
prediction <- predict(lm3.1.4, type="response", newdata=test_predict_data[test_predict_data$BILLCAT=="", c("AGEGROUP", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes3.1.4 <- cbind(test_predict_data[test_predict_data$BILLCAT=="",], (prediction)^4)
rmsle(tes3.1.4$HOSPITALBILL, tes3.1.4[,"(prediction)^4"]) ## 1.142522

tesmain3.1 <- rbind(tes3.1.1, tes3.1.2, tes3.1.3, tes3.1.4)
rmsle(tesmain3.1$HOSPITALBILL, tesmain3.1[,"(prediction)^4"]) ## 1.042781 0.9972215

## iteration 2 
lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data)
prediction <- predict(lm1, type="response", newdata=test_predict_data[, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes <- cbind(test_predict_data, (prediction)^4)
rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"]) ##1.117599


## testing with age group
lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 1,])
prediction1 <- predict(lm1, type="response", newdata=test_predict_data[test_predict_data$AGEGROUP == 1, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes1 <- cbind(test_predict_data[test_predict_data$AGEGROUP == 1, ], (prediction1)^4)
colnames(tes1)[ncol(tes1)] <- "prediction"
#rmsle(tes1$HOSPITALBILL, tes1[,"prediction"])

lm2 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 2,])
test_data2 <- test_predict_data[test_predict_data$AGEGROUP == 2,]
test_data2[, "TYPEOFHOSP"] <- apply(test_data2, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 2,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
test_data2[, "DIAGNOSISGROUP"] <- apply(test_data2, 1, function(row) ifelse(row["DIAGNOSISGROUP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 2,"DIAGNOSISGROUP"])), row["DIAGNOSISGROUP"], "Others" ))
prediction2 <- predict(lm2, type="response", newdata=test_data2[test_data2$AGEGROUP == 2, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes2 <- cbind(test_data2[test_data2$AGEGROUP == 2, ], (prediction2)^4)
colnames(tes2)[ncol(tes2)] <- "prediction"
#rmsle(tes2$HOSPITALBILL, tes2[,"prediction"])

lm3 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 3,])
prediction3 <- predict(lm3, type="response", newdata=test_predict_data[test_predict_data$AGEGROUP == 3, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes3 <- cbind(test_predict_data[test_predict_data$AGEGROUP == 3, ], (prediction3)^4)
colnames(tes3)[ncol(tes3)] <- "prediction"
#rmsle(tes3$HOSPITALBILL, tes3[,"prediction"])

lm4 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 4,])
test_data4 <- test_predict_data[test_predict_data$AGEGROUP == 4,]
test_data4[, "TYPEOFHOSP"] <- apply(test_data4, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 4,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
test_data4[, "DIAGNOSISGROUP"] <- apply(test_data4, 1, function(row) ifelse(row["DIAGNOSISGROUP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 4,"DIAGNOSISGROUP"])), row["DIAGNOSISGROUP"], "Others" ))
prediction4 <- predict(lm4, type="response", newdata=test_data4[test_data4$AGEGROUP == 4, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes4 <- cbind(test_data4[test_data4$AGEGROUP == 4, ], (prediction4)^4)
colnames(tes4)[ncol(tes4)] <- "prediction"
#rmsle(tes4$HOSPITALBILL, tes4[,"prediction"])

tesmain <- rbind(tes1, tes2, tes3, tes4)
rmsle(tesmain$HOSPITALBILL, tesmain$prediction) ## 1.093548 1.029818

tesmain3.1.1 <- merge(tesmain, tesmain3.1, by=!colnames(tesmain) %in% c("prediction"))
tesmain3.1.1[, "preMean"] <- apply(tesmain3.1.1, 1, function(row) mean(c(as.numeric(row["prediction"]), as.numeric(row["(prediction)^4"])))) 
tesmain3.1.1[, "preMin"] <- apply(tesmain3.1.1, 1, function(row) min(as.numeric(row["prediction"]), as.numeric(row["(prediction)^4"])))
rmsle(tesmain3.1.1$HOSPITALBILL, tesmain3.1.1$preMean) ## 1.054006
rmsle(tesmain3.1.1$HOSPITALBILL, tesmain3.1.1$preMin) ## 1.049124












### ITERATION 4
BILLCATCODE <- data.frame(BILLCATCODE=c(1:4), BILLCAT=unique(bills$BILLCAT))
tesmain <- tes4[0,]

## need to test with AGE GROUP, BILL CAT total 16
for(i in 1:4){
  for(j in 1:4){
    lm4 <- lm(qutbill ~ AGE + GENDER + DURATIONOFSTAY + YEAROFADM +
                Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                Public.Specialist + Specialist + Surgery +
                Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
              , data=estimation_data[estimation_data$AGEGROUP == i & estimation_data$BILLCAT == BILLCATCODE[j,2],])
    
    test_data4 <- test_predict_data[test_predict_data$AGEGROUP == i & test_predict_data$BILLCAT == BILLCATCODE[j,2],]
    
    prediction4 <- predict(lm4, type="response", newdata=test_data4)
    tes4 <- cbind(test_data4, (prediction4)^4)
    colnames(tes4)[ncol(tes4)] <- "prediction"
    rmsle(tes4$HOSPITALBILL, tes4[,"prediction"])
    
    tesmain <- rbind(tesmain, tes4)
  }
}

rmsle(tesmain$HOSPITALBILL, tesmain$prediction) #0.9906317

## testing for one group out of 16
lm4 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + YEAROFADM +
            Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
            Public.Specialist + Specialist + Surgery +
            Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
            , data=estimation_data[estimation_data$AGEGROUP == 4,])

test_data4 <- test_predict_data[test_predict_data$AGEGROUP == 4,]

prediction4 <- predict(lm4, type="response", newdata=test_data4[test_data4$AGEGROUP == 4,])
tes4 <- cbind(test_data4[test_data4$AGEGROUP == 4, ], (prediction4)^4)
colnames(tes4)[ncol(tes4)] <- "prediction"
rmsle(tes4$HOSPITALBILL, tes4[,"prediction"])

### testing for ADM2013

#x <- estimation_data
#y <- test_predict_data

#estimation_data <- estimation_data[estimation_data$BILLCAT == "", ]
#test_predict_data <- test_predict_data[test_predict_data$BILLCAT == "", ]

logreg_solution <- glm(ADM2013 ~ AGE + GENDER +factor(BILLCAT) + DURATIONOFSTAY + YEAROFADM +
                         Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                         Public.Specialist + Specialist + Surgery +
                         Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
  , family=binomial(link="logit"),  data=estimation_data)
test_Probability_class1_log <- predict(logreg_solution, type="response", newdata=test_predict_data)
test_prediction_class_log   <- 1*as.vector(test_Probability_class1_log > .25)

test101 <- cbind(test_predict_data, test_prediction_class_log)

## hit rate
100*sum(test_prediction_class_log==test_predict_data[, "ADM2013"])/length(test_predict_data[, "ADM2013"])

##
sum((!test_predict_data[, "ADM2013"]))/nrow(test_predict_data)

## confusion matrix
conf_matrix = matrix(rep(0,4),ncol=2)
conf_matrix[1,1]<- 100*sum(test_prediction_class_log*test_predict_data[, "ADM2013"])/sum(test_predict_data[, "ADM2013"])
conf_matrix[1,2]<- 100*sum((!test_prediction_class_log)*test_predict_data[, "ADM2013"])/sum(test_predict_data[, "ADM2013"])
conf_matrix[2,2]<- 100*sum((!test_prediction_class_log)*(!test_predict_data[, "ADM2013"]))/sum((!test_predict_data[, "ADM2013"]))
conf_matrix[2,1]<- 100*sum((test_prediction_class_log)*(!test_predict_data[, "ADM2013"]))/sum((!test_predict_data[, "ADM2013"]))
conf_matrix = round(conf_matrix,2)

colnames(conf_matrix) <- c("Predicted 1", "Predicted 0")
rownames(conf_matrix) <- c("Actual 1", "Actual 0")

conf_matrix

#estimation_data <- x
#test_predict_data <- y
