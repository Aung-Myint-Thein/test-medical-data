trainData <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSISCODE  + BILLCAT + DURATIONOFSTAY + TYPEOFHOSP , bills, mean)
trainData <- trainData[order(trainData$HRN),]

trainData <- merge(trainData, unique(bills[, c("ID", "HRN", "DIAGNOSISGROUP", "AGEGROUP", "DATEOFADM", "DIAGNOSISGROUPCODE")]), by=c("ID", "HRN"), sort=F, all.x=T)
trainData[, "YEAROFADM"] <- apply(trainData, 1, function(row) as.numeric(format(as.Date(row["DATEOFADM"], "%d/%m/%Y"), "%Y")))
trainData[, "qutbill"] <- trainData[, "HOSPITALBILL"]^(1/4)
trainData[, "sqrtbill"]<- trainData[, "HOSPITALBILL"]^(1/2)

#diagroupcode <- data.frame(DIAGNOSISGROUPCODE=c(1:23), DIAGNOSISGROUP=unique(trainData$DIAGNOSISGROUP))
#trainData <- merge(trainData, diagroupcode, by=c("DIAGNOSISGROUP"), all.x=T, sort=F)

trainData[, "DIAGNOSISGROUPCLUSTER"] <- apply(trainData, 1, function(row) ifelse(row["DIAGNOSISGROUPCODE"] %in% c(1,2,5,7,16,17,21), "A", ifelse(row["DIAGNOSISGROUPCODE"] %in% c(4,8,11,12,13,14,18,19,23), "B", "C")))

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
hist(prediction)
hist((prediction)^4)

tes <- cbind(test_predict_data, (prediction)^4)

rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"])

## iteration 2
lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM , data=estimation_data)
prediction <- predict(lm1, type="response", newdata=test_predict_data[, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes <- cbind(test_predict_data, (prediction)^4)
rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"])


## testing with age group
lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 1,])
prediction1 <- predict(lm1, type="response", newdata=test_predict_data[test_predict_data$AGEGROUP == 1, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes1 <- cbind(test_predict_data[test_predict_data$AGEGROUP == 1, ], (prediction1)^4)
colnames(tes1)[ncol(tes1)] <- "prediction"
#rmsle(tes1$HOSPITALBILL, tes1[,"prediction"])

lm2 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 2,])
test_data2 <- test_predict_data[test_predict_data$AGEGROUP == 2,]
test_data2[, "TYPEOFHOSP"] <- apply(test_data2, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 2,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
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
rmsle(tesmain$HOSPITALBILL, tesmain$prediction)






