trainData <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSISCODE  + BILLCAT + DURATIONOFSTAY + TYPEOFHOSP , bills, mean)
trainData <- trainData[order(trainData$HRN),]

trainData <- merge(trainData, unique(bills[, c("ID", "HRN", "DIAGNOSISGROUP", "AGEGROUP", "DATEOFADM")]), by=c("ID", "HRN"), sort=F, all.x=T)
trainData[, "YEAROFADM"] <- apply(trainData, 1, function(row) as.numeric(format(as.Date(row["DATEOFADM"], "%d/%m/%Y"), "%Y")))

set.seed(123442323)

estimation_data_ids <- sample.int(nrow(trainData), 12000)
estimation_data <- trainData[estimation_data_ids, ]

non_estimation_data <- setdiff(1:nrow(trainData),estimation_data_ids)
test_data <- trainData[non_estimation_data, ]

test_data_ids <- sample(rownames(test_data), 2000)
test_predict_data <- test_data[test_data_ids, ]

#estimation_data[, "logbill"] <- ifelse(estimation_data[, "HOSPITALBILL"] == 0, 0, log(estimation_data[, "HOSPITALBILL"]))
estimation_data[, "sqrtbill"] <- sqrt(estimation_data[, "HOSPITALBILL"])
estimation_data[, "qutbill"] <- estimation_data[, "HOSPITALBILL"]^(1/4)
hist(estimation_data[, "HOSPITALBILL"])
#hist(estimation_data[, "logbill"])
hist(estimation_data[, "qutbill"])

#lm1 <- lm(qutbill ~ AGE + GENDER + BILLCAT + DURATIONOFSTAY, data=estimation_data)
lm1 <- lm(qutbill ~ factor(AGEGROUP) + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data)

prediction <- predict(lm1, type="response", newdata=test_predict_data[, c("AGEGROUP", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
hist(prediction)
hist((prediction)^4)

tes <- cbind(test_predict_data, (prediction)^4)

rmsle(tes$HOSPITALBILL, tes[,"(prediction)^4"])



estimation_data_ids <- sample.int(nrow(bills), 5000)
estimation_data <- bills[estimation_data_ids, ]

non_estimation_data <- setdiff(1:nrow(bills),estimation_data_ids)
test_data <- bills[non_estimation_data, ]

lm1 <- lm(HOSPITALBILL ~ factor(BILLCAT) + inpatient + outpatient + surgpatient + AGE + GENDER + DURATIONOFSTAY, data=estimation_data)

prediction <- predict(lm1, type="response", newdata=test_data[1:4000, c("BILLCAT","inpatient", "outpatient", "surgpatient", "AGE", "GENDER","DURATIONOFSTAY")])

tes <- cbind(test_data[1:4000,], prediction)
tes[, "prediction"] <- ifelse(tes[, "prediction"] < 0, 0, tes[, "prediction"])

rmsle(tes$HOSPITALBILL, tes[,"prediction"])


## testing with age group
lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 1,])
prediction1 <- predict(lm1, type="response", newdata=test_predict_data[test_predict_data$AGEGROUP == 1, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes1 <- cbind(test_predict_data[test_predict_data$AGEGROUP == 1, ], (prediction1)^4)
colnames(tes1)[ncol(tes1)] <- "prediction"
rmsle(tes1$HOSPITALBILL, tes1[,"prediction"])

lm2 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 2,])
test_data2 <- test_predict_data[test_predict_data$AGEGROUP == 2,]
test_data2[, "TYPEOFHOSP"] <- apply(test_data2, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 2,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
prediction2 <- predict(lm2, type="response", newdata=test_data2[test_data2$AGEGROUP == 2, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes2 <- cbind(test_data2[test_data2$AGEGROUP == 2, ], (prediction2)^4)
colnames(tes2)[ncol(tes2)] <- "prediction"
rmsle(tes2$HOSPITALBILL, tes2[,"prediction"])

lm3 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 3,])
prediction3 <- predict(lm3, type="response", newdata=test_predict_data[test_predict_data$AGEGROUP == 3, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes3 <- cbind(test_predict_data[test_predict_data$AGEGROUP == 3, ], (prediction3)^4)
colnames(tes3)[ncol(tes3)] <- "prediction"
rmsle(tes3$HOSPITALBILL, tes3[,"prediction"])

lm4 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP) + YEAROFADM, data=estimation_data[estimation_data$AGEGROUP == 4,])
test_data4 <- test_predict_data[test_predict_data$AGEGROUP == 4,]
test_data4[, "TYPEOFHOSP"] <- apply(test_data4, 1, function(row) ifelse(row["TYPEOFHOSP"] %in% as.character(unique(estimation_data[estimation_data$AGEGROUP == 4,"TYPEOFHOSP"])), row["TYPEOFHOSP"], "Others" ))
prediction4 <- predict(lm4, type="response", newdata=test_data4[test_data4$AGEGROUP == 4, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP", "YEAROFADM")])
tes4 <- cbind(test_data4[test_data4$AGEGROUP == 4, ], (prediction4)^4)
colnames(tes4)[ncol(tes4)] <- "prediction"
rmsle(tes4$HOSPITALBILL, tes4[,"prediction"])

tesmain <- rbind(tes1, tes2, tes3, tes4)
rmsle(tesmain$HOSPITALBILL, tesmain$prediction)


## over fitting
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


