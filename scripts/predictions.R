trainData <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSISCODE  + BILLCAT + DURATIONOFSTAY + TYPEOFHOSP , bills, mean)
trainData <- trainData[order(trainData$HRN),]

trainData <- merge(trainData, unique(bills[, c("ID", "HRN", "DIAGNOSISGROUP")]), by=c("ID", "HRN"), sort=F, all.x=T)

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
lm1 <- lm(qutbill ~ AGE + GENDER + factor(BILLCAT) + DURATIONOFSTAY + factor(TYPEOFHOSP) + factor(DIAGNOSISGROUP), data=estimation_data)

prediction <- predict(lm1, type="response", newdata=test_predict_data[, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "TYPEOFHOSP", "DIAGNOSISGROUP")])
hist(prediction)
hist((prediction)^4)

tes <- cbind(test_predict_data, (prediction)^4)
#library(Metrics)

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
