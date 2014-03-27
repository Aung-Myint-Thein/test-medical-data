trainData <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSUS + BILLCAT + DURATIONOFSTAY, bills, mean)
trainData <- trainData[order(trainData$HRN),]
trainData[, "inpatient"] <- ifelse(trainData[, "BILLCAT"] == "IN", 1, 0)
trainData[, "outpatient"] <- ifelse(trainData[, "BILLCAT"] == "OU", 1, 0)
trainData[, "surgpatient"] <- ifelse(trainData[, "BILLCAT"] == "DY", 1, 0)

estimation_data_ids <- sample.int(nrow(trainData), 5000)
estimation_data <- trainData[estimation_data_ids, ]

non_estimation_data <- setdiff(1:nrow(trainData),estimation_data_ids)
test_data <- trainData[non_estimation_data, ]


#estimation_data[, "logbill"] <- ifelse(estimation_data[, "HOSPITALBILL"] == 0, 0, log(estimation_data[, "HOSPITALBILL"]))
estimation_data[, "sqrtbill"] <- sqrt(estimation_data[, "HOSPITALBILL"])
estimation_data[, "qutbill"] <- estimation_data[, "HOSPITALBILL"]^(1/2)
hist(estimation_data[, "HOSPITALBILL"])
hist(estimation_data[, "logbill"])
hist(estimation_data[, "qutbill"])

#lm1 <- lm(qutbill ~ AGE + GENDER + BILLCAT + DURATIONOFSTAY, data=estimation_data)
lm1 <- lm(qutbill ~ AGE + GENDER + BILLCAT + DURATIONOFSTAY + inpatient + outpatient + surgpatient, data=estimation_data)

#family=poisson()

prediction <- predict(lm1, type="response", newdata=test_data[1:4000, c("AGE", "GENDER", "BILLCAT", "DURATIONOFSTAY", "inpatient", "outpatient", "surgpatient")])
hist(prediction)
hist((prediction)^2)

tes <- cbind(test_data[1:4000,], (prediction)^2)
#library(Metrics)

rmsle(tes$HOSPITALBILL, tes[,"(prediction)^2"])



estimation_data_ids <- sample.int(nrow(bills), 5000)
estimation_data <- bills[estimation_data_ids, ]

non_estimation_data <- setdiff(1:nrow(bills),estimation_data_ids)
test_data <- bills[non_estimation_data, ]

lm1 <- lm(HOSPITALBILL ~ inpatient + outpatient + surgpatient + AGE + GENDER + DURATIONOFSTAY, data=estimation_data)

prediction <- predict(lm1, type="response", newdata=test_data[1:4000, c("inpatient", "outpatient", "surgpatient", "AGE", "GENDER","DURATIONOFSTAY")])

tes <- cbind(test_data[1:4000,], prediction)
tes[, "prediction"] <- ifelse(tes[, "prediction"] < 0, 0, tes[, "prediction"])

rmsle(tes$HOSPITALBILL, tes[,"prediction"])
