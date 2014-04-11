a <- rep(1:20)
a <- a/20

logitresult <- data.frame(threashold=a, hitrate=a, truepositiverate=a, truenegativerate=a)
colnames(logitresult) <- c("threashold", "Hit rate", "True positive rate", "True negative rate")

logreg_solution <- glm(ADM2013 ~ AGE + GENDER +factor(BILLCAT) + DURATIONOFSTAY + YEAROFADM +
                         Cancer.Specialist + Community.Hospital + Dental + ENT + Eye + Kidney + Others + Oversea.Hospital + Private.Hospital + Public.Hospital + 
                         Public.Specialist + Specialist + Surgery +
                         Diseases.of.the.genitourinary.system + Symptoms.signs.and.abnormal.clinical.and.laboratory.findings.not.elsewhere.classified + Diseases.of.the.nervous.system + Diseases.of.the.respiratory.system + Infectious.and.parasitic.diseases + Diseases.of.the.eye.and.adnexa + Diseases.of.the.sense.organs + Diseases.of.the.digestive.system + Diseases.of.the.circulatory.system + Diseases.of.the.skin.and.subcutaneous.tissue + Injury.poisoning.and.certain.other.consequences.of.external.causes + Diseases.of.the.musculoskeletal.system.and.connective.tissue + Neoplasms + Mental.and.behavioural.disorders + Factors.influencing.health.status.and.contact.with.health.services + Endocrine.nutritional.and.metabolic.diseases + Others + Diseases.of.the.ear.and.mastoid.process + Diseases.of.the.blood.and.bloodforming.organs.and.certain.disorders.involving.the.immune.mechanism + Pregnancy.childbirth.and.the.puerperium + Congenital.malformations.deformations.and.chromosomal.abnormalities + External.causes.of.morbidity.and.mortality + Certain.conditions.originating.in.the.perinatal.period
                       , family=binomial(link="logit"),  data=estimation_data)
test_Probability_class1_log <- predict(logreg_solution, type="response", newdata=test_predict_data)

for(i in 1:length(a)){

  test_prediction_class_log   <- 1*as.vector(test_Probability_class1_log > a[i])
  
  ## hit rate
  logitresult[i, 2] <- 100*sum(test_prediction_class_log==test_predict_data[, "ADM2013"])/length(test_predict_data[, "ADM2013"])
  
  ##
  #sum((!test_predict_data[, "ADM2013"]))/nrow(test_predict_data)
  
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
  
  logitresult[i, 3] <- conf_matrix[1,1]
  logitresult[i, 4] <- conf_matrix[2,2]
}

colorsL <- c("a", "blue", "yellow", "red")

plot(range(0,1), range(0,100), type="n", xlab="Probability Threashold", ylab="Accuracy Performance (%)" ) 

for (i in 2:ncol(logitresult)) {
  lines(logitresult[, 1], logitresult[, i], type="l", lwd=1.5, col=colorsL[i])     
} 


## ggplot
ggplot(logitresult, aes(threashold, ylab="performance")) + 
  geom_line(aes(y = hitrate, colour = "blue")) + 
  geom_line(aes(y = truepositiverate, colour = "yellow")) +
  geom_line(aes(y = truenegativerate, colour = "red"))


## better ggplot
test_data_long <- melt(logitresult, id="threashold")  # convert to long format

ggplot(data=test_data_long,
       aes(x=threashold, y=value, colour=variable)) +
  geom_line() + ylab("Accuracy Performance (%)") + xlab("Probability Threashold") + theme_bw()

