## import the bills data
bills <- read.csv("data/1 Core Data/Hospital Bills Final Set.csv")
bills <- bills[!is.na(bills$HOSPITALBILL), ]
bills[, "AGE"] <- 2014 - bills[, "yyyy"]
bills[, "DURATIONOFSTAY"] <- as.numeric(difftime(as.Date(bills[, "DATEDISCHARGE"], "%d/%m/%Y"), as.Date(bills[, "DATEOFADM"], "%d/%m/%Y"), units="days"))

WardType <- read.csv("data/1 Core Data/Reference - WardType.csv")


## exploraton for age
summary(bills$AGE)
boxplot(bills$AGE)
barplot(table(bills$AGE))

hist(bills[bills$HOSPITALBILL < 2000, "HOSPITALBILL"])

hist(bills[bills$HOSPITALBILL < 20, "HOSPITALBILL"])

hist(bills[bills$HOSPITALBILL > 500 & bills$HOSPITALBILL < 2000, "HOSPITALBILL"])

table(bills[bills$HOSPITALBILL < 20, "HOSPITALBILL"])

head(bills[bills$HOSPITALBILL == 0, ])

bills <- bills[order(bills$ID, bills$yyyy, bills$DATEOFEVENT, bills$HOSPITALBILL, decreasing=F),]

head(bills, 30)

big.ID <- bills[bills$ID=="C000003554",]
sum(big.ID$HOSPITALBILL)

length(unique(big.ID$HRN))

aggregate(HOSPITALBILL ~ HRN , big.ID, sum)

bills2 <- aggregate(HOSPITALBILL ~ HRN , bills, sum)
bills2 <- bills2[order(bills2$HRN),]

bills3 <- aggregate(HOSPITALBILL ~ ID + HRN , bills, sum)
bills3 <- bills3[order(bills3$HRN),]

bills4 <- aggregate(HOSPITALBILL ~ ID + HRN + GENDER + BILLCAT + DIAGNOSUS + AGE , bills, sum)
bills4 <- bills4[order(bills4$HRN),]

bills5 <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSUS + BILLCAT , bills, sum)

tb <- bills[1:12, ]

tb2 <- aggregate(HOSPITALBILL ~ ID + HRN , tb, sum)
tb3 <- aggregate(HOSPITALBILL ~ ID + HRN + GENDER + BILLCAT + DIAGNOSUS + AGE , tb, sum)
tb4 <- aggregate(HOSPITALBILL ~ ID + AGE + GENDER + HRN + HOSPITAL + DIAGNOSUS + BILLCAT , tb, sum)

for(i in 2:nrow(bills4)){
  if(bills4[i-1, 2] == uni2[i, 2]){
    print(uni2[i,])
  }
}

bills2[table(bills3[,2]) > 1,]

ndates1 <- difftime(as.Date(bills[, "DATEDISCHARGE"], "%d/%m/%Y"),as.Date(bills[, "DATEOFADM"], "%d/%m/%Y"), units="auto")
head(ndates1)
ndates1[1]- ndates1[2]
