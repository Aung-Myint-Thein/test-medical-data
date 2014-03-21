bills <- read.csv("data/1 Core Data/Hospital Bills Final Set.csv")

bills <- bills[!is.na(bills$HOSPITALBILL), ]

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
