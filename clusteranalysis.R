medicare <- read_delim("medicare.txt", "\t", escape_double = FALSE)

beneficiaries1 <- select(medicare, starts_with("beneficiary_cc"),total_medicare_payment_amt, total_med_medicare_payment_amt,total_drug_medicare_payment_amt, beneficiary_average_age, beneficiary_female_count, beneficiary_male_count,total_services, total_unique_benes)

beneficiaries_sample <- beneficiaries1[sample(nrow(beneficiaries1), 100000), ]

wss <- (nrow(beneficiaries_sample)-1)*sum(apply(beneficiaries_sample,2,var))
for (i in 2:20) {wss[i] <- kmeans(na.omit(beneficiaries_sample), centers = i, nstart = 20)$tot.withinss
}

plot(1:20,wss, type = 'b', main = 'Sample 1', xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# based on plot above, optimal number of clusters is 10

### create new sample and determine best clusters 
beneficiaries_sample2 <- beneficiaries[sample(nrow(beneficiaries), 100000), ]

wss2 <- (nrow(beneficiaries_sample2)-1)*sum(apply(beneficiaries_sample2,2,var))
for (i in 2:20) {wss2[i] <- kmeans(na.omit(beneficiaries_sample2), centers = i, nstart = 20)$tot.withinss
}

plot(1:20,wss2, type = 'b', main = "Sample 2", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# optimal is 9 clusters

## create new sample and determine best clusters 
beneficiaries_sample3 <- beneficiaries[sample(nrow(beneficiaries), 100000), ]

wss3 <- (nrow(beneficiaries_sample3)-1)*sum(apply(beneficiaries_sample3,2,var))
for (i in 2:20) {wss3[i] <- kmeans(na.omit(beneficiaries_sample3), centers = i, nstart = 20)$tot.withinss
}

plot(1:20,wss3, type = 'b', main ="Sample 3", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# optimal is 9 clusters 

## create new sample and determine best clusters 
beneficiaries_sample4 <- beneficiaries[sample(nrow(beneficiaries), 100000), ]

wss4 <- (nrow(beneficiaries_sample4)-1)*sum(apply(beneficiaries_sample4,2,var))
for (i in 2:20) {wss4[i] <- kmeans(na.omit(beneficiaries_sample4), centers = i, nstart = 20)$tot.withinss
}

plot(1:20,wss4, type = 'b', main ="Sample 4", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# optimal is 10 clusters 

## create new sample and determine best clusters 
beneficiaries_sample5 <- beneficiaries[sample(nrow(beneficiaries), 100000), ]

wss5 <- (nrow(beneficiaries_sample5)-1)*sum(apply(beneficiaries_sample5,2,var))
for (i in 2:20) {wss5[i] <- kmeans(na.omit(beneficiaries_sample5), centers = i, nstart = 20)$tot.withinss
}

plot(1:20,wss5, type = 'b', main ="Sample 5", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# optimal number is 15 clusters 

## create new sample and determine best clusters 
beneficiaries_sample6 <- beneficiaries[sample(nrow(beneficiaries), 100000), ]

wss6 <- (nrow(beneficiaries_sample6)-1)*sum(apply(beneficiaries_sample6,2,var))
for (i in 2:20) {wss6[i] <- kmeans(na.omit(beneficiaries_sample6), centers = i, nstart = 20)$tot.withinss
}

plot(1:20,wss6, type = 'b', main ="Sample 6", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

# optimal is 10

#################################

# based on the 6 random samples, 10 is the optimal number of clusters. Now run the kmeans algorithm with k = 10

km.out1 <- kmeans(na.omit(beneficiaries1),10,nstart = 10)
km.out1$tot.withinss

plot(beneficiaries1$beneficiary_cc_afib_percent,beneficiaries1$total_medicare_payment_amt,col=km.out1$cluster, xlab = 'Chronic Illness Atrial Fibrulation', ylab='Medicare Payment')
plot(beneficiaries1$total_medicare_payment_amt, beneficiaries1$total_services, col=km.out1$cluster)

variance <- matrix(0,1,24)

for (i in 1:24){variance[i]<-var(km.out1$centers[ ,i])}

> write.csv(variance, file = "variance.csv")

