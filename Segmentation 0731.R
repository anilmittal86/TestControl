library(RODBC) 
library(readr)
library(ggplot2)
library(plotly)
library(dplyr)

dbhandle = odbcConnect("PMData",uid = 'datascientist', pwd = '!dtl#21Science')
dbhandle = odbcConnect("PMData",uid = 'datascientist', pwd = '!dtl#21Science')

#dbhandle <- odbcDriverConnect(connection="Driver={SQL Server};server=DTLDEVDS0002;database=DTL;trusted_connection=true")


customers_c = sqlQuery(dbhandle,paste("SELECT * FROM customer_master_ALL"))

customers = subset(customers_c,customers_c$createddate >= '2012-01-01')
customers = subset(customers,customers$LifeTimeSales > 0)

customers = subset(customers,!is.na(customers$FirstActivityDate))

customers$gender_male = ifelse(customers$gender == 'Male',1,0)
customers$gender_female = ifelse(customers$gender == 'Female',1,0)

customers$age_20 =ifelse(customers$age > 20 & customers$age <= 30 ,1,0)
customers$age_30 =ifelse(customers$age > 30 & customers$age <= 40 ,1,0)
customers$age_40 =ifelse(customers$age > 40 & customers$age <= 50 ,1,0)
customers$age_50 =ifelse(customers$age > 50 & customers$age <= 60 ,1,0)
customers$age_60 =ifelse(customers$age > 60 & customers$age <= 70 ,1,0)
#customers$country_GE = NULL
customers$country_DE =ifelse(customers$countrycode == 'DE',1,0)
customers$country_SE =ifelse(customers$countrycode == 'SE',1,0)
customers$country_FI =ifelse(customers$countrycode == 'FI',1,0)
customers$country_CH =ifelse(customers$countrycode == 'CH',1,0)
customers$country_IT =ifelse(customers$countrycode == 'IT',1,0)
customers$country_NO =ifelse(customers$countrycode == 'NO',1,0)
customers$country_AT =ifelse(customers$countrycode == 'AT',1,0)
customers$country_JP =ifelse(customers$countrycode == 'JP',1,0)
customers$country_PL =ifelse(customers$countrycode == 'PL',1,0)
customers$country_ES =ifelse(customers$countrycode == 'ES',1,0)






customers_f = customers[,c('customernumber',
                           #'LastThreeMonthOrders',
                           'FirstThreeMonthOrders',
                           #'FirstThreeMonthReturnedOrders',
                           'FirstThreeMonthPayout',
                           'LifeTimeSales',
                           #'HighestQualiLevel',
                           #'CountofFirstSixMonthPayout',
                           'days_since_first_addition',
                           #'LifeTimeOrders',
                           #'NumberofTimesDowngraded',
                           #'LastSixMonthPayOut',
                           #'DaysSinceLastOrder',
                           'avg_days_between_network_addition',
                           #'LastThreeMonthSales',
                           'FirstThreeMonthSales',
                           #'FirstThreeMonthCancelledOrders',
                           #'FirstCommissionPayout',
                           'DaysTakenForSecondOrder',
                           #'Tenure',
                           'gender_male',
                           'gender_female',
                           'age_20',
                           'age_30',
                           'age_40',
                           'age_50',
                           'age_60',
                           'avgdiscountRate','lifetimepayout',
                           'ActiveSubscriptions',
                           'NumberofTimesUpgraded')]


customers_f$days_since_first_addition[is.na(customers_f$days_since_first_addition)] = 365
customers_f$days_since_first_addition[customers_f$days_since_first_addition < 0] = 0
customers_f$Network_Added_90days = ifelse(customers_f$days_since_first_addition < 90,1,0)
customers_f$days_since_first_addition = NULL


customers_f$avgdiscountRate[is.na(customers_f$avgdiscountRate)] = 0
customers_f$avgdiscountRate[customers_f$avgdiscountRate > 20] = 20
customers_f$avgdiscountRate = (customers_f$avgdiscountRate * 1.00) / 20


customers_f$lifetimepayout[is.na(customers_f$lifetimepayout)] = 0
x = mean(customers_f$lifetimepayout) + (2 * sd(customers_f$lifetimepayout))
customers_f$lifetimepayout[customers_f$lifetimepayout > x] = x
customers_f$lifetimepayout = (customers_f$lifetimepayout * 1.00) / x

customers_f$ActiveSubscriptions[is.na(customers_f$ActiveSubscriptions)] = 0
customers_f$ActiveSubscriptions = ifelse(customers_f$ActiveSubscriptions > 0 ,1,0)

customers_f$MultipleNetworkAddition = ifelse(customers_f$avg_days_between_network_addition >= 0 ,1,0)
customers_f$avg_days_between_network_addition = NULL

customers_f$DaysTakenForSecondOrder[is.na(customers_f$DaysTakenForSecondOrder)] = 365
customers_f$DaysTakenForSecondOrder[customers_f$DaysTakenForSecondOrder > 365] = 365
customers_f$Second_Order_180days = ifelse(customers_f$DaysTakenForSecondOrder < 180,1,0)
customers_f$DaysTakenForSecondOrder = NULL

customers_f[is.na(customers_f)] = 0


#customers_f$LastThreeMonthOrders = ifelse(customers_f$LastThreeMonthOrders > 0,1,0)

y = mean(customers_f$FirstThreeMonthOrders) + (2 * sd(customers_f$FirstThreeMonthOrders))
customers_f$FirstThreeMonthOrders[customers_f$FirstThreeMonthOrders > y] = y
customers_f$FirstThreeMonthOrders = (customers_f$FirstThreeMonthOrders * 1.00) / y

z = mean(customers_f$FirstThreeMonthSales) + (2 * sd(customers_f$FirstThreeMonthSales))
customers_f$FirstThreeMonthSales[customers_f$FirstThreeMonthSales > z] = z
customers_f$FirstThreeMonthSales = (customers_f$FirstThreeMonthSales * 1.00) / z

#boxplot(customers_f$FirstThreeMonthReturnedOrders)
#summary(customers_f$FirstThreeMonthReturnedOrders)
#customers_f$FirstThreeMonthReturnedOrders = ifelse(customers_f$FirstThreeMonthReturnedOrders > 0,1,0)

#table(customers_f$QualiLevel,customers_f$Sponsor_1019070)

customers_f$FirstThreeMonthPayout = ifelse(customers_f$FirstThreeMonthPayout > 0,1,0)
#customers_f$FirstThreeMonthCancelledOrders = ifelse(customers_f$FirstThreeMonthCancelledOrders > 0,1,0)

a = mean(customers_f$LifeTimeSales) + (2 * sd(customers_f$LifeTimeSales))
customers_f$LifeTimeSales[customers_f$LifeTimeSales > a] = a
customers_f$LifeTimeSales = (customers_f$LifeTimeSales * 1.00) / a

#b = mean(customers_f$LifeTimeOrders) + (2 * sd(customers_f$LifeTimeOrders))
#customers_f$LifeTimeOrders[customers_f$LifeTimeOrders > b] = b
#customers_f$LifeTimeOrders = (customers_f$LifeTimeOrders * 1.00) / b

#customers_f$FirstCommissionPayout = ifelse(customers_f$FirstCommissionPayout > 0,1,0)


#c = mean(customers_f$Tenure) + (2 * sd(customers_f$Tenure))
#customers_f$Tenure[customers_f$Tenure < 0] = 0
#customers_f$Tenure[customers_f$Tenure > c] = c
#customers_f$Tenure = (customers_f$Tenure * 1.00) / c

#customers_f$HighestQualiLevel[customers_f$HighestQualiLevel > 10] = 10
#customers_f$HighestQualiLevel = (customers_f$HighestQualiLevel * 1.00) / 10


# wss <- (nrow(customers_f)-1)*sum(apply(customers_f,2,var))
# for (i in 2:20) wss[i] <- sum(kmeans(customers_f, centers=i)$withinss)
# plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

set.seed(20)
Cluster <- kmeans(customers_f[,-1],4)

#Cluster <- hclust(dist(iris[, 3:4]))

#plot(clusters)

k = data.frame(Cluster$centers)
for(i in 1:length(k))
{
  k[6,names(k[i])] = mean(customers_f[,names(k[i])])
}

#k$Tenure = k$Tenure  * c
k$LifeTimeSales = k$LifeTimeSales * a
#k$LifeTimeOrders = k$LifeTimeOrders * b
k$FirstThreeMonthOrders = k$FirstThreeMonthOrders * y
k$FirstThreeMonthSales = k$FirstThreeMonthSales * z
k$avgdiscountRate = k$avgdiscountRate * 20
k$lifetimepayout = k$lifetimepayout * x


#Cluster$size
#Cluster$betweenss/Cluster$totss
#cor = cor(customers_f[,-1])
#customers = cbind(customers,Cluster$cluster)
#library('fpc')
#plotcluster(customers_f, Cluster$cluster)

#Split cluster three in further clusters
clusters_f1 = cbind(customers_f,cluster= Cluster$cluster)

bb = clusters_f1$cluster[which.max(clusters_f1$lifetimepayout)]

clusters_f1 = subset(clusters_f1,clusters_f1$cluster == bb)
clusters_f1$cluster = NULL



clusters_f1 = clusters_f1[,c('customernumber',
                             'FirstThreeMonthOrders',
                             'FirstThreeMonthPayout',
                             'LifeTimeSales',
                             'Network_Added_90days',
                             'MultipleNetworkAddition',
                             'FirstThreeMonthSales',
                             'lifetimepayout',
                             'NumberofTimesUpgraded')]


# wss <- (nrow(clusters_f1)-1)*sum(apply(clusters_f1,2,var))
# for (i in 2:20) wss[i] <- sum(kmeans(clusters_f1, centers=i)$withinss)
# plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

set.seed(20)
Cluster1 <- kmeans(clusters_f1[,-1],4)

# Cluster1$size
# Cluster1$betweenss/Cluster$totss


g = data.frame(Cluster1$centers)
for(i in 1:length(g))
{
  g[6,names(g[i])] = mean(clusters_f1[,names(g[i])])
}


g$LifeTimeSales = g$LifeTimeSales * a
g$FirstThreeMonthOrders = g$FirstThreeMonthOrders * y
g$FirstThreeMonthSales = g$FirstThreeMonthSales * z
g$lifetimepayout = g$lifetimepayout * x

cluster2 = cbind(customers,cluster= Cluster$cluster)
cluster2 = subset(cluster2,cluster2$cluster == bb)
cluster2$cluster = NULL

cluster2 = cbind(cluster2,cluster= Cluster1$cluster)

#Churn Profiling of clusters
clusters_updated = cbind(customers,cluster= Cluster$cluster)

#CustomersclusterInfo = clusters_updated[,c(1,94)]


library(flexclust)
system.time(km <- as.kcca(Cluster, data=customers_f[,-1], k = 4))
#    user  system elapsed 
#  read data from all the customers
customers_all = sqlQuery(dbhandle,paste("SELECT * FROM customer_master"))
customers_all_OD = customers_all

customers_all$gender_male = ifelse(customers_all$gender == 'Male',1,0)
customers_all$gender_female = ifelse(customers_all$gender == 'Female',1,0)

customers_all$age_20 =ifelse(customers_all$age > 20 & customers_all$age <= 30 ,1,0)
customers_all$age_30 =ifelse(customers_all$age > 30 & customers_all$age <= 40 ,1,0)
customers_all$age_40 =ifelse(customers_all$age > 40 & customers_all$age <= 50 ,1,0)
customers_all$age_50 =ifelse(customers_all$age > 50 & customers_all$age <= 60 ,1,0)
customers_all$age_60 =ifelse(customers_all$age > 60 & customers_all$age <= 70 ,1,0)

customers_all = customers_all[,c('customernumber',
                                 'FirstThreeMonthOrders',
                                 'FirstThreeMonthPayout',
                                 'LifeTimeSales',
                                 'days_since_first_addition',
                                 'avg_days_between_network_addition',
                                 'FirstThreeMonthSales',
                                 'DaysTakenForSecondOrder',
                                 'gender_male',
                                 'gender_female',
                                 'age_20',
                                 'age_30',
                                 'age_40',
                                 'age_50',
                                 'age_60',
                                 'avgdiscountRate',
                                 'lifetimepayout',
                                 'ActiveSubscriptions',
                                 'NumberofTimesUpgraded')]

customers_all$days_since_first_addition[is.na(customers_all$days_since_first_addition)] = 365
customers_all$days_since_first_addition[customers_all$days_since_first_addition < 0] = 0
customers_all$Network_Added_90days = ifelse(customers_all$days_since_first_addition < 90,1,0)
customers_all$days_since_first_addition = NULL


customers_all$avgdiscountRate[is.na(customers_all$avgdiscountRate)] = 0
customers_all$avgdiscountRate[customers_all$avgdiscountRate > 20] = 20
customers_all$avgdiscountRate = (customers_all$avgdiscountRate * 1.00) / 20


customers_all$lifetimepayout[is.na(customers_all$lifetimepayout)] = 0
customers_all$lifetimepayout[customers_all$lifetimepayout > x] = x
customers_all$lifetimepayout = (customers_all$lifetimepayout * 1.00) / x

customers_all$ActiveSubscriptions[is.na(customers_all$ActiveSubscriptions)] = 0
customers_all$ActiveSubscriptions = ifelse(customers_all$ActiveSubscriptions > 0 ,1,0)

customers_all$MultipleNetworkAddition = ifelse(customers_all$avg_days_between_network_addition >= 0 ,1,0)
customers_all$avg_days_between_network_addition = NULL

customers_all$DaysTakenForSecondOrder[is.na(customers_all$DaysTakenForSecondOrder)] = 365
customers_all$DaysTakenForSecondOrder[customers_all$DaysTakenForSecondOrder > 365] = 365
customers_all$Second_Order_180days = ifelse(customers_all$DaysTakenForSecondOrder < 180,1,0)
customers_all$DaysTakenForSecondOrder = NULL

customers_all[is.na(customers_all)] = 0

customers_all$FirstThreeMonthOrders[customers_all$FirstThreeMonthOrders > y] = y
customers_all$FirstThreeMonthOrders = (customers_all$FirstThreeMonthOrders * 1.00) / y
customers_all$FirstThreeMonthSales[customers_all$FirstThreeMonthSales > z] = z
customers_all$FirstThreeMonthSales = (customers_all$FirstThreeMonthSales * 1.00) / z
customers_all$FirstThreeMonthPayout = ifelse(customers_all$FirstThreeMonthPayout > 0,1,0)
customers_all$LifeTimeSales[customers_all$LifeTimeSales > a] = a
customers_all$LifeTimeSales = (customers_all$LifeTimeSales * 1.00) / a



system.time(pred <- predict(km, newdata=customers_all[,-1]))
clusters_updated1 = data.frame(pred,customers_all)
#clusters_updated2 = data.frame(pred,customernumber  = customers_all$customernumber)


# Predicitng again for sub-segments of cluster 3
system.time(km1 <- as.kcca(Cluster1, data=clusters_f1[,-1], k = 4))

customers_all_3 = subset(clusters_updated1,clusters_updated1$pred == bb)
customers_all_3 = customers_all_3[,c('customernumber',
                                     'FirstThreeMonthOrders',
                                     'FirstThreeMonthPayout',
                                     'LifeTimeSales',
                                     'Network_Added_90days',
                                     'MultipleNetworkAddition',
                                     'FirstThreeMonthSales',
                                     'lifetimepayout',
                                     'NumberofTimesUpgraded')]

system.time(pred1 <- predict(km1, newdata=customers_all_3[,-1]))
clusters_updated_3 = data.frame(cluster = pred1,customernumber = customers_all_3$customernumber)
#write.csv(clusters_updated_3,"C:\\Users\\anil.m\\Desktop\\SubSegmentsCluster3.csv",row.names=FALSE)
clusters_updated_3$cluster = clusters_updated_3$cluster + 4
#clusters_updated2[clusters_updated2$pred == 1] = clusters_updated_3$cluster

clusters_updated = left_join(clusters_updated1,clusters_updated_3[,c("customernumber","cluster")], by = "customernumber")
clusters_updated$pred = ifelse(clusters_updated$pred == bb,clusters_updated$cluster,clusters_updated$pred)

customers_all_OD$lifetimepayout[is.na(customers_all_OD$lifetimepayout)] = 0
clusters_updated = left_join(clusters_updated,customers_all_OD[,c("customernumber","lifetimepayout")], by = "customernumber")

clusters_updated$pred = ifelse(clusters_updated$lifetimepayout.y > 100000,9,clusters_updated$pred)

clusters_updated = clusters_updated[,c("customernumber","pred")]
