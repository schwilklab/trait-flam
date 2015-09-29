myvars <- c("spcode", "moisture", "t2ignit", "spread")
burnt2 <- burnt[myvars]

burnt2 <- scale(burnt2[-1])

wss <- (nrow(burnt2)-1)*sum(apply(burnt2, 2, var))

for (i in 2:15) wss[i] <- sum(kmeans(burnt2, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

fit <- kmeans(burnt2, 4) #of clusters from above
              
aggregate(burnt2, by=list(fit$cluster), FUN=mean)

burnt2 <- data.frame(burnt2, fit$cluster)

library(cluster)

clusplot(burnt2, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

library(fpc)

plotcluster(burnt2, fit$cluster)