# Machine Learning and Qualitative Research
# Marla Stuart (marlastuart@berkeley.edu)
# March 13, 2019

# This code provides a sample of using machine learning to identify 
# themes (clusters) in coded qualitative data.

# PREPARE ENVIRONMENT ##########################################################
# Clear the environment
rm(list=ls())

# set the working directory
setwd("") 

# load the data
# this is a the dataset that I already wrangled and wrote out from my
# data wrangling code file
data <- read.csv("josue.csv", header=TRUE)
dim(data)
# 83 x 14
names(data)
str(data)
data_original <- data

# PREPARE DATA ##########################################################

# standardize all values because clustering is sensitive to wide variations
test <- round(data[, 3:14]/data$Segments, 3)
head(test)

# check that it is accurate -- sum by row
test$sum <- rowSums(test)
summary(test$sum)
# off by hundredths of percents due to rounding above

# ok looks good -- replace the data with this
data <- test[, 1:13]
names(data)
rm(test)

# check if there are missing
table(is.na(data))
# no missing

# delete the sum var
data$sum <- NULL

# VISUALIZE THE DATA ###########################################################
# small multiple of each code

dev.off()
# pdf("data", width = 11, height = 8.5) 
par(mfrow = c(3,3))

# mean = .27
hist(data$Heteronormativity, main = "Heteronormativity",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Heteronormativity), 2), cex = 2)

# mean = .16
hist(data$Racism, main = "Racism",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Racism), 2), cex = 2)

# mean = .15
hist(data$Ethnocentrism, main = "Ethnocentrism",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Ethnocentrism), 2), cex = 2)

# mean = .14
hist(data$Sexism, main = "Sexism",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Sexism), 2), cex = 2)

# mean = .11
hist(data$Intersectionality, main = "Intersectionality",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Intersectionality), 2), cex = 2)

# mean = .09
hist(data$Practice, main = "Practice.Recommendations",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Practice), 2), cex = 2)

# mean = .05
hist(data$Classism, main = "Classism",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Classism), 2), cex = 2)

# mean = .01
hist(data$Microaggression, main = "Microagression",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Microaggression), 2), cex = 2)

# mean = .01
hist(data$Cisgenderism, main = "Cisgenderism",
     xlab = "Proportion of Codes", ylab = "Number of Articles",
     xlim = c(0, 1), ylim = c(1,80), cex.main = 2)
text(x = .7, y = 10, "m = ", cex = 2)
text(x = .8, y = 10, round(mean(data$Cisgenderism), 2), cex = 2)

# HOW MANY CLUSTERS? ##########################################################
# how many clusters?
library(cluster)

set.seed(1234)
nc <- NbClust(data, diss = NULL, distance = "euclidean", 
              min.nc = 2, max.nc = 20, method = "average",
              index = "all", alphaBeale = .1)

# best is 6

# test whether there are or are not real clusters
dev.off()
plot(nc$All.index[,4], type = "o", ylab = "CCC", xlab = "Number of Clusters")
# interpretation -- if it is all decreasing , there are likely no clusters

# MACHINE LEARNING #############################################################
library(cluster)

# create the distance measure
# read Kabacoff to decide which distance measure you need to use
d <- dist(data, method = "euclidean")

##############################################################
# create hierarchical  clusters
# read the help for hclust to decide which method you need to use
fit_h <- hclust(d, method = "average")
dev.off()
plot(fit_h, hang = -1, cex = .8)
# visualizte the clusters
rect.hclust(fit_h, k=2)
rect.hclust(fit_h, k=3)
rect.hclust(fit_h, k=4)
rect.hclust(fit_h, k=5)
rect.hclust(fit_h, k=6)

# examine the clusters
clusters_h2 <- cutree(fit_h, 2)
clusters_h3 <- cutree(fit_h, 3)
clusters_h4 <- cutree(fit_h, 4)
clusters_h5 <- cutree(fit_h, 5)
clusters_h6 <- cutree(fit_h, 6)

table(clusters_h6)


################################################
# create k=means clusters
set.seed(1234)
fit_km_2 <- kmeans(data, 2, nstart = 12, iter.max = 100)
fit_km_3 <- kmeans(data, 3, nstart = 12, iter.max = 100)
fit_km_4 <- kmeans(data, 4, nstart = 12, iter.max = 100)
fit_km_5 <- kmeans(data, 5, nstart = 12, iter.max = 100)
fit_km_6 <- kmeans(data, 6, nstart = 12, iter.max = 100)

# look at the freq for each cluster
fit_km_2$size
fit_km_3$size
fit_km_4$size
fit_km_5$size
fit_km_6$size

# look at the center or most common observation in each cluster
fit_km_6$centers

# Visualize k-means clusters
library(factoextra)
fviz_cluster(fit_km_2, data = data, geom = "point", frame.type = "norm", ellipse = T)
fviz_cluster(fit_km_3, data = data, geom = "point", frame.type = "norm", ellipse = T)
fviz_cluster(fit_km_4, data = data, geom = "point", frame.type = "norm", ellipse = F)
fviz_cluster(fit_km_5, data = data, geom = "point", frame.type = "norm", ellipse = T)
fviz_cluster(fit_km_6, data = data, geom = "point", frame.type = "norm", ellipse = F)

#######################################################
# create partitioning around medoids (PAM)
set.seed(1234)
fit_pam_2 <- pam(data, k = 2, stand = F)
fit_pam_3 <- pam(data, k = 3, stand = F)
fit_pam_4 <- pam(data, k = 4, stand = F)
fit_pam_5 <- pam(data, k = 5, stand = F)
fit_pam_6 <- pam(data, k = 6, stand = F)

# look at the medoid observation
fit_pam_6$medoids

# plot it
dev.off
clusplot(fit_pam_2, main = "Bivariate Cluster Plot", labels = 3, span = T)
clusplot(fit_pam_3, main = "Bivariate Cluster Plot", labels = 3, span = T)
clusplot(fit_pam_4, main = "Bivariate Cluster Plot", labels = 3, span = T)
clusplot(fit_pam_5, main = "Bivariate Cluster Plot", labels = 3, span = T)
clusplot(fit_pam_6, main = "Bivariate Cluster Plot", labels = 3, span = T)

# look at clusters distribution
table(fit_pam_6$clustering)

# look at fit statistics
fit_pam_6$clusinfo

######################################################33
# add the clusters to the data
data_c <- cbind(data, clusters_h6, fit_km_6$cluster, fit_pam_6$clustering)
head(data_c)
names(data_c)[13:15] <- c("H6", "KM6", "PAM6")
names(data_c)

# compare the means of each code by each cluster
library(compareGroups)
groups_table_h <- compareGroups(H6 ~ .,
                                data = data_c[, c(1:13)], bivar = T, byrow = T,
                                chisq.test.perm = T, max.ylev = 6)

groups_table_h <- createTable(groups_table_h, hide.no = c("no"), type =  1, 
                              show.all = F)       
groups_table_h
export2word(groups_table_h, file = "groups_table_h.doc")

# only macro and classism not sig difference
# so only show hierarchical

groups_table_p <- compareGroups(PAM6 ~ .,
                                data = data_c, bivar = T, byrow = T,
                                chisq.test.perm = T, max.ylev = 6)

groups_table_p <- createTable(groups_table_p, hide.no = c("no"), type =  1, 
                              show.all = F)       
groups_table_p
export2word(groups_table_p, file = "groups_table_p.doc")

# Not sig = lots 


groups_table_k <- compareGroups(KM6 ~ .,
                                data = data_c, bivar = T, byrow = T,
                                chisq.test.perm = T, max.ylev = 6)

groups_table_k <- createTable(groups_table_k, hide.no = c("no"), type =  1, 
                              show.all = F)       
groups_table_k
export2word(groups_table_k, file = "groups_table_k.doc")

# a lot of insignificant differences here

###############################################################################
# Make small multiples for each hierarchial
names(data_c)
data_c <- data_c[, c(8, 11, 7, 10, 1, 12, 9, 3, 6, 4, 2, 5, 13, 14, 15)]
names(data_c)

dev.off()
par(mar=c(10, 2.5, 2.5, 2.5)) # set the plot margins
par(mfrow = c(2,3))
barplot(colMeans(data_c[data_c$H6 == 1, c(1:12)]),
        main = "Cluster 1", las=2, cex.main = 2, ylim = c(0, .50))
barplot(colMeans(data_c[data_c$H6 == 2, c(1:12)]),
        main = "Cluster 2", las=2, cex.main = 2, ylim = c(0, .50))
barplot(colMeans(data_c[data_c$H6 == 3, c(1:12)]),
        main = "Cluster 3", las=2, cex.main = 2, ylim = c(0, .50))
barplot(colMeans(data_c[data_c$H6 == 4, c(1:12)]),
        main = "Cluster 4", las=2, cex.main = 2, ylim = c(0, .50))
barplot(colMeans(data_c[data_c$H6 == 5, c(1:12)]),
        main = "Cluster 5", las=2, cex.main = 2, ylim = c(0, .50))
barplot(colMeans(data_c[data_c$H6 == 6, c(1:12)]),
        main = "Cluster 6", las=2, cex.main = 2, ylim = c(0, .50))
dev.off()


# Randomly select an article from each cluster
data_cr <- cbind(data_original$Document, data_c)
names(data_cr)
library(dplyr)

set.seed(1234)
sample_n(data_cr[data_cr$H6 == 1, ], 3)
# 20, 59, 58
set.seed(1234)
sample_n(data_cr[data_cr$H6 == 2, ], 3)
# 6, 46, 44
set.seed(1234)
sample_n(data_cr[data_cr$H6 == 3, ], 3)
# 8, 43, 41
set.seed(1234)
sample_n(data_cr[data_cr$H6 == 4, ], 3)
# 15, 49, 64
set.seed(1234)
sample_n(data_cr[data_cr$H6 == 5, ], 3)
# 14, 40, 74
set.seed(1234)
sample_n(data_cr[data_cr$H6 == 6, ], 1)
# 60

data_cr[sample(nrow(data[data_cr$H6 == 1, ]), 3), ]
# row 2, 9, 8
set.seed(1234)
data_cr[sample(nrow(data[data_cr$H6 == 2, ]), 3), ]
# row 10
set.seed(1234)
data_cr[sample(nrow(data[data_cr$H6 == 3, ]), 3), ]
# row 7
set.seed(1234)
data_cr[sample(nrow(data[data_cr$H6 == 4, ]), 3), ]
# row 9
set.seed(1234)
data_cr[sample(nrow(data[data_cr$H6 == 5, ]), 3), ]
# row 17
set.seed(1234)
data_cr[sample(nrow(data[data_cr$H6 == 6, ]), 3), ]
# row 8
