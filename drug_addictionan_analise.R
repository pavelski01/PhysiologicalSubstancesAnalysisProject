drugs_consumption_data <- read.csv("drug_consumption.data", header=FALSE)
drugs_classes <- drugs_consumption_data[14:32]
drugs_scores <- sapply(drugs_classes, substring, 3, 4)
class(drugs_scores) <- "integer"
labels <- list(
  'Alcohol', 'Amphet', 'Amyl', 
  'Benzos', 'Caff', 'Cannabis', 
  'Choc', 'Coke', 'Crack', 
  'Ecstasy', 'Heroin', 'Ketamine', 
  'Legalh', 'LSD', 'Meth', 
  'Mushrooms', 'Nicotine', 'Semer', 
  'VSA')
colnames(drugs_scores) <- labels
drugs_stats <- data.frame(
  Min = apply(drugs_scores, 2, min), # minimum
  Med = apply(drugs_scores, 2, median), # median
  Mean = apply(drugs_scores, 2, mean), # mean
  SD = apply(drugs_scores, 2, sd), # standard deviation
  Max = apply(drugs_scores, 2, max) # maximum
)
set.seed(123)
drugs_clusters10 <- kmeans(drugs_scores, 10)
pie(colSums(drugs_scores[drugs_clusters10$cluster==1,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==2,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==3,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==4,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==5,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==6,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==7,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==8,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==9,]),cex=0.9)
pie(colSums(drugs_scores[drugs_clusters10$cluster==10,]),cex=0.9)
drugs_scores <- scale(drugs_scores)
drugs_clusters <- kmeans(drugs_scores, 4, nstart = 12)
drugs_dist <- hclust(dist(drugs_scores))
plot(drugs_dist)
library("factoextra")
fviz_cluster(drugs_clusters, data = drugs_scores,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Drugs Clustering Plot"
)