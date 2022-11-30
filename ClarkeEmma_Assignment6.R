#QUESTION 1

# Helper packages
library(dplyr)       # for data manipulation
library(ggplot2)     # for data visualization
library(stringr)     # for string functionality
library(gridExtra)   # for manipulaiting the grid

# Modeling packages
library(tidyverse)  # data manipulation
library(cluster)     # for general clustering algorithms
library(factoextra)  # for visualizing cluster results

data("USArrests")

#To remove any missing value that might be present in the data, type this:
df <- na.omit(USArrests)

#we start by scaling/standardizing the data
df <- scale(df[c(1:4)])
head(df)

#start at 2 clusters
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

#plot the 2 clusters
fviz_cluster(k2, data = df)

#get the each clsuter's data
df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         Species = row.names(iris)) %>%
  ggplot(aes(Sepal.Length, Sepal.Width, color = factor(cluster), label = Species)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Determining Optimal Number of Clusters
set.seed(123)

#function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#or use this
fviz_nbclust(df, kmeans, method = "silhouette")

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 2
set.seed(123)
final <- kmeans(df, 2, nstart = 25)
print(final)

#final data
fviz_cluster(final, data = df)

#QUESTION 2

#QUESTION 2

# Helper packages
library(dplyr)       # for data manipulation
library(ggplot2)     # for data visualization

# Modeling packages
library(cluster)     # for general clustering algorithms
library(factoextra)  # for visualizing cluster results


us_scale <- scale(USArrests)

# center & scale the resulting columns

# For reproducibility
set.seed(123)

# Dissimilarity matrix
d <- dist(USArrests, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# For reproducibility
set.seed(123)

# Compute maximum or complete linkage clustering with agnes
hc2 <- agnes(us_scale, method = "complete")

# Agglomerative coefficient
hc2$ac
## [1] 0.926775

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(us_scale, method = x)$ac
}

# get agglomerative coefficient for each linkage method
purrr::map_dbl(m, ac)
##   average    single  complete      ward 
## 0.9139303 0.8712890 0.9267750 0.9766577

# compute divisive hierarchical clustering
hc4 <- diana(us_scale)

# Divise coefficient; amount of clustering structure found
hc4$dc
## [1] 0.9191094

# Plot cluster results
p1 <- fviz_nbclust(us_scale, FUN = hcut, method = "wss", 
                   k.max = 10) +
  ggtitle("(A) Elbow method")
p2 <- fviz_nbclust(us_scale, FUN = hcut, method = "silhouette", 
                   k.max = 10) +
  ggtitle("(B) Silhouette method")
p3 <- fviz_nbclust(us_scale, FUN = hcut, method = "gap_stat", 
                   k.max = 10) +
  ggtitle("(C) Gap statistic")

# Display plots side by side
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


hc5 <- hclust(d, method = "ward.D2" )
dend_plot <- fviz_dend(hc5)
dend_data <- attr(dend_plot, "dendrogram")
dend_cuts <- cut(dend_data, h = 8)
fviz_dend(dend_cuts$lower[[2]])

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 8)

# Number of members in each cluster
table(sub_grp)

# Plot full dendogram
fviz_dend(
  hc5,
  k = 8,
  horiz = TRUE,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = "jco",
  k_colors = "jco",
  cex = 0.1
)


dend_plot <- fviz_dend(hc5)                # create full dendogram
dend_data <- attr(dend_plot, "dendrogram") # extract plot info
dend_cuts <- cut(dend_data, h = 70.5)      # cut the dendogram at 
# designated height
# Create sub dendrogram plots
p1 <- fviz_dend(dend_cuts$lower[[1]])
p2 <- fviz_dend(dend_cuts$lower[[1]], type = 'circular')

# Side by side plots
gridExtra::grid.arrange(p1, p2, nrow = 1)



#QUESTION 3
# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for data visualization

# Modeling packages
library(mclust)   # for fitting clustering algorithms

data("USArrests")

# Apply GMM model with 3 components
us_data <- Mclust(USArrests, G = 3)

# Plot results
plot(us_data, what = "density")
plot(us_data, what = "uncertainty")

# Observations with high uncertainty
sort(us_data$uncertainty, decreasing = TRUE) %>% head()


summary(us_data)

us_data_optimal <- Mclust(data)

summary(us_data_optimal)

legend_args <- list(x = "bottomright", ncol = 5)
plot(us_data_optimal, what = 'BIC', legendArgs = legend_args)
plot(us_data_optimal, what = 'classification')
plot(us_data_optimal, what = 'uncertainty')


probabilities <- probabilities %>%
  as.data.frame() %>%
  mutate(id = row_number()) %>%
  tidyr::gather(cluster, probability, -id)

ggplot(probabilities, aes(probability)) +
  geom_histogram() +
  facet_wrap(~ cluster, nrow = 2)

uncertainty <- data.frame(
  id = 1:nrow(my_basket),
  cluster = my_basket_mc$classification,
  uncertainty = my_basket_mc$uncertainty
)

uncertainty %>%
  group_by(cluster) %>%
  filter(uncertainty > 0.25) %>%
  ggplot(aes(uncertainty, reorder(id, uncertainty))) +
  geom_point() +
  facet_wrap(~ cluster, scales = 'free_y', nrow = 1)


cluster2 <- my_basket %>%
  scale() %>%
  as.data.frame() %>%
  mutate(cluster = my_basket_mc$classification) %>%
  filter(cluster == 2) %>%
  select(-cluster)

cluster2 %>%
  tidyr::gather(product, std_count) %>%
  group_by(product) %>%
  summarize(avg = mean(std_count)) %>%
  ggplot(aes(avg, reorder(product, avg))) +
  geom_point() +
  labs(x = "Average standardized consumption", y = NULL)


