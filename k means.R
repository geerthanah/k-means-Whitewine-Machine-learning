
#Sub Task 1

#a.

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# Load the wine dataset
wine_uncleaned = read_excel("C:/Users/geert/Desktop/ML cw/Whitewine_v6.xlsx")

#Data pre-processing

# Check for missing values
missing_values = sum(is.na(wine_uncleaned))
print(missing_values)

# Remove duplicates
wine = distinct(wine_uncleaned)

# Define the response variable column name
response_variable = "quality"  

# Extract only continuous predictors
continuous_predictors = c("fixed acidity", "volatile acidity", "citric acid",
                           "residual sugar", "chlorides", "free sulfur dioxide",
                           "total sulfur dioxide", "density", "pH",
                          "sulphates", "alcohol")

# Perform scaling using standard scaler
scaled_data_bo = scale(wine[, continuous_predictors])

View(scaled_data_bo)
head(scaled_data_bo)

#Visulalizing distributions of predictors - Before Outlier Analysis
before_outliers = lapply(continuous_predictors, function(col) {
  ggplot(data = as.data.frame(scaled_data_bo) , aes(x = "", y = .data[[col]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = paste("Boxplot of", col, "before removing outliers")) +
    theme_minimal()
})
before_outliers

# Calculate the z-scores for each column of the scaled data
z_scores = apply(scaled_data_bo, 2, function(x) abs(scale(x)))

# Identify outliers by checking if any z-score exceeds the threshold (3)
outliers = rowSums(z_scores > 3) > 0

# Remove rows containing outliers from the scaled data
scaled_data = scaled_data_bo[!outliers, ]


#Visulalizing distributions of predictors - After Outlier Analysis
after_outliers = lapply(continuous_predictors, function(col) {
  ggplot(data = as.data.frame(scaled_data), aes(x = "", y = .data[[col]])) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = paste("Boxplot of", col, "after removing outliers")) +
    theme_minimal()
})
after_outliers

View(scaled_data)
head(scaled_data)

#########################################################################
#b.

# Load required libraries
library(NbClust)
library(cluster)
library(factoextra)

# NBclust Method

nb_clusters = NbClust(scaled_data, distance = "euclidean",
                       min.nc = 2, max.nc = 10, method = "kmeans")
print(nb_clusters$Best.nc[1])



# Elbow Method

#Method 1
wss = numeric(10)
for (i in 1:10) {
  kmeans_model = kmeans(scaled_data, centers = i)
  wss[i] = sum(kmeans_model$withinss)
}

plot(1:10, wss[1:10], type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters",
     ylab = "Within cluster sum of squares")

#Method 2 using factoextra library
fviz_nbclust(scaled_data, kmeans, method = "wss")




#Gap statistics Method

gap_stat = clusGap(scaled_data, FUN = kmeans, nstart = 25,
                    K.max = 10 , B = 50)

print(gap_stat)

fviz_gap_stat(gap_stat)

#silhouette method

#Method 1
sil_scores = numeric(10)
for (i in 2:10) {
  kmeans_model = kmeans(scaled_data, centers = i)
  sil = silhouette(kmeans_model$cluster, dist(scaled_data))
  sil_scores[i] = mean(sil[, "sil_width"])
}

plot(2:10, sil_scores[2:10], type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters", 
     ylab = "Average silhouette width")

#Method 2
fviz_nbclust(scaled_data, kmeans, method='silhouette')

################################################################################

#c.

optimal_k = 2 

# Perform k-means clustering with the optimal number of clusters
kmeans_model = kmeans(scaled_data, centers = optimal_k)

# Display k-means output
print(kmeans_model)

# Ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_Squares (TSS)
BSS = kmeans_model$betweenss
TSS = kmeans_model$totss
BSS_TSS_ratio = BSS / TSS

# Calculate within_cluster_sums_of_squares (WSS)
WSS = sum(kmeans_model$withinss)

# Calculated BSS, WSS, and BSS/TSS ratio
cat("Ratio of BSS to TSS:", BSS_TSS_ratio, "\n")
cat("Between Cluster Sum of Squares (BSS):", BSS, "\n")
cat("Within Cluster Sum of Squares (WSS):", WSS, "\n")

# Perform k-means clustering for k=3
kmeans_model_k3 = kmeans(scaled_data, centers = 3)

# Display k-means output for k=3
print(kmeans_model_k3)

# Ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_Squares (TSS)
BSS_k3 = kmeans_model_k3$betweenss
TSS_k3 = kmeans_model_k3$totss
BSS_TSS_ratio_k3 = BSS_k3 / TSS_k3

# Calculate within_cluster_sums_of_squares (WSS)
WSS_k3 = sum(kmeans_model_k3$withinss)

# Calculated BSS, WSS, and BSS/TSS ratio for k=3
cat("Ratio of BSS to TSS for k=3:", BSS_TSS_ratio_k3, "\n")
cat("Between Cluster Sum of Squares (BSS) for k=3:", BSS_k3, "\n")
cat("Within Cluster Sum of Squares (WSS) for k=3:", WSS_k3, "\n")

#Visualizing the cluster
fviz_cluster(kmeans(scaled_data,centers = 2,iter.max = 100,nstart = 100), data = scaled_data)
fviz_cluster(kmeans(scaled_data,centers = 3,iter.max = 100,nstart = 100), data = scaled_data)


######################################################################################

#d.

# Calculate silhouette scores for k=2
silh = silhouette(kmeans_model$cluster, dist(scaled_data))

# Plot silhouette plot
silhouette_plot = fviz_silhouette(silh)

# Display silhouette plot
print(silhouette_plot)

# Calculate average silhouette width
avg_silhouette_width = mean(silh[, "sil_width"])
print(avg_silhouette_width)

# Discussion on silhouette plot
cat("The average silhouette width score is:", avg_silhouette_width, "\n")
cat("",avg_silhouette_width, "- A value close to 0 indicates 
    overlapping clusters or  clusters with similar distances to 
    neighboring clusters.\n")


# Calculate silhouette scores for k=3
silh_k3 <- silhouette(kmeans_model_k3$cluster, dist(scaled_data))

# Plot silhouette plot for k=3
silhouette_plot_k3 <- fviz_silhouette(silh_k3)

# Display silhouette plot for k=3
print(silhouette_plot_k3)

# Calculate average silhouette width for k=3
avg_silhouette_width_k3 <- mean(silh_k3[, "sil_width"])

# Discussion on silhouette plot for k=3
cat("For k=3:")
cat("The average silhouette width score is:", avg_silhouette_width_k3, "\n")
cat("",avg_silhouette_width_k3, " - A value close to 0 indicates 
    overlapping clusters or clusters with similar distances 
    to neighboring clusters.\n")


#####################################################################################
#####################################################################################

#Sub Task 2

#e. 
# Perform PCA analysis
pca_result = prcomp(scaled_data)

# Display PCA results
print(summary(pca_result))

# Eigenvalues and eigenvectors
eigenvalues = pca_result$sdev^2
eigenvalues

eigenvectors = pca_result$rotation
eigenvectors

# Cumulative score per principal components (PC)
cumulative_variance = cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
cumulative_variance

# Create a plot of the explained variance by each principal component
fviz_eig(pca_result)

# Create a plot of the correlation between variables and principal components
fviz_pca_var(pca_result)

# Identify the number of principal components with cumulative score > 85%
selected_pcs = which(cumulative_variance > 0.85)[1]

# Brief discussion for the choice of PCs
cat("The number of principal components explaining at least 85% of the variance:", selected_pcs, "\n")
cat("By selecting these principal components, we retain most of the variance in the data while reducing its dimensionality.\n")
cat("This allows us to simplify the analysis while preserving the most important information.\n")

# Create a new "transformed" dataset with selected principal components as attributes
transformed_data = as.data.frame(pca_result$x[, 1:selected_pcs])

# Print transformed dataset with selected PCs
head(transformed_data)


##############################################################################

#f.

# NBclust method

nb_clusters_2 = NbClust(transformed_data, distance = "euclidean",
                      min.nc = 2, max.nc = 10, method = "kmeans")

print(nb_clusters_2)


# Elbow method

fviz_nbclust(transformed_data, kmeans, method = "wss")



#Gap statistics method

gap_stat_2 = clusGap(transformed_data, FUN = kmeans, nstart = 25,
                   K.max = 10 , B = 50)

print(gap_stat_2)

fviz_gap_stat(gap_stat_2)

#silhouette method

fviz_nbclust(transformed_data, kmeans, method='silhouette')

################################################################################

#g.

optimal_k_2 = 2 

# Perform k-means clustering with the optimal number of clusters
kmeans_model_2 = kmeans(transformed_data, centers = optimal_k_2)

# Display k-means output
print(kmeans_model_2)

# Ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_Squares (TSS)
BSS_2 = kmeans_model_2$betweenss
TSS_2 = kmeans_model_2$totss
BSS_TSS_ratio_2 = BSS_2 / TSS_2

# Calculate within_cluster_sums_of_squares (WSS)
WSS_2 = sum(kmeans_model_2$withinss)

# Calculated BSS, WSS, and BSS/TSS ratio
cat("Ratio of BSS to TSS:", BSS_TSS_ratio_2, "\n")
cat("Between Cluster Sum of Squares (BSS):", BSS_2, "\n")
cat("Within Cluster Sum of Squares (WSS):", WSS_2, "\n")

# Perform k-means clustering for k=3
kmeans_model_3 = kmeans(transformed_data, centers = 3)

# Display k-means output for k=3
print(kmeans_model_3)

# Ratio of between_cluster_sums_of_squares (BSS) over total_sum_of_Squares (TSS)
BSS_3 = kmeans_model_3$betweenss
TSS_3 = kmeans_model_3$totss
BSS_TSS_ratio_3 = BSS_3 / TSS_3

# Calculate within_cluster_sums_of_squares (WSS)
WSS_3 = sum(kmeans_model_3$withinss)

# Calculated BSS, WSS, and BSS/TSS ratio for k=3
cat("Ratio of BSS to TSS for k=3:", BSS_TSS_ratio_3, "\n")
cat("Between Cluster Sum of Squares (BSS) for k=3:", BSS_3, "\n")
cat("Within Cluster Sum of Squares (WSS) for k=3:", WSS_3, "\n")

#Visualizing the cluster
fviz_cluster(kmeans(transformed_data,centers = 2,iter.max = 100,nstart = 100), data = transformed_data)
fviz_cluster(kmeans(transformed_data,centers = 3,iter.max = 100,nstart = 100), data = transformed_data)


######################################################################################

#h.

# Calculate silhouette scores
silh_2 = silhouette(kmeans_model_2$cluster, dist(transformed_data))

# Plot silhouette plot
silhouette_plot_2 = fviz_silhouette(silh_2)

# Display silhouette plot
print(silhouette_plot_2)

# Calculate average silhouette width
avg_silhouette_width_2 = mean(silh_2[, "sil_width"])
print(avg_silhouette_width_2)

# Discussion on silhouette plot
cat("The average silhouette width score is:", avg_silhouette_width_2, "\n")
cat("",avg_silhouette_width_2, "- A value close to 0 indicates 
    overlapping clusters or  clusters with similar distances to 
    neighboring clusters.\n")

##############################################################################################

#i.
# Calculate Calinski-Harabasz index
CH_index = (BSS_2 / (optimal_k_2 - 1)) / (WSS_2 / (nrow(transformed_data) - optimal_k_2))

# Display the Calinski-Harabasz Index
print(CH_index)

# Calculate Calinski-Harabasz index for 2-10 clusters
CH_index <- numeric(length = 9) # Create a numeric vector to store CH index values
for (k in 2:10) {
  CH_index[k-1] <- (BSS_2 / (k - 1)) / (WSS_2 / (nrow(transformed_data) - k))
}

# Display the Calinski-Harabasz Index for 2-10 clusters
print(CH_index)

# Plot the Calinski-Harabasz index values
plot(x = 2:10, y = CH_index, type = "b", xlab = "Number of clusters", ylab = "Calinski-Harabasz index")

##############################################################################################

