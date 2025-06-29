# Wine Quality Clustering and PCA Analysis in R

This project analyzes white wine data to identify clusters of similar samples and reduce dimensionality using Principal Component Analysis (PCA). It includes data preprocessing, outlier removal, k-means clustering, and cluster validation with silhouette and Calinski-Harabasz indices.

The analysis is performed both on the original scaled data and on PCA-transformed components that capture at least 85% of the variance.

Key techniques and tools used:
- Data cleaning and scaling
- Outlier detection via z-scores
- Multiple methods to find the optimal number of clusters: NbClust, Elbow, Gap Statistic, Silhouette
- K-means clustering for different cluster counts
- Silhouette analysis to assess cluster quality
- PCA for dimensionality reduction and clustering on principal components
- Calinski-Harabasz index for cluster validation
- Visualization with ggplot2 and factoextra

Required R packages: `dplyr`, `tidyr`, `ggplot2`, `readxl`, `NbClust`, `cluster`, `factoextra`.

