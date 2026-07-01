# Instalar y cargar librerías necesarias
install.packages(c("tidyverse", "corrplot", "ggplot2", "factoextra", "cluster"))

library(tidyverse)
library(corrplot)
library(ggplot2)
library(factoextra)
library(cluster)

install.packages("factoextra")
library(factoextra)

datos <- read.csv(file.choose())
datos <- datos %>% select(-id, -X)
diagnosis <- datos$diagnosis
datos_num <- datos %>% select(-diagnosis)
sum(is.na(datos_num))
datos_scaled <- scale(datos_num)
dist_matrix <- dist(datos_scaled, method = "euclidean")



# Cargar el dataset
datos <- read.csv(file.choose())

# Exploración inicial
dim(datos)
head(datos)
str(datos)

# Eliminar columnas que no usamos
datos <- datos %>% select(-id, -X)

diagnosis <- datos$diagnosis

# Dataset solo con variables numéricas
datos_num <- datos %>% select(-diagnosis)

# Verificar missing values
sum(is.na(datos_num))


#-----------------------------------------------------------------------------------------------------
summary(datos_num)
datos_mean <- datos %>% select(diagnosis, ends_with("_mean"))

datos_long <- datos_mean %>%
  pivot_longer(-diagnosis, names_to = "variable", values_to = "valor")

ggplot(datos_long, aes(x = diagnosis, y = valor, fill = diagnosis)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("B" = "#4CAF50", "M" = "#F44336")) +
  theme_minimal() +
  labs(title = "Distribución de variables de media por diagnóstico",
       x = "Diagnóstico", y = "Valor")

#--------------------------------------------------------------------------------------------------------
matriz_cor <- cor(datos_num)

corrplot(matriz_cor,
         method = "color",
         type = "upper",
         tl.cex = 0.6,
         tl.col = "black",
         title = "Matriz de correlaciones",
         mar = c(0,0,1,0))

#-------------------------------------------------------------------------------------------------------
datos_scaled <- scale(datos_num)

dist_mah <- mahalanobis(datos_scaled,
                        center = colMeans(datos_scaled),
                        cov = cov(datos_scaled))

# Umbral chi-cuadrado al 99% con 30 grados de libertad
umbral <- qchisq(0.99, df = ncol(datos_scaled))

# Número de outliers detectados
sum(dist_mah > umbral)

# Visualización
plot(dist_mah, pch = 20, col = ifelse(dist_mah > umbral, "red", "gray50"),
     main = "Distancia de Mahalanobis",
     ylab = "Distancia", xlab = "Observación")
abline(h = umbral, col = "red", lty = 2)

#------------------------------------------------------------------------------------------------------
datos_scaled_df <- as.data.frame(datos_scaled)

#---------------------------------------------------------------------------------------------------------------
# Aplicar PCA sobre datos estandarizados
pca <- prcomp(datos_num, scale. = TRUE)

# Resumen: varianza explicada por cada componente
summary(pca)

# Scree plot
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50),
         main = "Gráfico de sedimentación",
         xlab = "Componente principal",
         ylab = "Porcentaje de varianza explicada")

# Varianza acumulada
varianza_acumulada <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
varianza_acumulada

# Cargas de las dos primeras componentes
pca$rotation[, 1:2]

# Biplot
fviz_pca_biplot(pca,
                geom.ind = "point",
                col.ind = diagnosis,
                palette = c("#4CAF50", "#F44336"),
                addEllipses = TRUE,
                legend.title = "Diagnóstico",
                title = "Biplot PCA")

#-----------------------------------------------------------------------------------------------------------
# Método del codo
fviz_nbclust(datos_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Método del codo",
       x = "Número de clusters",
       y = "Suma de cuadrados intra-cluster")

# Índice de silueta
fviz_nbclust(datos_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Índice de silueta",
       x = "Número de clusters",
       y = "Silueta media")

#------------------------------------------------------------------------------------------------------
set.seed(123)
km <- kmeans(datos_scaled, centers = 2, nstart = 25)

# Tamaño de los clusters
km$size

# Visualización de clusters
fviz_cluster(km, data = datos_scaled,
             palette = c("#4CAF50", "#F44336"),
             geom = "point",
             ellipse.type = "convex",
             main = "Clusters K-means")

# Medias de cada cluster en variables originales
aggregate(datos_num, by = list(cluster = km$cluster), FUN = mean)
#--------------------------------------------------------------------------------------------------------
# Matriz de distancias
dist_matrix <- dist(datos_scaled, method = "euclidean")

# Clustering jerárquico con método de Ward
hc <- hclust(dist_matrix, method = "ward.D2")

# Dendrograma
plot(hc, main = "Dendrograma - Método de Ward",
     xlab = "Observaciones", ylab = "Altura",
     labels = FALSE, hang = -1)
rect.hclust(hc, k = 2, border = c("#4CAF50", "#F44336"))

# Cortar el dendrograma en 2 clusters
hc_clusters <- cutree(hc, k = 2)

# Tamaño de los clusters
table(hc_clusters)

#------------------------------------------------------------------------------------------------------------
# MDS métrico
mds <- cmdscale(dist_matrix, k = 2, eig = TRUE)

# Varianza explicada por las dos dimensiones
mds$eig[1:2] / sum(mds$eig[mds$eig > 0])

# Visualización MDS coloreado por diagnóstico
mds_df <- data.frame(Dim1 = mds$points[,1],
                     Dim2 = mds$points[,2],
                     diagnosis = diagnosis)

ggplot(mds_df, aes(x = Dim1, y = Dim2, color = diagnosis)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("B" = "#4CAF50", "M" = "#F44336")) +
  theme_minimal() +
  labs(title = "Escalado multidimensional (MDS)",
       x = "Dimensión 1", y = "Dimensión 2",
       color = "Diagnóstico")
#---------------------------------------------------------------------------------------------------------------
# K-means vs diagnóstico
table(km$cluster, diagnosis)

# Jerárquico vs diagnóstico
table(hc_clusters, diagnosis)
#-----------------------
summary(datos_num)
#----------------------------
# A.1 Histogramas de todas las variables
datos_long_all <- datos_num %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor")

ggplot(datos_long_all, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "#2196F3", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogramas de todas las variables",
       x = "Valor", y = "Frecuencia")
#---------------------------
# A.2 Boxplots por diagnóstico
datos_mean <- datos %>% select(diagnosis, ends_with("_mean"))
datos_long <- datos_mean %>%
  pivot_longer(-diagnosis, names_to = "variable", values_to = "valor")

ggplot(datos_long, aes(x = diagnosis, y = valor, fill = diagnosis)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("B" = "#4CAF50", "M" = "#F44336")) +
  theme_minimal() +
  labs(title = "Distribución de variables de media por diagnóstico",
       x = "Diagnóstico", y = "Valor")

#------------------------------------------
# A.3 Matriz de correlaciones
matriz_cor <- cor(datos_num)
corrplot(matriz_cor,
         method = "color",
         type = "upper",
         tl.cex = 0.6,
         tl.col = "black",
         title = "Matriz de correlaciones",
         mar = c(0,0,1,0))
#-------------------------------------------
# A.4 Gráfico de silueta
fviz_nbclust(datos_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Índice de silueta",
       x = "Número de clusters",
       y = "Silueta media")
#---------------------------------------------
# A.5 Dendrograma alternativo con enlace completo
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, main = "Dendrograma - Enlace completo",
     xlab = "Observaciones", ylab = "Altura",
     labels = FALSE, hang = -1)
rect.hclust(hc_complete, k = 2, border = c("#4CAF50", "#F44336"))
#-----------------------------------------------------
# ============================================================
# ANEXO A: GRÁFICOS COMPLEMENTARIOS
# ============================================================

# A.1 Histogramas de todas las variables
datos_long_all <- datos_num %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor")

ggplot(datos_long_all, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "#2196F3", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histogramas de todas las variables",
       x = "Valor", y = "Frecuencia")

# A.2 Boxplots por diagnóstico
datos_mean <- datos %>% select(diagnosis, ends_with("_mean"))
datos_long <- datos_mean %>%
  pivot_longer(-diagnosis, names_to = "variable", values_to = "valor")

ggplot(datos_long, aes(x = diagnosis, y = valor, fill = diagnosis)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("B" = "#4CAF50", "M" = "#F44336")) +
  theme_minimal() +
  labs(title = "Distribución de variables de media por diagnóstico",
       x = "Diagnóstico", y = "Valor")

# A.3 Matriz de correlaciones completa
matriz_cor <- cor(datos_num)
corrplot(matriz_cor,
         method = "color",
         type = "upper",
         tl.cex = 0.6,
         tl.col = "black",
         title = "Matriz de correlaciones",
         mar = c(0,0,1,0))

# A.4 Gráfico de silueta
fviz_nbclust(datos_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Índice de silueta",
       x = "Número de clusters",
       y = "Silueta media")

# A.5 Dendrograma alternativo con enlace completo
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, main = "Dendrograma - Enlace completo",
     xlab = "Observaciones", ylab = "Altura",
     labels = FALSE, hang = -1)
rect.hclust(hc_complete, k = 2, border = c("#4CAF50", "#F44336"))


# ============================================================
# B.2 Análisis exploratorio
summary(datos_num)

dist_mah <- mahalanobis(datos_scaled,
                        center = colMeans(datos_scaled),
                        cov = cov(datos_scaled))
umbral <- qchisq(0.99, df = ncol(datos_scaled))
sum(dist_mah > umbral)

plot(dist_mah, pch = 20, col = ifelse(dist_mah > umbral, "red", "gray50"),
     main = "Distancia de Mahalanobis",
     ylab = "Distancia", xlab = "Observación")
abline(h = umbral, col = "red", lty = 2)

# B.3 PCA
pca <- prcomp(datos_num, scale. = TRUE)
summary(pca)

varianza_acumulada <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
varianza_acumulada

pca$rotation[, 1:2]

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50),
         main = "Gráfico de sedimentación",
         xlab = "Componente principal",
         ylab = "Porcentaje de varianza explicada")

fviz_pca_biplot(pca,
                geom.ind = "point",
                col.ind = diagnosis,
                palette = c("#4CAF50", "#F44336"),
                addEllipses = TRUE,
                legend.title = "Diagnóstico",
                title = "Biplot PCA")

# B.4 Clustering
fviz_nbclust(datos_scaled, kmeans, method = "wss", k.max = 10) +
  labs(title = "Método del codo",
       x = "Número de clusters",
       y = "Suma de cuadrados intra-cluster")

fviz_nbclust(datos_scaled, kmeans, method = "silhouette", k.max = 10) +
  labs(title = "Índice de silueta",
       x = "Número de clusters",
       y = "Silueta media")

set.seed(123)
km <- kmeans(datos_scaled, centers = 2, nstart = 25)
km$size

fviz_cluster(km, data = datos_scaled,
             palette = c("#4CAF50", "#F44336"),
             geom = "point",
             ellipse.type = "convex",
             main = "Clusters K-means")

aggregate(datos_num, by = list(cluster = km$cluster), FUN = mean)

dist_matrix <- dist(datos_scaled, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

plot(hc, main = "Dendrograma - Método de Ward",
     xlab = "Observaciones", ylab = "Altura",
     labels = FALSE, hang = -1)
rect.hclust(hc, k = 2, border = c("#4CAF50", "#F44336"))

hc_clusters <- cutree(hc, k = 2)
table(hc_clusters)

table(km$cluster, diagnosis)
table(hc_clusters, diagnosis)

# B.5 MDS y visualizaciones
mds <- cmdscale(dist_matrix, k = 2, eig = TRUE)
mds$eig[1:2] / sum(mds$eig[mds$eig > 0])

mds_df <- data.frame(Dim1 = mds$points[,1],
                     Dim2 = mds$points[,2],
                     diagnosis = diagnosis)

ggplot(mds_df, aes(x = Dim1, y = Dim2, color = diagnosis)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("B" = "#4CAF50", "M" = "#F44336")) +
  theme_minimal() +
  labs(title = "Escalado multidimensional (MDS)",
       x = "Dimensión 1", y = "Dimensión 2",
       color = "Diagnóstico")





# Extraer varianza de las 6 primeras componentes
varianza <- summary(pca)$importance[, 1:6]
rownames(varianza) <- c("Desviación típica", 
                        "Proporción de varianza", 
                        "Varianza acumulada")

# Verlo limpio en consola
round(varianza, 4)



# Tabla 2: K-means vs diagnóstico
tabla_kmeans <- table(Cluster = km$cluster, Diagnóstico = diagnosis)
addmargins(tabla_kmeans)

# Tabla 3: Jerárquico vs diagnóstico
tabla_hc <- table(Cluster = hc_clusters, Diagnóstico = diagnosis)
addmargins(tabla_hc)