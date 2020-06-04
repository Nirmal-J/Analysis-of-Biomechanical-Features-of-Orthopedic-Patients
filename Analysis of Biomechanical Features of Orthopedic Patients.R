if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(rgl)) install.packages("rgl", repos = "http://cran.us.r-project.org")
if(!require(pca3d)) install.packages("pca3d", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)

dat <- read.csv("https://raw.githubusercontent.com/christinacanavati/Analysis-of-Biomechanical-Features-of-Orthopedic-Patients/master/column_3C_weka.csv")

dat$class <- as.factor(dat$class)

#Checking the dimentions of dat

str(dat)
head(dat)

#Checking if the dataset contains missing values

map(dat, function(.x) sum(is.na(.x)))

#The count of the three classes

dat %>% ggplot(aes(class)) + 
  geom_bar(stat="count", fill = "blue", alpha = 0.5) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Distribution of Classes")

#Performing a density plot and a histogram for each of the variables to examine their distribution

fig1 <- dat %>% ggplot(aes(degree_spondylolisthesis, fill=class)) + geom_histogram(aes(y=..density..), binwidth = 10, colour="black", fill="white") + geom_density(alpha=0.5) + labs(title="degree spondylolisthesis")
fig2 <- dat %>% ggplot(aes(pelvic_tilt, fill=class)) + geom_histogram(aes(y=..density..), binwidth = 3, colour="black", fill="white") + geom_density(alpha=0.5) + labs(title="pelvic tilt")  
fig3 <- dat %>% ggplot(aes(x=lumbar_lordosis_angle, fill=class)) + geom_histogram(aes(y=..density..), binwidth = 3, colour="black", fill="white") + geom_density(alpha=0.5) + labs(title="lumbar lordosis angle")
fig4 <- dat %>% ggplot(aes(sacral_slope,fill=class)) + geom_histogram(aes(y=..density..), binwidth = 3, colour="black", fill="white") + geom_density(alpha=0.5) + labs(title = "sacral slope")
fig5 <- dat %>% ggplot(aes(pelvic_radius, fill=class)) + geom_histogram(aes(y=..density..), binwidth = 3, colour="black", fill="white") + geom_density(alpha=0.5) + labs(title="pelvic radius")
fig6 <- dat %>% ggplot(aes(x=pelvic_incidence,fill=class)) + geom_histogram(aes(y=..density..), binwidth = 3, colour="black", fill="white") + geom_density(alpha=0.5) + labs(title="Pelvic Incidence") 
grid.arrange(fig1, fig2, fig3, fig4, fig5, fig6)

#Having a closer look at fig1

fig1

#Boxplot for all the variables

b1 <- dat %>% gather(Features, Value, -class) %>% 
  ggplot(aes(Features, Value, fill = class)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) + xlab("Features") + ylim(-40, 200)

b1

#Converting dat into a matrix so we can perform scaling

dat.matrix <- dat %>% select(-class) %>% as.matrix()

#Scaling of dat.matrix by substracting the columns' means and deviding by the column's starnard deviation
dat_centered <- sweep(dat.matrix, 2, colMeans(dat.matrix))
dat_scaled <- sweep(dat_centered, 2, colSds(dat.matrix), FUN = "/")

#Building a correlation matrix

cor.matrix <- round(cor(dat.matrix), 2)

cor.matrix_metled <- melt(cor.matrix)
head(cor.matrix_metled)

# Get the lower triangle of the correlation matrix
get_lower_tri <- function(cor.matrix){
  cor.matrix[upper.tri(cormat.matrix)] <- NA
  return(cor.matrix)
}
# Get the upper triangle of the correlation matrix
get_upper_tri <- function(cor.matrix){
  cor.matrix[lower.tri(cor.matrix)]<- NA
  return(cor.matrix)
}

upper_tri <- get_upper_tri(cor.matrix)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
# Use correlation between variables as distance

reorder_cormat <- function(cor.matrix){
  dd <- as.dist((1-cor.matrix)/2)
  hc <- hclust(dd)
  cor.matrix <-cor.matrix[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cor.matrix)
upper_tri <- get_upper_tri(cor.matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(high = "orange", low = "blue",  mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.8),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 2,
                               title.position = "top", title.hjust = 0.5))

ggheatmap 


#Heatmap to view clustering

Heatmap(dat_scaled, clustering_distance_rows = "maximum", 
        clustering_method_rows = "ward.D", row_dend_width = unit(3, "cm"), 
        show_row_names = TRUE, km = 3,  gap = unit(2, "mm"),  name = "dat_scaled")

#Performing principle component analysis to analyze the clustering of the data

pca <- prcomp(dat_scaled)
summary(pca)

#Scree plot

pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per, main = "Scree Plot", xlab = "Principle Component", ylab = "Percent Variation")

#2D plot of the first two PCs

gr <- dat$class
pca2d(pca, group=gr, biplot=TRUE, biplot.vars=4, legend="topright")

#3D plot of the first three PCs

pca3d(pca, group = gr, biplot=TRUE, biplot.vars=4, legend="topright")
snapshotPCA3d(file="pca3d-plot.png")


#Boxplot of the first 5 PCs grouped by disease class

data.frame(class = dat$class, pca$x[,1:5]) %>%
  gather(key = "PC", value = "value", -class) %>%
  ggplot(aes(PC, value, fill = class)) +
  geom_boxplot()

#Partitioning the data into train and test sets

set.seed(1815)
test_index <- createDataPartition(dat$class, times = 1, p = 0.2, list = FALSE)
test_x <- dat_scaled[test_index,]
test_y <- dat$class[test_index]
train_x <- dat_scaled[-test_index,]
train_y <- dat$class[-test_index]

control <- trainControl(method = "cv", number = 10, p = .9)

#rpart model


train_rpart <-  train(train_x, train_y, method = "rpart",
                      tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), 
                      trControl = control)

#Finding the best cp
ggplot(train_rpart)

#Plotting rpart decision tree: 

class.tree <- rpart( dat$class~., dat, control = rpart.control(cp = 0.03))
rpart.plot(class.tree)

#Checking model accuracy
rpart_preds <- predict(train_rpart,  test_x)

mean(rpart_preds == test_y)

plot(varImp(train_rpart))

cm_rpart <- confusionMatrix(rpart_preds, as.factor(test_y))

#Random forest model


tuning <- data.frame(mtry = c(2, 20, 2))   
train_rf <- train(train_x, train_y,
                  method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE,
                  trControl = control)

train_rf$bestTune

rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)

#Finding the variable of most importance

varImp(train_rf)
train_rf$finalModel

plot(varImp(train_rf))

cm_rf <- confusionMatrix(rf_preds, as.factor(test_y))

#k-nearest neighnors model, tuning to find the best k

tuning <- data.frame(k = seq(3, 100, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning, 
                   trControl = control)
train_knn$bestTune

plot(train_knn)

knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)
cm_knn <- confusionMatrix(knn_preds, as.factor(test_y))

#LDA model


train_lda <- train(train_x, train_y,
                   method = "lda", trControl = control)

lda_preds <- predict(train_lda,  test_x)
mean(lda_preds == test_y)

plot(varImp(train_lda))

cm_lda <- confusionMatrix(lda_preds, factor(test_y))


#Ensemble 

models <- c("lda", "knn", "rf", "rpart")

fits <- lapply(models, function(model){ 
  print(model)
  train(train_x, train_y, method = model)
}) 

names(fits) <- models

ensemble_preds <- sapply(fits, function(object) 
  predict(object, newdata = test_x))

accuracy <- colMeans(ensemble_preds == test_y)
accuracy
acc_ensemble <- mean(accuracy)

#Table showing the overall accuracy of each model
acc_table <- data.frame(KNN = cm_knn$overall['Accuracy'], RF = cm_knn$overall['Accuracy'], LDA = cm_lda$overall['Accuracy'], rpart = cm_rpart$overall['Accuracy'], Ensemble = acc_ensemble) %>%
  gather(key= model, value = overall_accuracy) 
acc_table

#Confusion matrix list for all the models
confusionmatrix.list <- list(
  LDA=confusionMatrix(lda_preds, as.factor(test_y)),
  rpart=confusionMatrix(rpart_preds, as.factor(test_y)),
  Random_forest=confusionMatrix(rf_preds, as.factor(test_y)),
  KNN=confusionMatrix(knn_preds, as.factor(test_y))
)

lapply(confusionmatrix.list, function(x) x$byClass)

#Converting the y_hats to numeric values so the multiclass.roc function accepts the arguments

test_y.n <- as.numeric(test_y)
lda_preds.n <- as.numeric(lda_preds)
knn_preds.n <- as.numeric(knn_preds)
rf_preds.n <- as.numeric(rf_preds)
rpart_preds.n <- as.numeric(rpart_preds)

#ROC for LDA model:
par(pty = "s")
roc.multi.lda <- multiclass.roc(test_y.n, lda_preds.n)
rs1 <- roc.multi.lda[['rocs']]
plot.roc(rs1[[1]], legacy.axes = TRUE, percent = TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage")
sapply(2:length(rs1),function(i) lines.roc(rs1[[i]],col=i))

#ROC for rpart model:

roc.multi.rpart <- multiclass.roc(test_y.n, rpart_preds.n)
rs2 <- roc.multi.rpart[['rocs']]
plot.roc(rs2[[1]], legacy.axes = TRUE, percent = TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage")
sapply(2:length(rs2),function(i) lines.roc(rs2[[i]],col=i))

#ROC for rf model: 


roc.multi.rf <- multiclass.roc(test_y.n, rf_preds.n)
rs3 <- roc.multi.rf[['rocs']]
plot.roc(rs3[[1]], legacy.axes = TRUE, percent = TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage")
sapply(2:length(rs3),function(i) lines.roc(rs3[[i]],col=i))

#ROC for knn model:


roc.multi.knn <- multiclass.roc(test_y.n, knn_preds.n)
rs4 <- roc.multi.knn[['rocs']]
plot.roc(rs4[[1]], legacy.axes = TRUE, percent = TRUE, xlab = "False Positive Percentage", ylab = "True Positive Percentage")
sapply(2:length(rs4),function(i) lines.roc(rs3[[i]],col=i))

#AUC for LDA model
roc.multi.lda$auc
#AUC for rpart model
roc.multi.rpart$auc
#AUC for RF model
roc.multi.rf$auc
#AUC for KNN model
roc.multi.knn$auc

