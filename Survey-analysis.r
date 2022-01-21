require(ggplot2)
library(readr)
require(FactoMineR)
require(factoextra)


# Data import and transformation into dataframe of factors
survey1 <- read_csv("~/Google Drive/My Drive/Projects/FAIR-phytoliths/Survey/R/survey1.csv")

data<-survey1[,1:64]
data<-as.data.frame(data[, 1:64])
data[, 1:64]<-sapply(data[,1:64], as.factor)
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# Create two specific datasets for MCA based on whether ppl have published or not. 
# Variables selected for analysis: Gender, Based, Research field 1,Position, 
                                #  Use of total or partial OS (for writing, analysis and visualisation)
                                #  Publishd GA (yes/no) and why
data.no.publ<-data[data$PublishedSomething == "No", c(1,2,3,5,7,12,13,18,19,24,25,27:30)]
data.publ<-data[data$PublishedSomething == "Yes", c(1,2,3,5,7,12,13,18,19,24,25,33,34,37,40)]

str(data)
summary(data)

str(data.no.publ)
summary(data.no.publ)

str(data.publ)
summary(data.publ)

plot.freq<-for (i in 1:15) {
  plot(data.no.publ[,i], main=colnames(data.no.publ)[i],
  ylab = "Count", col="steelblue", las = 2)
}

plot.freq<-for (i in 1:15) {
  plot(data.publ[,i], main=colnames(data.publ)[i],
       ylab = "Count", col="deeppink", las = 2)
}

# Perform MCA on individuals and variables
res.mca.p <- MCA(data.publ, graph = FALSE)
res.mca.np <- MCA(data.no.publ, graph = FALSE)

# Extract the proportion of variances retained by the different dimensions (axes)
eig.val.p <- get_eigenvalue(res.mca.p)
eig.val.np <- get_eigenvalue(res.mca.np)

# Visualize the percentage of inertia explained by each MCA dimensions
pi.p<-fviz_screeplot(res.mca.p, addlabels = TRUE, ylim = c(0, 45))
pi.np<-fviz_screeplot(res.mca.np, addlabels = TRUE, ylim = c(0, 45), col="deeppink", fill="deeppink")

pi.p
pi.np

# Draw the biplot of individuals (rows, blue points) and variable (columns, red triangles) categories
bip.p<-fviz_mca_biplot(res.mca.p, 
                repel = TRUE,
                ggtheme = theme_minimal())
bip.np<-fviz_mca_biplot(res.mca.np, 
                       repel = TRUE,
                       ggtheme = theme_minimal())
bip.p
bip.np

# Extract results for variable categories
var.p <- get_mca_var(res.mca.p)
var.np <- get_mca_var(res.mca.np)

# Visualize the correlation between variables and MCA principal dimensions
cor.p<-fviz_mca_var(res.mca.p, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
cor.np<-fviz_mca_var(res.mca.np, choice = "mca.cor", 
                    repel = TRUE, # Avoid text overlapping (slow)
                    ggtheme = theme_minimal())

cor.p
cor.np

# Biplot of variables only
bip.var.p<-fviz_mca_var(res.mca.p, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
bip.var.np<-fviz_mca_var(res.mca.np, 
                        repel = TRUE, # Avoid text overlapping (slow)
                        ggtheme = theme_minimal())

bip.var.p
bip.var.np

# Color by cos2 values: quality on the factor map
cos2.p<-fviz_mca_var(res.mca.p, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
cos2.np<-fviz_mca_var(res.mca.np, col.var = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                     repel = TRUE, # Avoid text overlapping
                     ggtheme = theme_minimal())

cos2.p
cos2.np

# Contributions of rows to dimension 1
row1.p<-fviz_contrib(res.mca.p, choice = "var", axes = 1, top = 15)
row1.np<-fviz_contrib(res.mca.np, choice = "var", axes = 1, top = 15,  fill="deeppink", col="deeppink")

row1.p
row1.np

# Contributions of rows to dimension 2
row2.p<-fviz_contrib(res.mca.p, choice = "var", axes = 2, top = 15)
row2.np<-fviz_contrib(res.mca.np, choice = "var", axes = 2, top = 15,  fill="deeppink", col="deeppink")

row2.p
row2.np

# Total contribution to dimension 1 and 2
row12.p<-fviz_contrib(res.mca.p, choice = "var", axes = 1:2, top = 15)
row12.np<-fviz_contrib(res.mca.np, choice = "var", axes = 1:2, top = 15,  fill="deeppink", col="deeppink")

row12.p
row12.np


# Visualize most contributing variable categories
cat.p<-fviz_mca_var(res.mca.p, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())
cat.np<-fviz_mca_var(res.mca.np, col.var = "contrib",
                    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                    repel = TRUE, # avoid text overlapping (slow)
                    ggtheme = theme_minimal())

cat.p
cat.np

# Get results by individual
ind.p <- get_mca_ind(res.mca.p)
ind.np <- get_mca_ind(res.mca.np)

p.ind.p<-fviz_mca_ind(res.mca.p, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
p.ind.np<-fviz_mca_ind(res.mca.np, col.ind = "cos2", 
                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE, # Avoid text overlapping (slow if many points)
                      ggtheme = theme_minimal())
p.ind.p
p.ind.np

 # Colour individuals by groups
gr.p<-fviz_mca_ind(res.mca.p, 
             label = "none", # hide individual labels
             habillage = "Position", # color by groups 
             palette = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#A6761D"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())
gr.np<-fviz_mca_ind(res.mca.np, 
                   label = "none", # hide individual labels
                   habillage = "Position", # color by groups 
                   palette = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02","#A6761D"),
                   addEllipses = TRUE, ellipse.type = "confidence",
                   ggtheme = theme_minimal())
gr.p
gr.np
