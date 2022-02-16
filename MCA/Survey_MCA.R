# Helpful tutorials
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
# http://factominer.free.fr/factomethods/multiple-correspondence-analysis.html
# https://www.researchgate.net/post/How_to_handle_Missing_Values_for_Skipped_Questions_Systematic_Missing_in_R

# Load packages
require(ggplot2)
library(readr)
require(FactoMineR)#
require(factoextra)#
library(dplyr)#
library(ggcorrplot)#

# Data import and transformation into dataframe of factors
survey<-read.table("Survey_MCA.csv", sep=";", header=TRUE, row.names=1)

# MCA
res.mca<-MCA(survey, graph=FALSE)
eig.val<-get_eigenvalue(res.mca)
pi<-fviz_screeplot(res.mca, addlabels=TRUE, ylim=c(0, 45))
pi
bip<-fviz_mca_biplot(res.mca, 
                       repel=TRUE,
                       col.var="grey35",
                       #habillage="Based", #color by groups 
                       #addEllipses=TRUE, ellipse.type="confidence",
                       ggtheme=theme_minimal())
bip

#Correlation analysis
model.matrix(~0+., data=survey) %>%
  cor(use="all") %>%
  ggcorrplot(show.diag=F, type="lower", lab=TRUE, lab_size=2)

#####Barplots
survey.publ<-survey[survey$Publications=="Yes",]
survey.nopubl<-survey[survey$Publications=="No",]

#Barplot open-access software (with publications)
ggplot(data=survey.publ)+
  geom_bar(aes(x=OpenSoftware, fill=Position, color=Field), size=1.25)+
  facet_grid(~Based)+
  scale_fill_manual(values=c("grey80","grey50","grey20"))+
  scale_color_brewer(palette="Set1")+
  labs(title="Participants with publications: use of open-access software",x="Use of open-access software",y="Participants",
       fill="Position",color="Research field")+
  theme_bw()

#Barplot open-access software (without publications)
ggplot(data=survey.nopubl)+
  geom_bar(aes(x=OpenSoftware, fill=Position, color=Field), size=1.25)+
  facet_grid(~Based)+
  scale_fill_manual(values=c("grey80","grey50","grey20"))+
  scale_color_brewer(palette="Set1")+
  labs(title="Participants without publications: use of open-access software",x="Use of open-access software",y="Participants",
       fill="Position",color="Research field")+
  theme_bw()

#Barplot willingness to preprint/green (with publications)
ggplot(data=survey.publ)+
  geom_bar(aes(x=PreprintGreen, fill=Position, color=Field), size=1.25)+
  facet_grid(~Based)+
  scale_fill_manual(values=c("grey80","grey50","grey20"))+
  scale_color_brewer(palette="Set1")+
  labs(title="Participants with publications: willingness to publish preprint/green access",x="Willingness to publish preprint/green access",y="Participants",
       fill="Position",color="Research field")+
  theme_bw()

#Barplot willingness to preprint/green (without publications)
ggplot(data=survey.nopubl)+
  geom_bar(aes(x=PreprintGreen, fill=Position, color=Field), size=1.25)+
  facet_grid(~Based)+
  scale_fill_manual(values=c("grey80","grey50","grey20"))+
  scale_color_brewer(palette="Set1")+
  labs(title="Participants without publications: willingness to publish preprint/green access",x="Willingness to publish preprint/green access",y="Participants",
       fill="Position",color="Research field")+
  theme_bw()





