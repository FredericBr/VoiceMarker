rm(list=ls())
chemin1="C:/Users/briend/Documents/Tours/Recherche/PROVAU_kmeans_test/Figure_article/"
setwd(chemin1)

library(wesanderson)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(caret)
library(ggplot2)
library(gridExtra)
library(gtools)
library(cowplot)
library(ggpubr)

##Fig. 2c. Classification of autistic children with respect to a diverse control population
Data <- read.table("C:/Users/briend/Documents/Tours/Recherche/PROVAU_kmeans_test/Resultats_2020_2021_final.csv",sep=';',row.names = 1,strip.white=TRUE,header=T,fill=TRUE)
set.seed(154)
#Datafinal=Data[Data$final=="yes",] #COMMENTER SI TEST 4
Datafinal=Data # SI TEST 4 = ne pas COMMENTER
Datafinal=Datafinal[Datafinal$REJECT==0,] #1 = rejete

training.samples <- Datafinal$Group %>%
        createDataPartition(p = 0.31, list = FALSE) #p=0.2, 0.4, 0.6, 0.8
test.data  <- Datafinal[training.samples, ]
train.data <- Datafinal[-training.samples, ]
Databis=train.data

Best_AUC=c("avg_F1","shimmer")
Combi=combinations(n=2, r=2, v=Best_AUC)

# names(Databis)[names(Databis) == 'avg_F1'] <- 'F1'
# names(Databis)[names(Databis) == 'shimmer'] <- 'Shimmer'
# names(Databis)[names(Databis) == 'shimmer_k'] <- 'k(Shimmer)'

Databis$Group_final=as.factor(Databis$Group_final)
Databis$Group=as.factor(Databis$Group)

p1=ggplot(Databis, aes_string(Combi[1,1], Combi[1,2],color = "Group")) +
        geom_point(shape = 16, size = 3) +xlab("F1")+ylab("Shimmer") +
        #theme(plot.margin = unit(c(20,0,20,0), "pt")) +
        scale_color_manual(values=c("#FF4E28", "#28FFAE", "#0CC17D", "#20976B"))

prow <- plot_grid( p1 + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("A"),
                   hjust = -1,
                   nrow = 1)

legend_b <- get_legend(p1 + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
#windows(12,5)
tiff("Figure2c.tiff",  width = 4, height = 4, units = 'in', res = 300)
p
dev.off()

# # Extended Fig. 1. Number of bootstraps for K-means clustering Analysis decided by convergence criteria. 
# # Supervised
# n=c(2e+00,5e+00,1e+01,5e+01,7.5e+01,1e+02,2.5e+02,5e+02,7.5e+02,1e+03)
# se_S=c(0.875,0.89,0.895,0.844,0.844,0.843,0.846,0.845,0.842,0.843)
# se_S_sd=c(0.035,0.065,0.064,0.086,0.085,0.081,0.079,0.077,0.078,0.077)
# sp_S=c(0.652,0.738,0.719,0.813,0.821,0.816,0.811,0.814,0.811,0.810)
# sp_S_sd=c(0.058,0.108,0.098,0.121,0.122,0.120,0.113,0.115,0.114,0.119)
# 
# ## Unsupervised
# se_US=c(0.775,0.79,0.810,0.774,0.787,0.781,0.779,0.781,0.783,0.784)
# se_US_sd=c(0.176,0.096,0.077,0.092,0.092,0.087,0.084,0.080,0.080,0.079)
# sp_US=c(0.750,0.755,0.697,0.713,0.708,0.708,0.711,0.720,0.718,0.718)
# sp_US_sd=c(0.039,0.030,0.128,0.124,0.120,0.128,0.125,0.116,0.115,0.116)
# 
# se=c(se_S,se_US)
# se_sd=c(se_S_sd,se_US_sd)
# sp=c(sp_S,sp_US)
# sp_sd=c(sp_S_sd,sp_US_sd)
# 
# se_lo=se-se_sd
# se_up=se+se_sd
# sp_lo=sp-sp_sd
# sp_up=sp+sp_sd
# n_rep=rep(n,4)
# value=c(se,sp)
# value_lo=c(se_lo,sp_lo)
# value_up=c(se_up,sp_up)
# Type=c(rep("Sensitivity Supervised",10),rep("Specificity Supervised",10),rep("Sensitivity Unsupervised",10),rep("Specificity Unsupervised",10))
# 
# test_data_S <-data.frame(n_rep,value,value_lo,value_up,type)
# 
# #windows(15,10)
# tiff("SFigure1.tiff", width= 2404, height= 1600, units="px", res=300)
# ggplot(data=test_data_S, aes(x=n_rep, y=value, ymin=value_lo,
#                              ymax=value_up, fill=Type, linetype=Type)) + 
#         geom_line(aes(color=Type),size=1.15) +theme_minimal()+ 
#         geom_ribbon(alpha=0.15) + scale_x_log10() + ylim(0.5, 1)+
#         xlab("log10(Number of bootstraps performed)")+ylab("Sensitivity and Specificity")+
#         geom_vline(xintercept = 500, linetype="dotted", color = "red", size=1)
# dev.off()

#Fig. 1c. Classification of autistic children with respect to a diverse control population. 
# Supervised
library(fmsb)
library(data.table)
Data <- read.table("C:/Users/briend/Documents/Tours/Recherche/PROVAU_kmeans_test/Resultats_2020_2021_final.csv",sep=';',row.names = 1,strip.white=TRUE,header=T,fill=TRUE)
n_ASD_Data=length(Data$Group_final[Data$Group=="ASD"])
n_TD_Data=length(Data$Group_final[Data$Group=="TD"])
Data=Data[Data$Group=="TD"|Data$Group=="ASD",]
set.seed(154)

Datafinal=Data[Data$final=="yes",] #COMMENTER SI TEST 4
Datafinal=Datafinal[Datafinal$REJECT==0,] #1 = rejete

training.samples <- Datafinal$Group %>%
        createDataPartition(p = 0.31, list = FALSE) #p=0.2, 0.4, 0.6, 0.8 0.31
test.data  <- Datafinal[training.samples, ]
train.data <- Datafinal[-training.samples, ]

#choix groupe pour choix 
Databis=train.data

#Definition
Group<- Databis[,2]
Databis<- Databis[,-1] #remove participant column
subgroup<- Databis[,1]
Databis<- Databis[,-1] #remove group column
Databis<- Databis[,-1] #remove group_final
Databis<- Databis[,-1] #remove model
Databis<- Databis[,c(-1,-2)] #remove "REJECT","ErrorsROCKCA"

df <- scale(Databis) #standardiser (centrage réduction) les données
df= df[,c(2,6,8,17)]
#pour moyenne sur Figure
xdf <- as.data.frame(df, stringsAsFactors = FALSE)
xdf=cbind(xdf,Group)

df_TSA=xdf[xdf$Group=="ASD",];df_TSA<- df_TSA[,-5] #remove Group
df_TD=xdf[xdf$Group=="TD",];df_TD<- df_TD[,-5] #remove Group

averag_ASD <- data.frame(avg_F1 = mean(df_TSA$avg_F1),
                         mean_hnr = mean(df_TSA$mean_hnr),
                         shimmer = mean(df_TSA$shimmer),
                         jitter_s = mean(df_TSA$jitter_s))
rownames(averag_ASD) <- "Average"
averag_TD <- data.frame(avg_F1 = mean(df_TD$avg_F1),
                         mean_hnr = mean(df_TD$mean_hnr),
                         shimmer = mean(df_TD$shimmer),
                         jitter_s = mean(df_TD$jitter_s))
rownames(averag_ASD) <- "Average_ASD"
rownames(averag_TD) <- "Average_TD"
# Rattacher les plages de variables aux données
df_TSA <- rbind(df_TSA,averag_ASD)
df_TD <- rbind(df_TD,averag_TD)
#pour graph
df_TSA <- df_TSA*-1
df_TD <- df_TD*-1
# Définir les intervalles des variables : maximum et minimum
max_min <- data.frame(
        avg_F1 = c(-4, 4), mean_hnr = c(-4, 4), shimmer = c(-4, 4),jitter_s = c(-4, 4))
rownames(max_min) <- c("Max", "Min")

# Rattacher les plages de variables aux données
df_TSA <- rbind(max_min, df_TSA)
df_TD <- rbind(max_min, df_TD)

setnames(df_TSA, old = c("avg_F1", "mean_hnr", "shimmer", "jitter_s"),
        new = c("F1", "HNR", "Shimmer","s(Jitter)"))
setnames(df_TD, old = c("avg_F1", "mean_hnr", "shimmer", "jitter_s"),
         new = c("F1", "HNR", "Shimmer","s(Jitter)"))

create_beautiful_radarchart <- function(data, color = "#00AFBB", plwd_fig=5,
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
        radarchart(
                data, axistype = 1,
                # Personnaliser le polygone
                pcol = color, pfcol = scales::alpha(color, 0), plwd = plwd_fig, plty = 1,
                # Personnaliser la grille
                cglcol = "black", cglty = "dashed", cglwd = 0.8,
                # Personnaliser l'axe
                axislabcol = "black", 
                # Étiquettes des variables
                vlcex = vlcex, vlabels = vlabels,
                caxislabels = caxislabels, title = title, ...
        )
}
tiff("Figure1c.tiff", width= 2604, height= 1000, units="px", res=300)
#windows(15,10)
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,2))

B=rep("grey",(dim(df_TSA)[1]-3))
B2=rep(2,(dim(df_TSA)[1]-3))
create_beautiful_radarchart(
 data = df_TSA, caxislabels = c(-4, -2, 0, 2, 4),vlcex=1.2,
 color = c(B,"#FF4E28"),plwd_fig=c(B2,5))

A=rep("grey",(dim(df_TD)[1]-3))
A2=rep(2,(dim(df_TD)[1]-3))
create_beautiful_radarchart(
        data = df_TD, caxislabels = c(-4, -2, 0, 2, 4),vlcex=1.2,
        color = c(A,"#20976b"),plwd_fig=c(A2,5))
par(op)
dev.off()