rm(list=ls())
chemin1="C:/Users/briend/Documents/Tours/Recherche/PROVAU_kmeans_test/"
setwd(chemin1)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(caret)
library(psych) #describe (permet d'avoir sd)
library(pROC)
library(ROCR)
library(questionr)

auc_u_test = function(vec, len_A, len_B){
  rank_value = rank(vec, ties.method="average")
  rank_sum = sum(rank_value[1:len_A])
  u_value = rank_sum - (len_A*(len_A+1))/2
  auc = u_value / (len_A * len_B)
  if(auc < 0.50) {
    auc = 1.0 - auc
  }
  return (auc)
}

Data <- read.table("Article_Voice_biomarker_results.csv",sep=';',row.names = 1,strip.white=TRUE,header=T,fill=TRUE)
n_ASD_Data=length(Data$Group_final[Data$Group=="ASD"])
n_HC_Data=length(Data$Group_final)-n_ASD_Data
n_SLI_Data=length(Data$Group_final[Data$Group=="SLI"])
n_IC_Data=length(Data$Group_final[Data$Group=="IC"])
n_TD_Data=length(Data$Group_final[Data$Group=="TD"])

####Superloop
# For Result Superloop test 1
sensitivity_test1_US = list()
specificity_test1_US = list()
posPredValue_test1_US = list()
negPredValue_test1_US = list()
Percentratio_test1_US = list()
Percent_table_test1_US=c()
Pourcent_test1_US=c()
auc_u_test_loop_fo = list()
auc_u_test_loop_f1 = list()
auc_u_test_loop_f2 = list()
auc_u_test_loop_f3 = list()
auc_u_test_loop_f4 = list()
auc_u_test_loop_hnr = list()
auc_u_test_loop_jit = list()
auc_u_test_loop_shi = list()
auc_u_test_loop_dis = list()
#pour kurtosis et skewness
auc_u_test_loop_fo_k = list();auc_u_test_loop_fo_s = list()
auc_u_test_loop_hnr_k = list();auc_u_test_loop_hnr_s = list()
auc_u_test_loop_jit_k = list();auc_u_test_loop_jit_s = list()
auc_u_test_loop_shi_k = list();auc_u_test_loop_shi_s = list()
auc_u_test_loop_dis_k = list();auc_u_test_loop_dis_s = list()

sensitivity_test1_S = list()
specificity_test1_S = list()
posPredValue_test1_S = list()
negPredValue_test1_S = list()
Percentratio_test1_S = list()
Percent_table_test1_S=c()
Pourcent_test1_S=c()

# For Result Superloop test 2
sensitivity_test2 = list()
specificity_test2 = list()
posPredValue_test2 = list()
negPredValue_test2 = list()
Percentratio_test2 = list()
Percent_table_test2_final=c()
Percent_table_test2_name_final=c()
Percent_table_test2_group_final=c()

# For Result Superloop test 3
sensitivity_test3 = list()
specificity_test3 = list()
posPredValue_test3 = list()
negPredValue_test3 = list()
Percentratio_test3 = list()
Percent_table_test3_final=c()
Percent_table_test3_name_final=c()
Percent_table_test3_group_final=c()

#for (i in (115:115)){
#n=c(2e+00,5e+00,1e+01,5e+01,7.5e+01,1e+02,2.5e+02,5e+02,7.5e+02,1e+03)
for (i in (1:7.5e+01)){
#for (i in (100:599)){

# selection aléatoire sujet
#set.seed(129)
set.seed(i) #p=0.31 pour avoir equilibre
  
#Datafinal=Data[Data$final=="yes",] #COMMENTER SI TEST 4
Datafinal=Data # SI TEST 4 = ne pas COMMENTER
Datafinal=Datafinal[Datafinal$REJECT==0,] #1 = rejete

training.samples <- Datafinal$Group %>%
  createDataPartition(p = 0.31, list = FALSE) #p=0.2, 0.4, 0.6, 0.8 0.31
test.data  <- Datafinal[training.samples, ]
train.data <- Datafinal[-training.samples, ]

#choix groupe pour choix 
Databis=train.data

#test1
n_ASD=length(Databis$Group_final[Databis$Group=="ASD"])
n_HC=length(Databis$Group_final)-n_ASD
n_train=length(Databis$Group_final)
#test2
n_ASD_test2=length(test.data$Group_final[test.data$Group=="ASD"])
#n_HC_test2=length(test.data$Group_final[test.data$Group=="TD"]) #COMMENTER SI TEST 4
n_HC_test2=length(test.data$Group_final)-n_ASD_test2

#test 3
NO=Data[Data$final=="test3",]
NO=NO[NO$REJECT==0,] #1 = rejete
n_SLI_test3=length(NO$Group_final[NO$Group=="SLI"]) #DLD
n_IC_test3=length(NO$Group_final[NO$Group=="IC"]) #CI

#Definition
Databis<- Databis[,-1] #remove participant column
Group<- Databis[,2]
subgroup<- Databis[,1]
Databis<- Databis[,-1] #remove group column
Databis<- Databis[,-1] #remove group_final
Databis<- Databis[,-1] #remove model
Databis<- Databis[,c(-1,-2)] #remove "REJECT","ErrorsROCKCA"

df <- scale(Databis) #standardiser (centrage réduction) les données
xdf <- as.data.frame(df, stringsAsFactors = FALSE)
xdf=cbind(xdf,Group)
table(xdf$Group)

# AUC-------------------------------------------------------
#########
## AUC ##
#########
# #Calcul AUC pour avoir kmeans supervisé ("avg_f0"   "avg_F1"   "avg_F2"   "avg_F3"   "avg_F4"   "mean_hnr" "jitter"   "shimmer" "dispersion_formantique")
# #WAY 3 cf. autre scripts pour way 1 & 2
# auc_u_test_loop_fo[[i]] = auc_u_test(xdf$avg_f0,n_ASD,n_HC)
# auc_u_test_loop_f1[[i]] = auc_u_test(xdf$avg_F1,n_ASD,n_HC)
# auc_u_test_loop_f2[[i]] = auc_u_test(xdf$avg_F2,n_ASD,n_HC)
# auc_u_test_loop_f3[[i]] = auc_u_test(xdf$avg_F3,n_ASD,n_HC)
# auc_u_test_loop_f4[[i]] = auc_u_test(xdf$avg_F4,n_ASD,n_HC)
# auc_u_test_loop_hnr[[i]] = auc_u_test(xdf$mean_hnr,n_ASD,n_HC)
# auc_u_test_loop_jit[[i]] = auc_u_test(xdf$jitter,n_ASD,n_HC)
# auc_u_test_loop_shi[[i]] = auc_u_test(xdf$shimmer,n_ASD,n_HC)
# auc_u_test_loop_dis[[i]] = auc_u_test(xdf$dispersion_formantique,n_ASD,n_HC)
# # pour kurtosis
# auc_u_test_loop_fo_k[[i]] = auc_u_test(xdf$avg_f0_k,n_ASD,n_HC)
# auc_u_test_loop_hnr_k[[i]] = auc_u_test(xdf$mean_hnr_k,n_ASD,n_HC)
# auc_u_test_loop_jit_k[[i]] = auc_u_test(xdf$jitter_k,n_ASD,n_HC)
# auc_u_test_loop_shi_k[[i]] = auc_u_test(xdf$shimmer_k,n_ASD,n_HC)
# auc_u_test_loop_dis_k[[i]] = auc_u_test(xdf$dispersion_formantique_k,n_ASD,n_HC)
# # pour skewness
# auc_u_test_loop_fo_s[[i]] = auc_u_test(xdf$avg_f0_s,n_ASD,n_HC)
# auc_u_test_loop_hnr_s[[i]] = auc_u_test(xdf$mean_hnr_s,n_ASD,n_HC)
# auc_u_test_loop_jit_s[[i]] = auc_u_test(xdf$jitter_s,n_ASD,n_HC)
# auc_u_test_loop_shi_s[[i]] = auc_u_test(xdf$shimmer_s,n_ASD,n_HC)
# auc_u_test_loop_dis_s[[i]] = auc_u_test(xdf$dispersion_formantique_s,n_ASD,n_HC)

# Test 1-------------------------------------------------------
############
## Test 1 ##
############

###k-means k=2, Unsupervised#
#############################
k2 <- kmeans(df, centers = 2, nstart = 50) #2 clusters

#affiliation group et cluster
testre1_US=as.data.frame(table(k2$cluster[1:n_ASD])) #car 15 premiers sont TSA, puis HC
threshold_testre1_US=testre1_US$Var1[testre1_US$Freq==max(testre1_US$Freq)]
predicted_values<-ifelse(k2$cluster==threshold_testre1_US,0,1)  ##0=malade, 1= sain (TSA)
actual_values=xdf$Group

conf_matrix<-table(predicted_values,actual_values)

#sensitivity
sensitivity(conf_matrix)
#Specificity
specificity(conf_matrix)
#valeur prédictive positive: (+)Likelihood Ratio
posPredValue(conf_matrix)
#valeur prédictive négative: (-)Likelihood Ratio
negPredValue(conf_matrix)

#Wss/tot ss: Internal statistic
#Percentratio=(k2$betweenss/k2$totss)*100
Percentratio=(k2$tot.withinss/k2$totss)*100;Percentratio

#Result Superloop
sensitivity_test1_US[[i]] = sensitivity(conf_matrix)
specificity_test1_US[[i]] = specificity(conf_matrix)
posPredValue_test1_US[[i]] = posPredValue(conf_matrix)
negPredValue_test1_US[[i]] = negPredValue(conf_matrix)
Percentratio_test1_US[[i]] = Percentratio

for (taille in seq(length(actual_values))){
Percent_table_test1_US[[taille]]=identical(as.numeric(actual_values[taille]),as.numeric(predicted_values[taille]))
}
machin = do.call(rbind, Percent_table_test1_US);
Pourcent_test1_US= cbind(Pourcent_test1_US,machin)

## k-means k=2, 4 variables# Supervised ##
##########################################
#df= df[,c(2,6,8,13,17)] #5 variable decidée par AUC
df= df[,c(2,8,13)] #3 variable decidée par AUC pour TEST 4
xdf <- as.data.frame(df, stringsAsFactors = FALSE)
xdf=cbind(xdf,Group)

k2 <- kmeans(df, centers = 2, nstart = 50) #2 clusters

#affiliation group et cluster
testre1_S=as.data.frame(table(k2$cluster[1:n_ASD])) #car 15 premiers sont TSA, puis HC
threshold_testre1_S=testre1_S$Var1[testre1_S$Freq==max(testre1_S$Freq)]
predicted_values<-ifelse(k2$cluster==threshold_testre1_S,0,1)  ##0=malade, 1= sain (TSA)
actual_values=xdf$Group
conf_matrix<-table(predicted_values,actual_values)

#sensitivity
sensitivity(conf_matrix)
#Specificity
specificity(conf_matrix)
#valeur prédictive positive: (+)Likelihood Ratio
posPredValue(conf_matrix)
#valeur prédictive négative: (-)Likelihood Ratio
negPredValue(conf_matrix)

#Wss/tot ss: Internal statistic
#Percentratio=(k2$betweenss/k2$totss)*100
Percentratio=(k2$tot.withinss/k2$totss)*100;Percentratio

#Result Superloop
sensitivity_test1_S[[i]] = sensitivity(conf_matrix)
specificity_test1_S[[i]] = specificity(conf_matrix)
posPredValue_test1_S[[i]] = posPredValue(conf_matrix)
negPredValue_test1_S[[i]] = negPredValue(conf_matrix)
Percentratio_test1_S[[i]] = Percentratio

for (taille in seq(length(actual_values))){
  Percent_table_test1_S[[taille]]=identical(as.numeric(actual_values[taille]),as.numeric(predicted_values[taille]))
}
machino = do.call(rbind, Percent_table_test1_S);
Pourcent_test1_S= cbind(Pourcent_test1_S,machino)

# Test 2-------------------------------------------------------
############
## Test 2 ##
############
sensitivity_datalista = list()
specificity_datalista = list()
posPredValue_datalista = list()
negPredValue_datalista = list()
Percentratio_datalista = list()
Percent_table_test2 = list()
Percent_table_test2_name = list()
Percent_table_test2_group = list()

Databis=train.data
A=dim(test.data)[1]

for (m in seq(A)){
  #m=18
  B=paste("#2#",test.data$etiquettes.de.lignes[m],test.data$Group[m],"#2#",sep=" ")
  print (B)
  Data_test2=rbind(Databis,test.data[m,])
  Data_test2<- Data_test2[,-1] #remove participant column
  Group_test2<- Data_test2[,2]
  subgroup<- Data_test2[,1]
  Data_test2<- Data_test2[,-1] #remove group column
  Data_test2<- Data_test2[,-1] #remove group_final
  Data_test2<- Data_test2[,-1] #remove final
  Data_test2<- Data_test2[,c(-1,-2)] #remove "REJECT","ErrorsROCKCA"
#
  df_test2 <- scale(Data_test2) #standardiser (centrage réduction) les données
  #df_test2= df_test2[,c(2,6,8,13,17)] #variable decidée par AUC Abstract with F2 et f0
  df_test2= df_test2[,c(2,8,13)] #3 variable decidée par AUC pour TEST4
  xdf_test2 <- as.data.frame(df_test2, stringsAsFactors = FALSE)
  xdf_test2=cbind(xdf_test2,Group_test2)
  k2_test2 <- kmeans(df_test2, centers = 2, nstart = 50) #2 clusters
#
  #affiliation group et cluster
  testre=as.data.frame(table(k2_test2$cluster[1:n_ASD])) #car 15 premiers sont TSA, puis HC
  threshold_test2=testre$Var1[testre$Freq==max(testre$Freq)]
  predicted_values_test2<-ifelse(k2_test2$cluster==threshold_test2,0,1) ##0=malade, 1= sain (TSA)
  actual_values_test2=xdf_test2$Group
  conf_matrix_test2<-table(predicted_values_test2,actual_values_test2)
#
  AV=paste("actual_values_test2",actual_values_test2[(n_train+1)],sep=" = ")
  PV=paste("predicted_values_test2",predicted_values_test2[(n_train+1)],sep=" = ")
  Percent_table_test2[[m]]=identical(as.numeric(actual_values_test2[(n_train+1)]),as.numeric(predicted_values_test2[(n_train+1)]))
  Percent_table_test2_name[[m]]=test.data$etiquettes.de.lignes[m]
  Percent_table_test2_group[[m]]=test.data$Group[m]
#
  sensitivity_datalista[[m]]=sensitivity(conf_matrix_test2)
  specificity_datalista[[m]]=specificity(conf_matrix_test2)
  posPredValue_datalista[[m]]=posPredValue(conf_matrix_test2)
  negPredValue_datalista[[m]]= negPredValue(conf_matrix_test2)
  Percentratio_datalista[[m]]=(k2_test2$tot.withinss/k2_test2$totss)*100
}

sensitivity_big_data = do.call(rbind, sensitivity_datalista);
sensitivity_test2[[i]]=mean(sensitivity_big_data)
specificity_big_data = do.call(rbind, specificity_datalista);
specificity_test2[[i]]=mean(specificity_big_data)
posPredValue_big_data = do.call(rbind, posPredValue_datalista);
posPredValue_test2[[i]]=mean(posPredValue_big_data)
negPredValue_big_data = do.call(rbind, negPredValue_datalista);
negPredValue_test2[[i]]=mean(negPredValue_big_data)
Percentratio_big_data = do.call(rbind, Percentratio_datalista);
Percentratio_test2[[i]]=mean(Percentratio_big_data)
Percent_table_test2_big_data = do.call(rbind, Percent_table_test2);
Percent_table_test2_final= cbind(Percent_table_test2_final,Percent_table_test2_big_data)
Percent_table_test2_name_big_data = do.call(rbind, Percent_table_test2_name);
Percent_table_test2_name_final= cbind(Percent_table_test2_name_final,Percent_table_test2_name_big_data)
Percent_table_test2_group_big_data = do.call(rbind, Percent_table_test2_group);
Percent_table_test2_group_final= cbind(Percent_table_test2_group_final,Percent_table_test2_group_big_data)

# Test 3-------------------------------------------------------
# ############
# ## Test 3 ##
# ############
# sensitivity_datalista3 = list()
# specificity_datalista3 = list()
# posPredValue_datalista3 = list()
# negPredValue_datalista3 = list()
# Percentratio_datalista3 = list()
# Percent_table_test3 = list()
# Percent_table_test3_name = list()
# Percent_table_test3_group = list()
# #
# Databis=train.data
# #
# test.data3=Data[Data$final=="test3",]
# test.data3=test.data3[test.data3$REJECT==0,] #1 = rejete
# A3=dim(test.data3)[1]
# 
# for (m in seq(A3)){
#   #m=18
#   c=paste("#3#",test.data3$etiquettes.de.lignes[m],test.data3$Group[m],"#3#",sep=" ")
#   print (c)
#   Data_test3=rbind(Databis,test.data3[m,])
#   Data_test3<- Data_test3[,-1] #remove participant column
#   Group_test3<- Data_test3[,2]
#   subgroup<- Data_test3[,1]
#   Data_test3<- Data_test3[,-1] #remove group column
#   Data_test3<- Data_test3[,-1] #remove group_final
#   Data_test3<- Data_test3[,-1] #remove final
#   Data_test3<- Data_test3[,c(-1,-2)] #remove "REJECT","ErrorsROCKCA"
# #
#   df_test3 <- scale(Data_test3) #standardiser (centrage réduction) les données
#   df_test3= df_test3[,c(2,6,8,13,17)] #variable decidée par AUC Abstract with F2 et f0
#   xdf_test3 <- as.data.frame(df_test3, stringsAsFactors = FALSE)
#   xdf_test3=cbind(xdf_test3,Group_test3)
#   k2_test3 <- kmeans(df_test3, centers = 2, nstart = 50) #2 clusters
# #
#   #affiliation group et cluster
#   testre=as.data.frame(table(k2_test3$cluster[1:n_ASD]))
#   threshold_test3=testre$Var1[testre$Freq==max(testre$Freq)]
#   predicted_values_test3<-ifelse(k2_test3$cluster==threshold_test3,0,1) ##0=malade, 1= sain (TSA)
#   actual_values_test3=xdf_test3$Group
#   conf_matrix_test3<-table(predicted_values_test3,actual_values_test3)
# #
#   AV=paste("actual_values_test3",actual_values_test3[(n_train+1)],sep=" = ")
#   #print(AV)
#   PV=paste("predicted_values_test3",predicted_values_test3[(n_train+1)],sep=" = ")
#   #print(PV)
#   Percent_table_test3[[m]]=identical(as.numeric(actual_values_test3[(n_train+1)]),as.numeric(predicted_values_test3[(n_train+1)]))
#   Percent_table_test3_name[[m]]=test.data3$etiquettes.de.lignes[m]
#   Percent_table_test3_group[[m]]=test.data3$Group[m]
# #
#   sensitivity_datalista3[[m]]=sensitivity(conf_matrix_test3)
#   specificity_datalista3[[m]]=specificity(conf_matrix_test3)
#   posPredValue_datalista3[[m]]=posPredValue(conf_matrix_test3)
#   negPredValue_datalista3[[m]]= negPredValue(conf_matrix_test3)
#   Percentratio_datalista3[[m]]=(k2_test3$tot.withinss/k2_test3$totss)*100
# }
# 
# sensitivity_big_data3 = do.call(rbind, sensitivity_datalista3);
# sensitivity_test3[[i]]=mean(sensitivity_big_data3)
# specificity_big_data3 = do.call(rbind, specificity_datalista3);
# specificity_test3[[i]]=mean(specificity_big_data3)
# posPredValue_big_data3 = do.call(rbind, posPredValue_datalista3);
# posPredValue_test3[[i]]=mean(posPredValue_big_data3)
# negPredValue_big_data3 = do.call(rbind, negPredValue_datalista3);
# negPredValue_test3[[i]]=mean(negPredValue_big_data3)
# Percentratio_big_data3 = do.call(rbind, Percentratio_datalista3);
# Percentratio_test3[[i]]=mean(Percentratio_big_data3)
# Percent_table_test3_big_data = do.call(rbind, Percent_table_test3);
# Percent_table_test3_final= cbind(Percent_table_test3_final,Percent_table_test3_big_data)
# Percent_table_test3_name_big_data = do.call(rbind, Percent_table_test3_name);
# Percent_table_test3_name_final= cbind(Percent_table_test3_name_final,Percent_table_test3_name_big_data)
# Percent_table_test3_group_big_data = do.call(rbind, Percent_table_test3_group);
# Percent_table_test3_group_final= cbind(Percent_table_test3_group_final,Percent_table_test3_group_big_data)

} #fin Superloop
# Fin loop-------------------------------------------------------

# ##########
# ## AUC  ##
# ##########
# auc_u_test_loop_fo_big_data = do.call(rbind, auc_u_test_loop_fo);
# auc_u_test_loop_f1_big_data = do.call(rbind, auc_u_test_loop_f1);
# auc_u_test_loop_f2_big_data = do.call(rbind, auc_u_test_loop_f2);
# auc_u_test_loop_f3_big_data = do.call(rbind, auc_u_test_loop_f3);
# auc_u_test_loop_f4_big_data = do.call(rbind, auc_u_test_loop_f4);
# auc_u_test_loop_hnr_big_data = do.call(rbind, auc_u_test_loop_hnr);
# auc_u_test_loop_jit_big_data = do.call(rbind, auc_u_test_loop_jit);
# auc_u_test_loop_shi_big_data = do.call(rbind, auc_u_test_loop_shi);
# auc_u_test_loop_dis_big_data = do.call(rbind, auc_u_test_loop_dis);
# #pour kurtosis
# auc_u_test_loop_fo_k_big_data = do.call(rbind, auc_u_test_loop_fo_k);
# auc_u_test_loop_hnr_k_big_data = do.call(rbind, auc_u_test_loop_hnr_k);
# auc_u_test_loop_jit_k_big_data = do.call(rbind, auc_u_test_loop_jit_k);
# auc_u_test_loop_shi_k_big_data = do.call(rbind, auc_u_test_loop_shi_k);
# auc_u_test_loop_dis_k_big_data = do.call(rbind, auc_u_test_loop_dis_k);
# #pour skewness
# auc_u_test_loop_fo_s_big_data = do.call(rbind, auc_u_test_loop_fo_s);
# auc_u_test_loop_hnr_s_big_data = do.call(rbind, auc_u_test_loop_hnr_s);
# auc_u_test_loop_jit_s_big_data = do.call(rbind, auc_u_test_loop_jit_s);
# auc_u_test_loop_shi_s_big_data = do.call(rbind, auc_u_test_loop_shi_s);
# auc_u_test_loop_dis_s_big_data = do.call(rbind, auc_u_test_loop_dis_s);
# 
# # AUC_all=cbind(auc_u_test_loop_fo_big_data,auc_u_test_loop_f1_big_data,auc_u_test_loop_f2_big_data,auc_u_test_loop_f3_big_data,auc_u_test_loop_f4_big_data,
# #               auc_u_test_loop_hnr_big_data,auc_u_test_loop_jit_big_data,auc_u_test_loop_shi_big_data,auc_u_test_loop_dis_big_data)
# # summary(AUC_all)
# 
# AUC_all_k_et_s=cbind(auc_u_test_loop_fo_big_data,auc_u_test_loop_f1_big_data,auc_u_test_loop_f2_big_data,auc_u_test_loop_f3_big_data,auc_u_test_loop_f4_big_data,
#               auc_u_test_loop_hnr_big_data,auc_u_test_loop_jit_big_data,auc_u_test_loop_shi_big_data,auc_u_test_loop_dis_big_data,
#               auc_u_test_loop_fo_k_big_data,
#               auc_u_test_loop_hnr_k_big_data,auc_u_test_loop_jit_k_big_data,auc_u_test_loop_shi_big_data,auc_u_test_loop_dis_big_data,
#               auc_u_test_loop_fo_s_big_data,
#               auc_u_test_loop_hnr_s_big_data,auc_u_test_loop_jit_s_big_data,auc_u_test_loop_shi_s_big_data,auc_u_test_loop_dis_s_big_data)
# summary(AUC_all_k_et_s)

############
## Test 1 ##
############ Le test 1c est ce même résultats mais avec p de createDataPartition(p = 0.23, list = FALSE) modifié
sensitivity_test1_US_big_data = do.call(rbind, sensitivity_test1_US);
specificity_test1_US_big_data = do.call(rbind, specificity_test1_US);
posPredValue_test1_US_big_data = do.call(rbind, posPredValue_test1_US);
negPredValue_test1_US_big_data = do.call(rbind, negPredValue_test1_US);
Percentratio_test1_US_big_data = do.call(rbind, Percentratio_test1_US);
Percent_table_test1_US_big_data = do.call(rbind, Percent_table_test1_US);

sensitivity_test1_S_big_data = do.call(rbind, sensitivity_test1_S);
specificity_test1_S_big_data = do.call(rbind, specificity_test1_S);
posPredValue_test1_S_big_data = do.call(rbind, posPredValue_test1_S);
negPredValue_test1_S_big_data = do.call(rbind, negPredValue_test1_S);
Percentratio_test1_S_big_data = do.call(rbind, Percentratio_test1_S);

final_test1_US=cbind(sensitivity_test1_US_big_data,specificity_test1_US_big_data,posPredValue_test1_US_big_data, negPredValue_test1_US_big_data, Percentratio_test1_US_big_data)
summary(final_test1_US)
#describe(final_test1_US)
apply(final_test1_US, 2, sd)

final_test1_S=cbind(sensitivity_test1_S_big_data,specificity_test1_S_big_data,posPredValue_test1_S_big_data, negPredValue_test1_S_big_data, Percentratio_test1_S_big_data)
summary(final_test1_S)
#describe(final_test1_S)
apply(final_test1_S, 2, sd)

############
## Test 2 ##
############ Commenter ligne "df_test2= df_test2[,c(2,6,8,13,17)] #supervised or not" pour supervisé ou non
sensitivity_test2_big_data = do.call(rbind, sensitivity_test2);
specificity_test2_big_data = do.call(rbind, specificity_test2);
posPredValue_test2_big_data = do.call(rbind, posPredValue_test2);
negPredValue_test2_big_data = do.call(rbind, negPredValue_test2);
Percentratio_test2_big_data = do.call(rbind, Percentratio_test2);

final_test2=cbind(sensitivity_test2_big_data,specificity_test2_big_data,posPredValue_test2_big_data, negPredValue_test2_big_data, Percentratio_test2_big_data)
#summary(final_test2)
describe(final_test2)

##ASD
Freq_test2=freq(table(Percent_table_test2_final[1:n_ASD_test2,]), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #ASD
Freq_test2
binom.test(Freq_test2$n[2],(Freq_test2$n[2]+Freq_test2$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int TEST4
binom.test(3787     ,(3787    +713         ))$conf.int #IC95  binom.test(True,(True+False))$conf.i TEST4
# binom.test(3289   ,(3289  +1211     ))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
# binom.test(4005    ,(4005   +495       ))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
##HC
Freq_test2a=freq(table(Percent_table_test2_final[(n_ASD_test2+1):(n_ASD_test2+n_HC_test2),]), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #HC
Freq_test2a
binom.test(Freq_test2a$n[2],(Freq_test2a$n[2]+Freq_test2a$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int TEST4
binom.test(7696      ,(7696     +1804           ))$conf.int #IC95  binom.test(True,(True+False))$conf.i TEST4
# binom.test(3230  ,(3230+270  ))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
# binom.test(3185  ,(3185 +315    ))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
# DLD #IC #TD
# DLD
n_ASD_test2_dld=length(test.data$Group_final[test.data$Group=="SLI"]) #DLD
n_ASD_test2_ic=length(test.data$Group_final[test.data$Group=="IC"]) #CI
n_ASD_test2_td=length(test.data$Group_final[test.data$Group=="TD"]) #TD
Freq_test2dld=freq(table(Percent_table_test2_final[(n_ASD_test2+1):(n_ASD_test2_dld),]), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #HC
Freq_test2dld
binom.test(Freq_test2dld$n[2],(Freq_test2dld$n[2]+Freq_test2dld$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int TEST4
# IC
Freq_test2ic=freq(table(Percent_table_test2_final[(n_ASD_test2+n_ASD_test2_dld+1):(n_ASD_test2_ic),]), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #HC
Freq_test2ic
binom.test(Freq_test2ic$n[2],(Freq_test2ic$n[2]+Freq_test2ic$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int TEST4
#TD
Freq_test2td=freq(table(Percent_table_test2_final[(n_ASD_test2+n_ASD_test2_dld+n_ASD_test2_ic+1):(n_ASD_test2+n_HC_test2),]), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #HC
Freq_test2td
binom.test(Freq_test2td$n[2],(Freq_test2td$n[2]+Freq_test2td$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int TEST4
##total
Freq_test2b=freq(table(Percent_table_test2_final), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #HC
Freq_test2b
binom.test(Freq_test2b$n[2],(Freq_test2b$n[2]+Freq_test2b$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int TEST4
binom.test(11483     ,(11483     +2517))$conf.int #IC95  binom.test(True,(True+False))$conf.i TEST4
# binom.test(6519 ,(6519 +1481  ))$conf.int #IC95  binom.test(True,(True+False))$conf.int
# binom.test(7190   ,(7190   +810    ))$conf.int #IC95  binom.test(True,(True+False))$conf.int

table(Percent_table_test2_final[(n_ASD_test2+n_SLI_test2+n_IC_test2+1):(n_ASD_test2+n_HC_test2),]) #HC TD

############
## Test 3 ##
############ Commenter ligne "df_test3= df_test3[,c(2,6,8,13,17)] #supervised or not" pour supervisé ou non
sensitivity_test3_big_data = do.call(rbind, sensitivity_test3);
specificity_test3_big_data = do.call(rbind, specificity_test3);
posPredValue_test3_big_data = do.call(rbind, posPredValue_test3);
negPredValue_test3_big_data = do.call(rbind, negPredValue_test3);
Percentratio_test3_big_data = do.call(rbind, Percentratio_test3);

final_test3=cbind(sensitivity_test3_big_data,specificity_test3_big_data,posPredValue_test3_big_data, negPredValue_test3_big_data, Percentratio_test3_big_data)
#summary(final_test3)
describe(final_test3)

Freq_test3=freq(table(Percent_table_test3_final), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #total
Freq_test3
binom.test(Freq_test3$n[2],(Freq_test3$n[2]+Freq_test3$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
binom.test(10622    ,(10622   +6878  ))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
Freq_test3a=freq(table(Percent_table_test3_final[1:n_SLI_test3,]), cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #SLI DLD
Freq_test3a
binom.test(Freq_test3a$n[2],(Freq_test3a$n[2]+Freq_test3a$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int
binom.test(4059   ,(4059   +3441    ))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
Freq_test3b=freq(table(Percent_table_test3_final[(n_SLI_test3+1):(n_SLI_test3+n_IC_test3),]) , cum = TRUE, total = TRUE, sort = "inc", digits = 2, exclude = NA) #IC CI
Freq_test3b
binom.test(Freq_test3b$n[2],(Freq_test3b$n[2]+Freq_test3b$n[1]))$conf.int #IC95  binom.test(True,(True+False))$conf.int 
binom.test(6563    ,(6563    +3437))$conf.int #IC95  binom.test(True,(True+False))$conf.int 