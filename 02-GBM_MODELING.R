###################################################################################################
######################################## GBM MODELLING ############################################
###################################################################################################

# Last Update: 10/07/16

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

###################################################################################################
## STEP 01 : Base test                                                                           ##
###################################################################################################

#Definition des contrats
set.seed(2011)
dummy<-sample(rep(0:1),size=length(unique(base_train$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base_train$Num_contrat)[dummy==0]
num_contrat_test<-unique(base_train$Num_contrat)[dummy==1]

#base
base_gbm_train<-base_train[base_train$Num_contrat %in% num_contrat_train,]
base_gbm_test<-base_train[base_train$Num_contrat %in% num_contrat_test,]

###################################################################################################
## STEP 01 : Combine all data                                                                       ##
###################################################################################################

combine_base     = rbind(base_gbm_train,base_gbm_test,base_test)

combine_base %<>%
  dplyr::select(CHARGE_SINISTRE, 
                Classe_Age_Situ_Cont,
                Creation_Entr,
                Mode_gestion,
                Zone,
                Segment,
                Fractionnement,
                Exposition_au_risque,
                Age_du_vehicule,
                FORMULE,
                Activite,
                ValeurPuissance)

###################################################################################################
## STEP 02 : Create data                                                                      ##
###################################################################################################

#Sparse matrix
sparse_matrix_combine <- sparse.model.matrix(CHARGE_SINISTRE~.-1,data = combine_base)
sparse_matrix_train<-sparse_matrix_combine[1:55928,]
sparse_matrix_test<-sparse_matrix_combine[55929:70019,]
sparse_matrix_real_test<-sparse_matrix_combine[70020:87195,]

dtrain <- as.data.frame(as.matrix(sparse_matrix_train))
dtest <- as.data.frame(as.matrix(sparse_matrix_test))
d_real_test <- as.data.frame(as.matrix(sparse_matrix_real_test))

dtrain$CHARGE_SINISTRE<-base_gbm_train$CHARGE_SINISTRE

###################################################################################################
## STEP 01 : FREQUENCY                                                                          ##
###################################################################################################

x<-c(1:32)
y<-33

gbm_model <- gbm.step(data=dtrain, gbm.x = x, gbm.y = y,
                            family = "gaussian", tree.complexity = 5,
                            learning.rate = 0.01, bag.fraction = 0.5)


summary(gbm_model)
find.int <- gbm.interactions(gbm_model)

interaction<-as.data.frame(find.int$interactions)

