###################################################################################################
######################################## GLM MODELLING ############################################
###################################################################################################

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

###################################################################################################
## STEP 01 : Base test                                                                           ##
###################################################################################################

#Definition des contrats
set.seed(2012)
dummy<-sample(rep(0:1),size=length(unique(base_train$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base_train$Num_contrat)[dummy==0]
num_contrat_test<-unique(base_train$Num_contrat)[dummy==1]

#base
base_xt_train<-base_train[base_train$Num_contrat %in% num_contrat_train,]
base_xt_test<-base_train[base_train$Num_contrat %in% num_contrat_test,]

###################################################################################################
## STEP 01 : Combine all data                                                                       ##
###################################################################################################

combine_base     = rbind(base_xt_train,base_xt_test,base_test)

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
sparse_matrix_train<-sparse_matrix_combine[1:55728,]
sparse_matrix_test<-sparse_matrix_combine[55729:70019,]
sparse_matrix_real_test<-sparse_matrix_combine[70020:87195,]

dtrain <- as.data.frame(as.matrix(sparse_matrix_train))
dtest <- as.data.frame(as.matrix(sparse_matrix_test))
d_real_test <- as.data.frame(as.matrix(sparse_matrix_real_test))

###################################################################################################
## STEP 03 : MODEL                                                                      ##
###################################################################################################

fit_xt<-extraTrees(x=dtrain, 
                   y=base_xt_train$CHARGE_SINISTRE,
                   ntree=400,
                   numRandomCuts = 2,
                   mtry=2,
                   nodesize=350)

###################################################################################################
## STEP 04 : PURE PREMIUM : VERIF                                                                      ##
###################################################################################################

pred_xt = predict(fit_xt,dtrain)
sum(pred_xt)
sum(base_xt_train$CHARGE_SINISTRE)

###################################################################################################
## STEP 05 : PURE PREMIUM ON TEST                                                                    ##
###################################################################################################

PP_pred_xt = predict(fit_xt,dtest)
sum(PP_pred_xt)
sum(base_xt_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 06 : ERROR MEASURES                                                                     ##
###################################################################################################

#Notre modèle
ineq(PP_pred_xt)
rmse(base_xt_test$CHARGE_SINISTRE,PP_pred_xt)
our_mape(base_xt_test$CHARGE_SINISTRE,PP_pred_xt)

#Modèle naïf
ineq(mean(base_xt_test$CHARGE_SINISTRE))
rmse(base_xt_test$CHARGE_SINISTRE,mean(base_xt_test$CHARGE_SINISTRE))
our_mape(base_xt_test$CHARGE_SINISTRE,mean(base_xt_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 07 : PP ON REAL TEST                                                                     ##
###################################################################################################

PP_pred_test_xt = predict(fit_xt,d_real_test)
sum(PP_pred_test_xt)
sum(base_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 08 : ERROR MEASURES  ON TEST                                                                   ##
###################################################################################################

#Notre modèle
ineq(PP_pred_test_xt)
rmse(base_test$CHARGE_SINISTRE,PP_pred_test_xt)
our_mape(base_test$CHARGE_SINISTRE,PP_pred_test_xt)

#Modèle naïf
ineq(mean(base_test$CHARGE_SINISTRE))
rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))
our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 09 : SAVE XT                                                                   ##
###################################################################################################

save(fit_xt,file="01-OUTPUTS/MODEL/XTREE.Rdata")

###################################################################################################
## STEP 10 : SAVE PRED                                                                   ##
###################################################################################################

save(PP_pred_test_xt,file="01-OUTPUTS/PREDICTIONS/PP_PRED_XT.Rdata")
