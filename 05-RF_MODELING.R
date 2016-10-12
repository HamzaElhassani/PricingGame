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
set.seed(2013)
dummy<-sample(rep(0:1),size=length(unique(base_train$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base_train$Num_contrat)[dummy==0]
num_contrat_test<-unique(base_train$Num_contrat)[dummy==1]

#base
base_rf_train<-base_train[base_train$Num_contrat %in% num_contrat_train,]
base_rf_test<-base_train[base_train$Num_contrat %in% num_contrat_test,]

###################################################################################################
## STEP 02 : MODEL                                                                          ##
###################################################################################################

fit_rf <- randomForest(CHARGE_SINISTRE ~ 
                      Creation_Entr + 
                      Mode_gestion +
                      Zone + 
                      Fractionnement + 
                      Age_du_vehicule + 
                      FORMULE + 
                      Activite + 
                      ValeurPuissance, 
                    data=base_rf_train,
                    offset=Exposition_au_risque,
                    ntree=600, 
                    mtry=1,
                    nodesize=350,
                    type="regression")

###################################################################################################
## STEP 03 : PURE PREMIUM : VERIF                                                                      ##
###################################################################################################

PP_pred  = predict(fit_rf,newdata  = base_rf_train,type="response")

sum(PP_pred) 
sum(base_rf_train$CHARGE_SINISTRE)

###################################################################################################
## STEP 04 : PURE PREMIUM ON TEST                                                                    ##
###################################################################################################

PP_pred_rf  = predict(fit_rf,newdata  = base_rf_test,type="response")

sum(PP_pred_rf) 
sum(base_rf_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 05 : ERROR MEASURES                                                                     ##
###################################################################################################

#Notre modèle
ineq(PP_pred_rf)
rmse(base_rf_test$CHARGE_SINISTRE,PP_pred_rf)
our_mape(base_rf_test$CHARGE_SINISTRE,PP_pred_rf)

#Modèle naïf
ineq(mean(base_rf_test$CHARGE_SINISTRE))
rmse(base_rf_test$CHARGE_SINISTRE,mean(base_rf_test$CHARGE_SINISTRE))
our_mape(base_rf_test$CHARGE_SINISTRE,mean(base_rf_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 06 : PP ON REAL TEST                                                                     ##
###################################################################################################

PP_pred_test_rf  = predict(fit_rf,newdata  = base_test,type="response")

sum(PP_pred_test_rf) 
sum(base_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 07 : ERROR MEASURES  ON TEST                                                                   ##
###################################################################################################

#Notre modèle
ineq(PP_pred_test_rf)
rmse(base_test$CHARGE_SINISTRE,PP_pred_test_rf)
our_mape(base_test$CHARGE_SINISTRE,PP_pred_test_rf)

#Modèle naïf
ineq(mean(base_test$CHARGE_SINISTRE))
rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))
our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 08 : SAVE RF                                                                   ##
###################################################################################################

save(fit_rf,file="01-OUTPUTS/MODEL/RF.Rdata")

###################################################################################################
## STEP 09 : SAVE PRED                                                                   ##
###################################################################################################

save(PP_pred_test_rf,file="01-OUTPUTS/PREDICTIONS/PP_PRED_RF.Rdata")


