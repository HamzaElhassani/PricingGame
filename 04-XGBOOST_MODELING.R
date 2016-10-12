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
set.seed(2014)
dummy<-sample(rep(0:1),size=length(unique(base_train$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base_train$Num_contrat)[dummy==0]
num_contrat_test<-unique(base_train$Num_contrat)[dummy==1]

#base
base_xgb_train<-base_train[base_train$Num_contrat %in% num_contrat_train,]
base_xgb_test<-base_train[base_train$Num_contrat %in% num_contrat_test,]

###################################################################################################
## STEP 01 : Combine all data                                                                       ##
###################################################################################################

combine_base     = rbind(base_xgb_train,base_xgb_test,base_test)

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
sparse_matrix_train<-sparse_matrix_combine[1:56016,]
sparse_matrix_test<-sparse_matrix_combine[56017:70019,]
sparse_matrix_real_test<-sparse_matrix_combine[70020:87195,]

label_train<-base_xgb_train$CHARGE_SINISTRE
label_test<-base_xgb_test$CHARGE_SINISTRE

dtrain <- xgb.DMatrix(data = sparse_matrix_train, label=label_train)
dtest <- xgb.DMatrix(data = sparse_matrix_test, label=label_test)

###################################################################################################
## STEP 03 : XGBOOST                                                                          ##
###################################################################################################

train.gdbt<-xgb.train(params=list(objective="reg:linear", 
                                  eta=0.01, 
                                  max_depth=2 ,
                                  nthread = 2, 
                                  subsample = 0.3,
                                  colsample_bytree = 2, 
                                  eval_metric = "rmse"),
                      early.stop.round = 30,
                      data=dtrain, nrounds=1000, 
                      watchlist=list(eval=dtest, train=dtrain))

importance_matrix <- xgb.importance(sparse_matrix_train@Dimnames[[2]], model = train.gdbt)
xgb.plot.importance(importance_matrix)

###################################################################################################
## STEP 04 : PURE PREMIUM : VERIF                                                                      ##
###################################################################################################

pred_xgb = predict(train.gdbt,sparse_matrix_train)
sum(pred_xgb)
sum(base_xgb_train$CHARGE_SINISTRE)

###################################################################################################
## STEP 05 : PURE PREMIUM ON TEST                                                                    ##
###################################################################################################

PP_pred_xgb = predict(train.gdbt,sparse_matrix_test)
sum(PP_pred_xgb)
sum(base_xgb_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 06 : ERROR MEASURES                                                                     ##
###################################################################################################

#Notre modèle
ineq(PP_pred_xgb)
rmse(base_xgb_test$CHARGE_SINISTRE,PP_pred_xgb)
our_mape(base_xgb_test$CHARGE_SINISTRE,PP_pred_xgb)

#Modèle naïf
ineq(mean(base_xgb_test$CHARGE_SINISTRE))
rmse(base_xgb_test$CHARGE_SINISTRE,mean(base_xgb_test$CHARGE_SINISTRE))
our_mape(base_xgb_test$CHARGE_SINISTRE,mean(base_xgb_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 07 : PP ON REAL TEST                                                                     ##
###################################################################################################

PP_pred_test_xgb = predict(train.gdbt,sparse_matrix_real_test)
sum(PP_pred_test_xgb)
sum(base_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 08 : ERROR MEASURES  ON TEST                                                                   ##
###################################################################################################

#Notre modèle
ineq(PP_pred_test_xgb)
rmse(base_test$CHARGE_SINISTRE,PP_pred_test_xgb)
our_mape(base_test$CHARGE_SINISTRE,PP_pred_test_xgb)

#Modèle naïf
ineq(mean(base_test$CHARGE_SINISTRE))
rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))
our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 09 : SAVE XGB                                                                   ##
###################################################################################################

save(train.gdbt,file="01-OUTPUTS/MODEL/XGBOOST.Rdata")

###################################################################################################
## STEP 10 : SAVE PRED                                                                   ##
###################################################################################################

save(PP_pred_test_xgb,file="01-OUTPUTS/PREDICTIONS/PP_PRED_XGB.Rdata")


