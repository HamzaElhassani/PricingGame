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
set.seed(2015)
dummy<-sample(rep(0:1),size=length(unique(base_train$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base_train$Num_contrat)[dummy==0]
num_contrat_test<-unique(base_train$Num_contrat)[dummy==1]

#base
base_tweedie_train<-base_train[base_train$Num_contrat %in% num_contrat_train,]
base_tweedie_test<-base_train[base_train$Num_contrat %in% num_contrat_test,]

###################################################################################################
## STEP 02 : TWEEDIE                                                                          ##
###################################################################################################

min_charge = min(base_tweedie_train$CHARGE_SINISTRE)
cum_charge = (-min(base_tweedie_train$CHARGE_SINISTRE) +1)*length(base_tweedie_train$CHARGE_SINISTRE[base_tweedie_train$CHARGE_SINISTRE!=0])

base_tweedie_train %>%
  mutate(CHARGE_SINISTRE_noneg = ifelse(CHARGE_SINISTRE!=0,
                                    CHARGE_SINISTRE - min_charge + 1 ,
                                    0)) -> base_tweedie_train


#Tweedie
tweed<-glm(CHARGE_SINISTRE_noneg ~ 
             Creation_Entr + 
             Mode_gestion+ 
             Zone +
             Age_du_vehicule + 
             FORMULE  + 
             ValeurPuissance, 
           data=base_tweedie_train,
           offset = log (Exposition_au_risque),
           family = tweedie (var.power =1.5, link.power=0),
           weights = Exposition_au_risque^(1.5-1))

summary(tweed)
#anova(tweed, test="Chisq")
#Plot_Coeff(tweed,base_tweedie_train)

###################################################################################################
## STEP 04 : PURE PREMIUM : VERIF                                                                      ##
###################################################################################################

pred_tweedie = predict(tweed,newdata = base_tweedie_train,type="response") - cum_charge/length(base_tweedie_train$CHARGE_SINISTRE)
sum(pred_tweedie)
sum(base_tweedie_train$CHARGE_SINISTRE)

###################################################################################################
## STEP 05 : PURE PREMIUM ON TEST                                                                    ##
###################################################################################################

PP_pred_tweedie = predict(tweed,newdata = base_tweedie_test,type="response") - cum_charge/length(base_tweedie_train$CHARGE_SINISTRE)
sum(PP_pred_tweedie)
sum(base_tweedie_test$CHARGE_SINISTRE)

summary(PP_pred_tweedie)
###################################################################################################
## STEP 06 : ERROR MEASURES                                                                     ##
###################################################################################################

#Notre modèle
ineq(PP_pred_tweedie)
rmse(base_tweedie_test$CHARGE_SINISTRE,PP_pred_tweedie)
our_mape(base_tweedie_test$CHARGE_SINISTRE,PP_pred_tweedie)

#Modèle naïf
ineq(mean(base_tweedie_test$CHARGE_SINISTRE))
rmse(base_tweedie_test$CHARGE_SINISTRE,mean(base_tweedie_test$CHARGE_SINISTRE))
our_mape(base_tweedie_test$CHARGE_SINISTRE,mean(base_tweedie_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 07 : PP ON REAL TEST                                                                     ##
###################################################################################################

PP_pred_test_tweedie = predict(tweed,newdata = base_test,type="response") - cum_charge/length(base_tweedie_train$CHARGE_SINISTRE)

sum(PP_pred_test_tweedie) 
sum(base_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 06 : ERROR MEASURES  ON TEST                                                                   ##
###################################################################################################

#Notre modèle
ineq(PP_pred_test_tweedie)
rmse(base_test$CHARGE_SINISTRE,PP_pred_test_tweedie)
our_mape(base_test$CHARGE_SINISTRE,PP_pred_test_tweedie)

#Modèle naïf
ineq(mean(base_test$CHARGE_SINISTRE))
rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))
our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))


###################################################################################################
## STEP 07 : SAVE GLM                                                                   ##
###################################################################################################

save(tweed,file="01-OUTPUTS/MODEL/GLM_TWEEDIE.Rdata")

###################################################################################################
## STEP 08 : SAVE PRED                                                                   ##
###################################################################################################

save(PP_pred_test_tweedie,file="01-OUTPUTS/PREDICTIONS/PP_PRED_TWEEDIE.Rdata")
