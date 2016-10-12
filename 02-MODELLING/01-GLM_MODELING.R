###################################################################################################
######################################## GLM MODELLING ############################################
###################################################################################################

# Last Update: 10/07/16

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

###################################################################################################
## STEP 01 : Base test                                                                           ##
###################################################################################################
#Definition des contrats
set.seed(2016)
dummy<-sample(rep(0:1),size=length(unique(base_train$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base_train$Num_contrat)[dummy==0]
num_contrat_test<-unique(base_train$Num_contrat)[dummy==1]

#base
base_glm_train<-base_train[base_train$Num_contrat %in% num_contrat_train,]
base_glm_test<-base_train[base_train$Num_contrat %in% num_contrat_test,]

#sinistre_
sinistres_glm_train<-sinistres_train[sinistres_train$Num_contrat.y %in% num_contrat_train,]
sinistres_glm_test<-sinistres_train[sinistres_train$Num_contrat.y %in% num_contrat_test,]

###################################################################################################
## STEP 02 : FREQUENCY                                                                          ##
###################################################################################################

#Modif pour model

glm_freq<- glm(NB_SINISTRE ~ 
                 Creation_Entr + 
                 Mode_gestion +
                 Zone + 
                 Fractionnement + 
                 Age_du_vehicule + 
                 FORMULE + 
                 Activite + 
                 ValeurPuissance  ,
               offset=log(Exposition_au_risque),
               family =poisson,
               data=base_glm_train)

summary(glm_freq)
#anova(glm_freq, test="Chisq")
#Plot_Coeff(glm_freq,base)

nb_pred   = predict(glm_freq,newdata = base_glm_train,type="response")
mean(nb_pred)
mean(base_glm_train$NB_SINISTRE)

###################################################################################################
## STEP 03 : CLAIMS                                                                             ##
###################################################################################################

min_charge = min(sinistres_glm_train$CHARGE_SINISTRE)
sinistres_glm_train %>% mutate(CHARGE_SINISTRE = CHARGE_SINISTRE - min_charge + 1 ) -> sinistres_glm_train_2

glm_claims<- glm(CHARGE_SINISTRE ~ 
                   Classe_Age_Situ_Cont +
                   Fractionnement + 
                   Age_du_vehicule + 
                   FORMULE  + 
                   ValeurPuissance ,
                 family = Gamma(link = "log"),
                 data=sinistres_glm_train_2)

summary(glm_claims)
#anova(glm_claims, test="Chisq")
#Plot_Coeff(glm_claims,base)

cout_pred = predict(glm_claims,newdata = sinistres_glm_train_2,type="response")
mean(sinistres_glm_train_2$CHARGE_SINISTRE)
mean(cout_pred)

###################################################################################################
## STEP 04 : PURE PREMIUM : VERIF                                                                      ##
###################################################################################################

charge_pred  = predict(glm_claims,newdata  = base_glm_train,type="response") + min_charge -1 
nb_pred      = predict(glm_freq, newdata   = base_glm_train,type="response")
PP_pred<-charge_pred*nb_pred

sum(PP_pred) 
sum(base_glm_train$CHARGE_SINISTRE)

###################################################################################################
## STEP 05 : PURE PREMIUM ON TEST                                                                    ##
###################################################################################################

charge_pred  = predict(glm_claims,newdata  = base_glm_test,type="response") + min_charge -1 
nb_pred      = predict(glm_freq, newdata   = base_glm_test,type="response")
PP_pred_glm  = charge_pred* nb_pred

sum(PP_pred_glm) 
sum(base_glm_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 06 : ERROR MEASURES                                                                     ##
###################################################################################################

#Notre modèle
ineq(PP_pred_glm)
rmse(base_glm_test$CHARGE_SINISTRE,PP_pred_glm)
our_mape(base_glm_test$CHARGE_SINISTRE,PP_pred_glm)

#Modèle naïf
ineq(mean(base_glm_test$CHARGE_SINISTRE))
rmse(base_glm_test$CHARGE_SINISTRE,mean(base_glm_test$CHARGE_SINISTRE))
our_mape(base_glm_test$CHARGE_SINISTRE,mean(base_glm_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 07 : PP ON REAL TEST                                                                     ##
###################################################################################################

charge_pred  = predict(glm_claims,newdata  = base_test,type="response") + min_charge -1 
nb_pred      = predict(glm_freq, newdata   = base_test,type="response")

PP_pred_test_glm  = charge_pred* nb_pred

sum(PP_pred_test_glm) 
sum(base_test$CHARGE_SINISTRE)

###################################################################################################
## STEP 08 : ERROR MEASURES  ON TEST                                                                   ##
###################################################################################################

#Notre modèle
ineq(PP_pred_test_glm)
rmse(base_test$CHARGE_SINISTRE,PP_pred_test_glm)
our_mape(base_test$CHARGE_SINISTRE,PP_pred_test_glm)

#Modèle naïf
ineq(mean(base_test$CHARGE_SINISTRE))
rmse(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))
our_mape(base_test$CHARGE_SINISTRE,mean(base_test$CHARGE_SINISTRE))

###################################################################################################
## STEP 09 : SAVE GLM                                                                   ##
###################################################################################################

save(glm_claims,file="01-OUTPUTS/MODEL/GLM_CLAIM.Rdata")
save(glm_freq,file="01-OUTPUTS/MODEL/GLM_FREQ.Rdata")

###################################################################################################
## STEP 10 : SAVE PRED                                                                   ##
###################################################################################################

save(PP_pred_test_glm,file="01-OUTPUTS/PREDICTIONS/PP_PRED_GLM.Rdata")
