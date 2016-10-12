###################################################################################################
######################################## PTF MANIPULATION #########################################
###################################################################################################

# Last Update: 10/07/16

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

require(dplyr)
require(tidyr)
require(magrittr)
require(ggplot2)
require(ineq)
###################################################################################################
## STEP 01 : IMPORT                                                                           ##
###################################################################################################

pred4 = read.csv2("01-PREDICTIONS/Base_4.csv")
pred4 %<>% mutate(nu_affa = 1:n(),PP= P_pure_prorata_sur_periode,TYP="pred4") %>% dplyr::select(nu_affa,TYP,PP)

pred6 = read.csv2("01-PREDICTIONS/Base_6.csv",dec = ".")
pred6 %<>% filter(nu_affa != 74089) %>%
           mutate(nu_affa = 1:n(),PP= Premium.with.Exposure.,TYP="pred6") %>% dplyr::select(nu_affa,TYP,PP) 

pred  = read.csv("01-PREDICTIONS/PRED_DGH.csv")
pred %<>% filter(nu_affa < 15554) %>%
           mutate(PP= Premium,TYP="pred5") %>% dplyr::select(nu_affa,TYP,PP) 

pred_global = rbind(pred4,pred6,pred)

pred_global %>% ggplot() + geom_density(aes(x= PP,colour=TYP))+ scale_x_log10()


pred_global %>% dplyr::select(TYP,PP) %>% group_by(TYP) %>% summarize_each(funs(mean,sd,sum,ineq))


###################################################################################################
## STEP 01 : IMPORT                                                                           ##
###################################################################################################

pred_spread= pred_global %>% spread(TYP,PP)
pred_spread %>% ggplot() + geom_point(aes(x=pred5,y=pred6)) + geom_abline() + scale_x_log10()+ scale_y_log10()
