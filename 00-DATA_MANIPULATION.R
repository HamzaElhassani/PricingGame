###################################################################################################
######################################## PTF MANIPULATION #########################################
###################################################################################################

###################################################################################################
## STEP 00 : LIBRARY                                                                             ##
###################################################################################################

load("00-DATA/pricinggame.RData")

###################################################################################################
## STEP 01 : PTF MANIPULATION                                                                    ##
###################################################################################################
l1 <- c("Creation_Entr","Zone","Segment","FORMULE","Activite","Type_Apporteur","ValeurPuissance","franchise")

ptf = train_contrats %<>% filter(FORMULE !=2) %>%
            mutate( KEY                  = paste(IMMAT,Date_Deb_Situ,Date_Fin_Situ,sep= "::"),
                    Exposition_au_risque = as.numeric(as.character(Exposition_au_risque)),
                    Zone                 = ifelse( Zone %in% c("5","6"),"5-6",Zone ),
                    franchise            = ifelse( franchise %in% c("1. 0","2. 1 - 200"),"1. <200",
                                           ifelse(franchise %in% c("5. 401 - 600","6. > 600"),"5. > 401",
                                                   franchise)),
                    ValeurPuissance      = ifelse( ValeurPuissance %in% c(1,2,3),"1-2-3",
                                           ifelse( ValeurPuissance %in% c(9,10,11),"9-10-11",
                                                   as.character(ValeurPuissance)))) %>%
                  mutate_each_(funs(factor), l1) 
save(ptf,file="00-DATA/ptf.Rdata")

###################################################################################################
## STEP 02 : CLAIMS MANIP                                                                 ##
###################################################################################################
sinistres_ = train_sinistres %>% mutate(KEY = paste(IMMAT,Date_Deb_Situ,Date_Fin_Situ,sep= "::"))  %>%
                                inner_join(ptf,by=c("KEY"="KEY")) 
save(sinistres_,file="00-DATA/sinistres_.Rdata")

sinistres = train_sinistres %>%
  mutate(KEY = paste(IMMAT,Date_Deb_Situ,Date_Fin_Situ,sep= "::")) %>%
  group_by(KEY) %>%
  summarize(NB_SINISTRE    = n(),
            CHARGE_SINISTRE = sum(CHARGE_SINISTRE))


###################################################################################################
## STEP 02 : CLAIMS MANIP                                                                        ##
###################################################################################################

base = ptf %>% left_join(sinistres,by=c("KEY"="KEY")) %>% 
                  mutate(NB_SINISTRE = ifelse(is.na(NB_SINISTRE),0,NB_SINISTRE),
                         CHARGE_SINISTRE = ifelse(is.na(CHARGE_SINISTRE),0,CHARGE_SINISTRE))
save(base,file="00-DATA/base.Rdata")


###################################################################################################
## STEP 03 : BASE TEST                                                                           ##
###################################################################################################
#Definition des contrats
set.seed(2016)
dummy<-sample(rep(0:1),size=length(unique(base$Num_contrat)),replace=T,prob=c(0.80,0.20))
num_contrat_train<-unique(base$Num_contrat)[dummy==0]
num_contrat_test<-unique(base$Num_contrat)[dummy==1]

#base
base_train<-base[base$Num_contrat %in% num_contrat_train,]
base_test<-base[base$Num_contrat %in% num_contrat_test,]

#sinistre_
sinistres_train<-sinistres_[sinistres_$Num_contrat.y %in% num_contrat_train,]
sinistres_test<-sinistres_[sinistres_$Num_contrat.y %in% num_contrat_test,]

save(base_train,file="00-DATA/base_train.Rdata")
save(base_test,file="00-DATA/base_test.Rdata")
save(sinistres_train,file="00-DATA/sinistres_train.Rdata")
save(sinistres_test,file="00-DATA/sinistres_test.Rdata")
