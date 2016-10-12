#######################################################################################################
###################################### Functions for Pricing  #########################################
#######################################################################################################




######################################################################
## Step 00  - LIB
######################################################################
require(dplyr)
require(magrittr)
require(Matrix)
require(ggplot2)
require(ineq)
require(gtable)
require(grid)
require(gridExtra)
######################################################################
## Step 01  - Functions
######################################################################

## ---------------------------------------------------------------------------------

Log = function(x){
cat("-----------------", sep="\n")  
cat(x, sep="\n")
cat("-----------------", sep="\n")
}


## ---------------------------------------------------------------------------------

plot_Gini = function (list,names,number){
  
  for (i in 1:number)
  {
    base[[i]] <- data.frame(L = list[[i]]$L,
                            p = list[[i]]$p, 
                            Uprob = c(1:length(list[[i]]$L)/length(list[[i]]$L)),
                            Models=factor(names[i]))
  }
  base %<>% bind_rows()
  
  
  base %>% sample_frac(size=0.1) %>%
    ggplot() +
    geom_line(aes(x = Uprob, y = L, color=Models), size=1.1) + 
    geom_line(aes(x = p, y = p)) +
    xlab("Proportion of predicted severity") + ylab("Percentage ") + ggtitle("Lorenz curve") 
  
  
}

## ---------------------------------------------------------------------------------

Plot_Coeff = function(glm,data){
  
variables=as.vector(names(glm$x[]))
coefficients = data.frame( mod=  names(glm$coefficients),
                           est=  glm$coefficients )

intercept = coefficients %>% filter(mod=="(Intercept)") %>% dplyr::select(est)
intercept= exp(as.numeric(intercept))

coef<-list()
for (i in 1:length(variables)) {
        tmp=as.data.frame(prop.table(table(data[,variables[i]]))) %>% 
             mutate (mod=paste0(variables[i],Var1),
                     var=variables[i]) -> coef[[i]] }

tmp=rbind_all(coef)
tmp %>% left_join(coefficients) -> estimation
estimation %<>% mutate(est=ifelse(is.na(est),100,exp(est)*100)) %>%
                dplyr::select(-mod)

### 2. Generate the plots with different axis limits
ploti <-list()
for(i in 1: length(variables)) {
plotfunc(estimation, variables[i]) -> ploti[[i]] }

n <- length(ploti)

title= textGrob(paste0("Base Level = ",round(intercept,2)," \n"),gp=gpar(fontsize=20,font=3))
do.call("grid.arrange", list(grobs = ploti,nrow=1,top=title) )

}
plotfunc <- function(Data, xxx) {
  
  p1 = ggplot(data = subset(Data, var==xxx), aes(x=Var1,y=Freq))+
    geom_bar(stat="identity",fill="orange") + 
    facet_grid(.~ var, scales = "free") +
    
    geom_text(aes(label=paste0(round(Freq*100,1),"%"),col="red",vjust=-0.5))+
    theme(legend.position="none",axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("")
  
  p2 = ggplot(data = subset(Data, var==xxx),aes(x=Var1,y=est))+
    geom_line(aes(group=1),size=2)+
    geom_point(aes(x=Var1, y=est, group=var))+ 
    geom_label( aes(x=Var1,y=est,label=paste0(round(est,1),"%") ))+
    facet_grid(.~ var, scales = "free") +
    
    theme(legend.position="none",axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 0), 
          panel.background = element_rect(fill = NA), 
          panel.grid = element_blank()) +   xlab("")
  
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
} 

## ---------------------------------------------------------------------------------
our_mape <- function(y, yhat)
  mean(abs(((y+1) - (yhat+1))/(y+1)))



