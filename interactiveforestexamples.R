source("interactiveforest.R")

plotdata<- read.csv("forest-plot-table.csv")
plotdata <- plotdata %>% 
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))


param <- "Midazolam AUC"
plotdata <-  filter(plotdata,paramname==param)
plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
plotdata$label <- reorder(plotdata$label,plotdata$scen)

ggarrange(interactiveforest(plotdata,
                            legendreftext="Reference (vertical line)",
                            xlabtext=paste("Fold Change in",param,"Relative to Reference"),
                            show.relevanceareainlegend=FALSE,
                            show.relevanceareainplot=FALSE,
                            facetformula="covname~.",
                            facetscales="free_y",facetspace="free_y"),
          interactivetableplot(plotdata,striptextsize=0,facetspace="free_y"), 
          nrow=1, widths = c(4,1))


plotdata<- read.csv("forest-plot-table.csv")
plotdata <- plotdata %>% 
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
param <- c("Midazolam AUC","Midazolam Cmax")
plotdata <-  filter(plotdata,paramname%in%param)
plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
plotdata$label <- reorder(plotdata$label,plotdata$scen)

ggarrange(interactiveforest(plotdata,
                            legendreftext="Reference (vertical line)",
                            xlabtext=paste("Fold Change in",param,"Relative to Reference"),
                            show.relevanceareainlegend=FALSE,
                            show.relevanceareainplot=FALSE,
                            facetformula="covname~paramname",
                            facetscales="free_y",facetspace="free_y"),
          interactivetableplot(plotdata,striptextsize=14,
                               facetscales="free_y",facetspace="free_y",remove.ylabels=FALSE,xlim=c(0.25,2.5)), 
          nrow=2, heights =  c(1,1))

###


plotdata<- read.csv("forestplotdatacpidata.csv")
ggarrange(
interactiveforest(plotdata,REF= 1,REFmin= 0.8,REFmax= 1.20,striptextsize=13,
                 legendreftext="Reference (vertical line)\n+/- 20% limits (colored area)",
                 xlabtext=paste("Fold Change Relative to RHZE"),
                 facetformula="covname~paramname")
,
interactivetableplot(plotdata,striptextsize=13,remove.ylabels=FALSE)
,nrow=2)



plotdata<- read.csv("./data/dfall.csv")
plotdata <- plotdata %>% 
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
plotdata <- plotdata %>%
  filter(paramname%in%c("CL"))


ggarrange(
  interactiveforest(plotdata,REF= 1,REFmin= 0.8,REFmax= 1.20,striptextsize=13,
                    legendreftext="Reference (vertical line)\n+/- 20% limits (colored area)",
                    xlabtext=paste("Fold Change Relative to RHZE"),
                    facetformula="covname~.")
  ,
  interactivetableplot(plotdata,striptextsize=13,remove.ylabels=TRUE,switch="none",
                       facetformula="covname~.")+
    theme(    strip.background = element_blank(),strip.text = element_blank(),
              axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
      panel.grid.major.x = element_blank(), panel.grid.minor.x= element_blank())
  ,nrow=1,
  widths = c(4, 1))

########
plotdata<- read.csv("dataforest.csv")
plotdata <- plotdata %>% 
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
plotdata <- plotdata %>%
  filter(covname%in%c("Baseline TTR","Weight"))

egg::ggarrange(
  interactiveforest(plotdata,REF= 1,REFmin= 0.8,REFmax= 1.20,striptextsize=13,
                    legendreftext="Reference (vertical line)\n+/- 20% limits (colored area)",
                    xlabtext=paste("Fold Change Relative to Parameter"),
                    facetformula="covname~paramname",facetscales="free")
  ,
  interactivetableplot(plotdata,striptextsize=13,remove.ylabels=FALSE,switch="both",
                       facetformula="covname~paramname")
  ,nrow=2)

