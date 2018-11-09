require(ggplot2)
require(ggstance)
require(egg)



interactiveforest <- function(forestdata=plotdata,
                              xaxisparamname="param",xlabtext=NULL,
                              yaxistitlesize=16,xaxistitlesize=16,
                              striptextsize=13,
                              REF= 1,REFmin= 0.8,REFmax= 1.25,
                              legendreftext=paste("Reference (vertical line)\n","Clinically relevant limits (colored area)",sep=""),
                              show.relevanceareainplot=TRUE,show.relevanceareainlegend=TRUE,
                              facetformula="covname~paramname",
                              shapebyparamname=FALSE,
                              facetscales="free_y",facetspace="fixed",
                              switch="y")
{
  forestdata=forestdata
  xaxistitlesize<-  xaxistitlesize
  yaxistitlesize<- yaxistitlesize
  striptextsize <- striptextsize
  if(!is.null(legendreftext)){
    legendreftext<- legendreftext
  }
  if(is.null(legendreftext)){
    legendreftext<- paste("Reference (vertical line)\n","Clinically relevant limits (colored area)")
  }
  if(is.null(xlabtext)){
    xlabtext<- paste("Changes of",xaxisparamname,"Relative to Reference" )
  }
  if(!is.null(xlabtext)){
    xlabtext<- xlabtext
  }
  facetformula<- as.formula(facetformula)
  
  
  p1<-  ggplot(data=forestdata,aes(y=factor(label),
                                   x=mid, xmin=lower, xmax=upper)) +
    geom_pointrangeh(
      position=position_dodgev(height=0.75),
      aes(color="Median (points)\n95% CI (horizontal lines)"),
      size=1,alpha=1)
  
  if(switch!="none"){
    p1<- p1+
      facet_grid(facetformula,scales=facetscales,space = facetspace,switch=switch)
  }
  if(switch=="none"){
    p1<- p1+
      facet_grid(facetformula,scales=facetscales,space = facetspace)
  }
  p1<- p1+
    ylab("") + 
    theme_bw(base_size = 22)+
    theme(axis.text.y  = element_text(angle=0, size=yaxistitlesize),
          axis.text.x  = element_text(size=xaxistitlesize),
          legend.position="top", 
          legend.justification=c(0.5,0.5),
          legend.direction="horizontal",
          legend.key.width=unit(3,"line"),
          strip.text= element_text( size=striptextsize),
          panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
          panel.grid.major= element_line(colour = "gray", linetype = "solid")
    )+
    ggtitle("\n")+
    xlab(xlabtext)
  
  if(show.relevanceareainplot){
    p1<-  p1+
      annotate("rect",
               xmin=REFmin,
               xmax=REFmax,
               ymin=-Inf, ymax=Inf,
               col="grey",alpha=0.1)
  }
  
  if(show.relevanceareainlegend){
    p1<-  p1+
      geom_ribbon(x=1,ymax=1,ymin=1,
                  aes(
                    fill=legendreftext),
                  size=1,alpha=0.2)   # fake ribbon for fill legend
  }
  
gshape <- guide_legend("",override.aes = list(linetype = 0,colour="gray") )

  p1<-  p1+
    geom_vline(aes(xintercept=REF,
                   linetype=legendreftext),
               size=1) +
    scale_colour_manual(""  ,breaks  ="Median (points)\n95% CI (horizontal lines)",
                        values ="blue")+
    scale_linetype_manual("",breaks  = c(legendreftext),
                          values =2)+
    scale_fill_manual(""    ,breaks  = c(legendreftext),
                      values ="grey")+
    guides(colour = guide_legend(order = 1),shape=gshape)
  
  p1 <- p1+
    aes(group=paramname)
  
  if(shapebyparamname) {
    p1 <- p1+
      aes(shape=paramname)
  }
  
  (p1)
}

interactivetableplot<- function (forestdata=plotdata,
                                 textsize=6,xaxistitlesize=14,striptextsize=14,yaxistitlesize=14,
                                 facetformula="covname~paramname",
                                 facetscales="free_y",facetspace="fixed",
                                 remove.ylabels=TRUE,remove.xlabels=TRUE,xlim=c(0.5,1.5),
                                 switch="y")
{
  forestdata=forestdata
  if(striptextsize==0){
    strip.text= element_blank()
  }
  if(striptextsize>0){
    strip.text= element_text( size=striptextsize)
  }
  facetformula<- as.formula(facetformula)
  
  p2<- ggplot(data=forestdata, aes(y=factor(label) )) +
    geom_text(aes(x =1,label =LABEL,hjust=0.5),size=textsize,
              position=position_dodgev(height=0.75))
  
  if(switch!="none"){
    p2<- p2+
      facet_grid(facetformula,scales=facetscales,space = facetspace,switch=switch)
  }
  if(switch=="none"){
    p2<- p2+
      facet_grid(facetformula,scales=facetscales,space = facetspace)
  }
  p2<- p2+
    theme_bw(base_size = 22)+
    theme(axis.text.y  = element_text(angle=0, size=yaxistitlesize),
          axis.text.x  = element_text(size=xaxistitlesize),
          legend.position="top", 
          legend.justification=c(0.5,0.5),
          legend.direction="horizontal",
          legend.key.width=unit(3,"line"),
          strip.text= strip.text,
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank() , panel.grid.minor.y = element_blank(),
          axis.title=element_blank()
    )
  if(remove.ylabels){
    p2<-p2+
      theme(axis.text.y=element_blank(),axis.ticks.y  =element_blank()
      )
  }
  if(remove.xlabels){
    p2<-p2+
      theme(axis.text.x=element_blank(),axis.ticks.x  =element_blank()
      )
  }
  # theme(
  #    strip.background = element_blank(),strip.text = element_blank(),
  #   axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
  #    panel.grid.major.x = element_blank(), panel.grid.minor.x= element_blank())+
  p2<-p2+
    ylab("") + 
    xlab("") +
    xlim(xlim)+
    theme(legend.position="none")
  
  p2 <- p2+
    aes(group=paramname)
  
  (p2)
}
