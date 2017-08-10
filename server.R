function(input, output, session) {
  values <- reactiveValues(
    plots = list(),      # list of plots the user saved
    maindata = NULL,     # the data frame used throughout the app
    updatePlot = FALSE,  # whether to manually update the plot
    prevPlot = NULL    # the last plot that was successfully plotted
  )
  # Load user data
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    values$maindata <- read.csv(file, na.strings = c("NA","."))
  })
  
  # Load sample dataset
  observeEvent(input$sample_data_btn, {
    file <- "data/dfall.csv"
    values$maindata <- read.csv(file, na.strings = c("NA","."))
  })
  

  output$exposurevariables <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    choices<-     unique(df[,"paramname"])
    selectizeInput('exposurevariablesin',
                   label = "Exposure Variable(s)",
                   choices = choices,
                   selected = choices[1],
                   multiple=FALSE,
                   options = list(plugins = list('drag_drop')),
                   width = '8000px'
    )
  })
  
  output$covariates <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    df <- df %>%
      filter(paramname %in% c(input$exposurevariablesin))
    choices<-     unique(df[,"covname"])
    selectizeInput(  "covariatesin", "Covariates Top to Bottom (Remove/Drag and Drop to Desired Order):",
                     choices = choices,
                     selected = choices,
                     multiple=TRUE,
                     options = list(
                       placeholder = 'Please select one or more variables',
                       plugins = list('remove_button', 'drag_drop')),
                     width = '800px'
    )
    
  })
  
  output$covvalueorder <- renderUI({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    df <- df %>%
      filter(paramname %in% c(input$exposurevariablesin))%>%
      filter(covname %in% c(input$covariatesin))
    
    choices<-     as.character(unique(df[,"label"]))
    selectizeInput('covvalueorderin',
                   label = paste("Drag and Drop to Desired Order within facets","values"),
                   choices = choices,
                   selected = choices,
                   multiple=TRUE,
                   options = list(plugins = list('drag_drop')),
                   width = '8000px'
    )   
    
    
  })
  
  formatstats  <- reactive({
    df <-values$maindata
    validate(       need(!is.null(df), "Please select a data set"))
    
    df$covname<-factor(df$covname)
    df$label<-factor(df$label)
    df$ref<- 1
    df$exposurename<- df$paramname

    validate(need(length(input$covariatesin)>=1,
                  "Please select a least one covariate or Overall"))
    sigdigits<- input$sigdigits
    summarydata<- df %>%
      group_by(paramname,covname,label) %>%
      mutate(
        MEANEXP = median(mid),
        LOWCI = lower,
        UPCI =  upper,
        MEANLABEL=signif_pad(MEANEXP,sigdigits),
        LOWCILABEL =signif_pad(LOWCI,sigdigits),
        UPCILABEL =signif_pad(UPCI,sigdigits),
        LABEL= paste0(MEANLABEL," [",LOWCILABEL,"-",UPCILABEL,"]")
        
      )
    
    summarydata$covvalue <- factor(summarydata$label)
    summarydata <- summarydata %>%
      filter(covname %in% c(input$covariatesin))%>%
      filter(paramname == input$exposurevariablesin)
    summarydata<- as.data.frame(summarydata)
    summarydata
  })
  
  
  output$refarea <- renderUI({
    REF<- 1
    ymin<- REF*0.8
    ymax<- REF*1.25
    ymaxmax<- REF*2
    ystep <- 0.05
    sliderInput("refareain", "Reference Area", min=0, max=ymaxmax, value=c(ymin,ymax),step=ystep, animate = FALSE)
    
  })
  

  

  output$plot <- renderPlot({
             req(input$refareain)
             req(formatstats())
             req(input$height)
             validate(need(length(input$covariatesin)>=1,
                           "Please select a least one covariate or All"))
             
             
             summarydata <-  formatstats()
             summarydata [,"covname"] <- factor(summarydata [,"covname"], levels = c(input$covariatesin) )
             summarydata [,"label"]   <- factor(summarydata[,"label"]   , levels = c(input$covvalueorderin))
             facetformula <- "covname~."
             

               

             
             
             if (!input$customlegendtitle){
             
             p1<- ggplot(data=summarydata,aes(y=factor(label), x=mid, xmin=lower, xmax=upper)) +
               geom_pointrangeh(
                 position=position_dodgev(height=0.75),
                 aes(color="Median (points)\n95% CI (horizontal lines)"),
                 size=1,alpha=1,shape=16) + 
               facet_grid(covname~.,scales="free",space = "free",switch="both")+
               ylab("") + 
               theme_bw(base_size = 22)+
               theme(axis.text.y  = element_text(angle=0, vjust=1,size=24),
                     legend.position="top", 
                     legend.justification=c(0.5,0.5),
                     legend.direction="horizontal",
                     legend.key.width=unit(3,"line"),
                     strip.text= element_text( size=input$facettext),
                     panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
                     panel.grid.major= element_line(colour = "gray", linetype = "solid")
               )+
               ggtitle("\n")
             
             
             if(input$xaxistitle=="") {
               p1<- p1+
                 xlab(paste("Changes of",unique(summarydata$exposurename),"Relative to Reference" ))
             }
             if(input$xaxistitle!="") {
               p1<- p1+
                 xlab(input$xaxistitle)
             }
             if(input$yaxistitle!="") {
               p1<- p1+
                 ylab(input$yaxistitle)
             }
             if (input$customxticks) {
               p1<- p1+
                 scale_x_continuous(breaks = as.numeric(unique(unlist (strsplit(input$xaxisbreaks     , ","))) ),
                                    minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) )
                 )
             }

             
             p1<- p1 +
               annotate("rect",
                        xmin=input$refareain[1],
                        xmax=input$refareain[2],
                        ymin=-Inf, ymax=Inf,
                        col="grey",alpha=0.1)+
               geom_ribbon(x=1,ymax=1,ymin=1,aes(fill="Reference (vertical line)\nClinically relevant limits (colored area)"),size=1,alpha=0.2) +  # fake ribobn for fill legend
               
               geom_vline(aes(xintercept=ref,linetype="Reference (vertical line)\nClinically relevant limits (colored area)"),size=1) +
            
               scale_colour_manual(""  ,breaks  ="Median (points)\n95% CI (horizontal lines)",
                                   values ="blue")+
               scale_linetype_manual("",breaks  = c("Reference (vertical line)\nClinically relevant limits (colored area)"),
                                     values =2)+
               scale_fill_manual(""    ,breaks  = c("Reference (vertical line)\nClinically relevant limits (colored area)"),
                                 values ="grey")+

               guides(colour = guide_legend(order = 1))
             
             if(input$userxzoom){
               p1<- p1 +
                 coord_cartesian(xlim= c(input$lowerxin,input$upperxin)
                 )
             }
             }
             
             if (input$customlegendtitle){
               colourpos<-  which( input$legendordering=="pointinterval")
               fillpos      <-  which( input$legendordering=="area")
               linetypepos  <-  which( input$legendordering=="ref")
               
               if (input$combineareareflegend){
                 fillpos      <-  which( input$legendordering=="ref")
               }
               collegend <-  gsub("\\\\n", "\\\n", input$customcolourtitle)
               filllegend <- gsub("\\\\n", "\\\n", input$customfilltitle)
               linetypelegend <- gsub("\\\\n", "\\\n", input$customlinetypetitle)
               gcol  <- guide_legend("")
               if( length(colourpos)!=0) {
                 gcol  <- guide_legend("",order= colourpos)
               }
               gfill <- guide_legend("")
               if( length(fillpos)!=0) {
                 gfill <- guide_legend("",order = fillpos)
               } 
               glinetype <- guide_legend("")
               if( length(linetypepos)!=0) {
                 glinetype <- guide_legend("",order = linetypepos)
               }
               p1<- ggplot(data=summarydata,aes(y=factor(label), x=mid, xmin=lower, xmax=upper)) +
                 geom_pointrangeh(
                   position=position_dodgev(height=0.75),
                   aes(color=collegend),
                   size=1,alpha=1,shape=16) + 
                 facet_grid(covname~.,scales="free",space = "free",switch="both")+
                 ylab("") + 
                 theme_bw(base_size = 22)+
                 theme(axis.text.y  = element_text(angle=0, vjust=1,size=24),
                       legend.position="top", 
                       legend.justification=c(0.5,0.5),
                       legend.direction="horizontal",
                       legend.key.width=unit(3,"line"),
                       strip.text= element_text( size=input$facettext),
                       panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
                       panel.grid.major= element_line(colour = "gray", linetype = "solid")
                 )+
                 ggtitle("\n")
               
               
               if(input$xaxistitle=="") {
                 p1<- p1+
                   xlab(paste("Changes of",unique(summarydata$exposurename),"Relative to Reference" ))
               }
               if(input$xaxistitle!="") {
                 p1<- p1+
                   xlab(input$xaxistitle)
               }
               if(input$yaxistitle!="") {
                 p1<- p1+
                   ylab(input$yaxistitle)
               }
               if (input$customxticks) {
                 p1<- p1+
                   scale_x_continuous(breaks = as.numeric(unique(unlist (strsplit(input$xaxisbreaks     , ","))) ),
                                      minor_breaks = as.numeric(unique(unlist (strsplit(input$xaxisminorbreaks, ","))) )
                   )
               }
               
               
               p1<- p1 +
                 annotate("rect",
                          xmin=input$refareain[1],
                          xmax=input$refareain[2],
                          ymin=-Inf, ymax=Inf,
                          col="grey",alpha=0.1)+
                 geom_ribbon(x=1,ymax=1,ymin=1,aes(fill=filllegend),
                             size=1,alpha=0.2) +  # fake ribobn for fill legend
                 
                 geom_vline(aes(xintercept=ref,linetype=linetypelegend),size=1) +
                 
                 scale_colour_manual(""  ,breaks  =collegend,
                                     values ="blue")+
                 scale_linetype_manual("",breaks  = linetypelegend,
                                       values =2)+
                 scale_fill_manual(""    ,breaks  = filllegend,
                                   values ="grey")+
                 
                 guides(colour = guide_legend(order = 1))
               
               if(input$userxzoom){
                 p1<- p1 +
                   coord_cartesian(xlim= c(input$lowerxin,input$upperxin)
                   )
               }
             
               p1 <- p1+ guides(colour = gcol, linetype = glinetype, fill = gfill)
               
             }
             
             
             
             p2<- ggplot(data=summarydata, aes(y=factor(label) )) 
             p2<- p2+
               geom_text(aes(x =1,label =LABEL,hjust=0.5),size=input$textsize,position=position_dodgev(height=0.75))
             
             p2<- p2+
               facet_grid(covname~.,scales="free",space = "free")+
               theme_bw(base_size = 26) +
               theme(
                 strip.background = element_blank(),strip.text = element_blank(),
                 axis.title=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
                 panel.grid.major.x = element_blank(), panel.grid.minor.x= element_blank())+
               ylab("") + 
               xlab("") +
               xlim(c(0.99,1.01))+
               theme(legend.position="none")
             
             ggarrange(p1,p2,nrow=1,widths =c(input$plottotableratio,1))
           }, height =   function(){input$height})
           
   

           
           output$ui_plot <-  renderUI({                 
             plotOutput('plot',  height = input$height, width = "100%")
           })
           }