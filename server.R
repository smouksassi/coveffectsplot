function(input, output, session) {
  values <- reactiveValues(maindata = NULL)
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    values$maindata <- read.csv(file, na.strings = c("NA", "."))
  })
  observeEvent(input$sample_data_btn, {
    file <- "data/dfall.csv"
    values$maindata <- read.csv(file, na.strings = c("NA", "."))
  })
  
  
  output$exposurevariables <- renderUI({
    df <- values$maindata
    req(df)
    choices <-     unique(df[, "paramname"])
    selectizeInput(
      'exposurevariablesin',
      label = "Exposure Variable(s)",
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      options = list(plugins = list('remove_button', 'drag_drop')),
      width = '8000px'
    )
  })
  
  output$covariates <- renderUI({
    df <- values$maindata
    req(df)
    df <- df %>%
      filter(paramname %in% c(input$exposurevariablesin))

    
    choices <-     unique(df[, "covname"])
    selectizeInput(
      "covariatesin",
      "Covariates Top to Bottom (Remove/Drag and Drop to Desired Order):",
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(
        placeholder = 'Please select one or more variables',
        plugins = list('remove_button', 'drag_drop')
      ),
      width = '800px'
    )
  })
  
  output$covvalueorder <- renderUI({
    df <- values$maindata
    req(df)
    df <- df %>%
      filter(paramname %in% c(input$exposurevariablesin)) %>%
      filter(covname %in% c(input$covariatesin))
    
    choices <-     as.character(unique(df[, "label"]))
    selectizeInput(
      'covvalueorderin',
      label = paste("Drag and Drop to Desired Order within facets", "values"),
      choices = choices,
      selected = choices,
      multiple = TRUE,
      options = list(plugins = list('remove_button', 'drag_drop')),
      width = '8000px'
    )
  })
  
  formatstats  <- reactive({
    df <- values$maindata
    req(df)
    validate(need(
      length(input$covariatesin) >= 1,
      "Please select a least one covariate or All"
    ))
    validate(need(
      length(input$covvalueorderin) >= 1,
      "Please select a least one covariate/All level"
    ))
    df$covname <- factor(df$covname)
    df$label <- factor(df$label)
    df$ref <- 1
    df$exposurename <- df$paramname
    sigdigits <- input$sigdigits
    summarydata <- df %>%
      group_by(paramname, covname, label) %>%
      mutate(
        MEANEXP = mid,
        LOWCI = lower,
        UPCI =  upper,
        MEANLABEL = signif_pad(MEANEXP, sigdigits),
        LOWCILABEL = signif_pad(LOWCI, sigdigits),
        UPCILABEL = signif_pad(UPCI, sigdigits),
        LABEL = paste0(MEANLABEL, " [", LOWCILABEL, "-", UPCILABEL, "]")
       )
    
    summarydata$covvalue <- factor(summarydata$label)
    summarydata <- summarydata %>%
      filter(covname %in% c(input$covariatesin)) %>%
      filter(paramname %in% input$exposurevariablesin)
    summarydata <- as.data.frame(summarydata)
    summarydata
  })
  
  
  output$refarea <- renderUI({
    REF <- 1
    ymin <- REF * 0.8
    ymax <- REF * 1.25
    ymaxmax <- REF * 5
    ystep <- 0.05
    sliderInput(
      "refareain",
      "Reference Area",
      min = 0,
      max = ymaxmax,
      value = c(ymin, ymax),
      step = ystep,
      animate = FALSE
    )
    
  })
  observeEvent(input$colourpointrangereset, {
    shinyjs::reset("colourpointrange")
  })
  
  observeEvent(input$stripbackfillreset, {
    shinyjs::reset("stripbackgroundfill")
  })
  observeEvent(input$fillrefareareset, {
    shinyjs::reset("fillrefarea")
  })
  
  plotdataprepare  <- reactive({
req(formatstats())
    summarydata <-  formatstats()
    summarydata [, "covname"] <-
      factor(summarydata [, "covname"], levels = c(input$covariatesin))
    summarydata [, "label"]   <-
      factor(summarydata[, "label"]   , levels = c(input$covvalueorderin))
    summarydata <- summarydata %>%
      filter(label %in% c(input$covvalueorderin))
    
    summarydata [, "paramname"]   <-
      factor(summarydata[, "paramname"]   , levels = c(input$exposurevariablesin))
    
    summarydata
  })
  
  output$plot <- renderPlot({
    summarydata <- plotdataprepare()
    req(input$refareain)
    req(plotdataprepare())
    req(input$height)
    req(summarydata)
    
    facetformula<- as.formula(input$facetformula)

    if(input$facettextx==0){
      x.strip.text= element_blank()
    }
    if(input$facettextx>0){
      x.strip.text= element_text( size=input$facettextx)
    }
    
    if(input$facettexty==0){
      y.strip.text= element_blank()
    }
    if(input$facettexty>0){
      y.strip.text= element_text( size=input$facettexty)
    }
    
    
    
    p1 <-
      ggplot(data = summarydata, aes(
        y = factor(label),
        x = mid,
        xmin = lower,
        xmax = upper
      )) 

      colourpos <-  which(input$legendordering == "pointinterval")
      fillpos      <-  which(input$legendordering == "area")
      linetypepos  <-  which(input$legendordering == "ref")
      shapepos  <-  which(input$legendordering == "shape")
      

      if (input$combineareareflegend) {
        fillpos      <-  which(input$legendordering == "ref")
      }
      collegend <-
        gsub("\\\\n", "\\\n", input$customcolourtitle)
      filllegend <-
        gsub("\\\\n", "\\\n", input$customfilltitle)
      linetypelegend <-
        gsub("\\\\n", "\\\n", input$customlinetypetitle)
      gcol  <- guide_legend("")
      if (length(colourpos) > 0) {
        gcol  <- guide_legend("", order = colourpos)
      }
      gfill <- guide_legend("")
      if (length(fillpos) > 0) {
        gfill <- guide_legend("", order = fillpos)
      }
      glinetype <- guide_legend("")
      if (length(linetypepos) > 0) {
        glinetype <- guide_legend("", order = linetypepos)
      }
      gshape <- guide_legend("")
      if (length(shapepos) > 0) {
        gshape <- guide_legend("", order = shapepos,override.aes = list(linetype = 0,colour="gray") )
      }
      
        p1 <- p1 +
        geom_pointrangeh(
          position = position_dodgev(height = 0.75),
          aes(color = collegend),
          size = 1,
          alpha = 1
        ) 

      if (input$showrefarea) {
        p1 <- p1 +
          annotate(
            "rect",
            xmin = input$refareain[1],
            xmax = input$refareain[2],
            ymin = -Inf,
            ymax = Inf,
            fill = input$fillrefarea
          ) +
          geom_ribbon(
            x = 1,
            ymax = 1,
            ymin = 1,
            aes(fill = filllegend),
            size = 1
          )  # fake ribbon for fill legend
      }
      p1 <- p1 +
        geom_vline(aes(xintercept = ref, linetype = linetypelegend), size =
                     1) +
        
        scale_colour_manual(""  ,
                            breaks  = collegend,
                            values = input$colourpointrange) +
        scale_linetype_manual("", breaks  = linetypelegend,
                              values = 2) +
        scale_fill_manual(""    , breaks  = filllegend,
                          values = input$fillrefarea) +
        guides(colour = guide_legend(order = 1))
      p1 <-
        p1 + guides(colour = gcol,
                    linetype = glinetype,
                    fill = gfill,
                    shape=gshape)
      if (!input$showrefarea) {
        p1 <- p1 + guides(colour = gcol,
                          linetype = glinetype,
                          shape=gshape,
                          fill = NULL)
        
      }
      p1 <- p1+
        aes(group=paramname)
    
      if(input$shapebyparamname) {
        p1 <- p1+
          aes(shape=paramname)
        }

      
      
      if(input$facetswitch!="none"){
        p1<- p1+
          facet_grid(facetformula,scales=input$facetscales,space = input$facetspace,switch=input$facetswitch)
      }
    if(input$facetswitch=="none"){
      p1<- p1+
        facet_grid(facetformula,scales=input$facetscales,space = input$facetspace,switch=NULL)
    }
    
      
    p1 <- p1+
      ylab("") +
      theme_bw(base_size = 22) +
      theme(
        axis.text.y  = element_text(
          angle = 0,
          vjust = 1,
          size = input$ylablesize
        ),
        axis.text.x  = element_text(size = input$xlablesize),
        legend.position = "top",
        legend.justification = c(0.5, 0.5),
        legend.direction = "horizontal",
        legend.key.width = unit(3, "line"),
        strip.text.x = x.strip.text,
        strip.text.y = y.strip.text,
        panel.grid.minor = element_line(colour = "gray", linetype = "dotted"),
        panel.grid.major = element_line(colour = "gray", linetype = "solid"),
        strip.background = element_rect(fill = input$stripbackgroundfill),
        strip.placement  = input$stripplacement
      ) +
      ggtitle("\n")
     if (input$xaxistitle == "") {
      p1 <- p1 +
        xlab(paste(
          "Changes of Parameter",
          "Relative to Reference"
        ))
    }
    if (input$xaxistitle != "") {
      p1 <- p1 +
        xlab(input$xaxistitle)
    }
    if (input$yaxistitle != "") {
      p1 <- p1 +
        ylab(input$yaxistitle)
    }
    if (input$customxticks) {
      p1 <- p1 +
        scale_x_continuous(breaks = as.numeric(unique(unlist (
          strsplit(input$xaxisbreaks     , ",")
        ))),
        minor_breaks = as.numeric(unique(unlist (
          strsplit(input$xaxisminorbreaks, ",")
        ))))
    }
    if (input$userxzoom) {
      p1 <- p1 +
        coord_cartesian(xlim = c(input$lowerxin, input$upperxin))
    }
    
    p2 <- ggplot(data = summarydata, aes(y = factor(label)))
    p2 <- p2 +
      aes(group=paramname)+
      geom_text(
        aes(
          x = 1,
          label = LABEL,
          hjust = 0.5
        ),
        size = input$textsize,
        position = position_dodgev(height = 0.75)
      )
    

    if(input$facetswitch!="none"){
      p2 <- p2 +
        facet_grid(facetformula,scales=input$facetscales,space = input$facetspace,switch=input$facetswitch)
    }
    if(input$facetswitch=="none"){
      p2 <- p2 +
        facet_grid(facetformula,scales=input$facetscales,space = input$facetspace,switch=NULL)
    }
    
    p2 <- p2 +
      theme_bw(base_size = 26) +
      theme(
       
        strip.text.x = x.strip.text,
        strip.text.y = y.strip.text,
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(fill = input$stripbackgroundfill)
      ) +
      ylab("") +
      xlab("") +
      xlim(c(0.99, 1.01)) +
      theme(legend.position = "none")
    
    if ( !input$showtablefacetstrips) {
      p2 <- p2 +
        theme(
          strip.text = element_blank(),
          strip.background = element_blank())
      
    }
    

    if ( input$tableposition=="on the right") {
      ggarrange(p1,
                p2,
                nrow = 1,
                widths = c(input$plottotableratio, 1))
      
    }

    if ( input$tableposition=="below") {
      ggarrange(p1,
                p2,
                nrow = 2,
                heights = c(input$plottotableratio, 1))
      
    }
    
    
    
    
    
  }, height =   function() {
    input$height
  })
  
  
  
  
  output$ui_plot <-  renderUI({
    plotOutput('plot',  height = input$height, width = "100%")
  })
}