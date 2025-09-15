function(input, output, session) {
  maindata <- reactiveVal(NULL)
  
  # If this app was launched from a function that explicitly set an initial dataset
  if (exists("coveffectsplot_initdata")) {
    maindata(get("coveffectsplot_initdata"))
  }
  
  # Set the data source
  observeEvent(input$datafile, {
    file <- input$datafile$datapath
    maindata(read.csv(file, na.strings = c("NA", ".")))
  })
  observeEvent(input$sample_data_btn, {
    data <- get_sample_data()
    maindata(data)
  })

  # Show inputs once the data source exists
  observeEvent(maindata(), once = TRUE, {
    shinyjs::show("exposurevariables")
    shinyjs::show("covariates")
    shinyjs::show("covvalueorder")
    shinyjs::show("shapebyparamname")
    shinyjs::show("colourbyparamname")
    #shinyjs::show("staplearrow")
    shinyjs::show("vdodgeheight")
    shinyjs::show("get_code")
  })

  # Update the options in different inputs based on data
  observe({
    df <- maindata()
    shiny::req(df)
    choices <- unique(df[["paramname"]])
    updateSelectizeInput(session, "exposurevariables",
                         choices = choices, selected = choices[1])
  })
  observe({
    df <- maindata()
    shiny::req(df)
    df <- df %>%
      filter(paramname %in% c(input$exposurevariables))
    choices <- unique(df[["covname"]])
    updateSelectizeInput(session, "covariates",
                         choices = choices, selected = choices)
  })
  observe({
    df <- maindata()
    shiny::req(df)
    df <- df %>%
      filter(paramname %in% c(input$exposurevariables)) %>%
      filter(covname %in% c(input$covariates))
    choices <- as.character(unique(df[["label"]]))
    updateSelectizeInput(session, "covvalueorder",
                         choices = choices, selected = choices)
  })

  formatstats <- metaReactive2({
    shiny::req(maindata())
    validate(need(
      length(input$exposurevariables) >= 1,
      "Please select a least one exposure variable"
    ))
    validate(need(
      length(input$covariates) >= 1,
      "Please select a least one covariate or All"
    ))
    validate(need(
      length(input$covvalueorder) >= 1,
      "Please select a least one covariate value/All level"
    ))
    df <- maindata()
    metaExpr({
      df$covname <- factor(df$covname)
      df$label <- factor(df$label)
      df$exposurename <- df$paramname
      sigdigits <- ..(input$sigdigits)
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
        filter(covname %in% c(..(input$covariates))) %>%
        filter(paramname %in% ..(input$exposurevariables))
      summarydata <- as.data.frame(summarydata)
      summarydata
    })
  })


  output$refarea <- renderUI({
    REF <- ifelse(is.na(input$refvalue),1,input$refvalue)
    ymin <-  0.8
    ymax <-  1.25
    ymaxmax <- REF * ymax *3
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
  outputOptions(output, "refarea", suspendWhenHidden=FALSE)
  
  output$userdefinedcolorui <- renderUI({
    df <- maindata()
    shiny::req(df)
    cols <- c("#4682ac", "#ee3124", "#fdbb2f", "#6d405d",
              "#093b6d", "#2f71fd", "#336343", "#803333", "#279594", "#ef761b")

    if(input$colourbyparamname && length(input$exposurevariables) >0){
      lev <- 1:length(input$exposurevariables)
      lapply(seq_along(lev), function(i) {
        div(
          colourpicker::colourInput(inputId = paste0("col", lev[i]),
                                    label = paste0("Parameter Color", lev[i]),
                                    value = cols[i],
                                    showColour = "both",allowTransparent=TRUE, returnName = TRUE
          ), style = "display: inline-block;")
      })
    }
  })

  outputOptions(output, "userdefinedcolorui", suspendWhenHidden=FALSE)
  
  output$userdefinedshapeui <- renderUI({
    df <- maindata()
    shiny::req(df)
    #shapes <- c(16, 17, 15, 3, 7, 8,0,1,2,4,5,6,9,10,11,12,13,14,18,19,20,21,22,23,24,25)
    shapes = c("circle small", "triangle" ,"square","plus","square cross","asterisk",
                "square open","cross","diamond open","triangle down open","square cross",
                "diamond plus","circle plus","star","star","square plus","circle cross",
                "square triangle","diamond","circle","bullet",
                "circle filled","square filled","diamond filled","triangle filled",
                "triangle down filled","none")
    if(input$shapebyparamname && length(input$exposurevariables) >0){
      lev <- 1:length(input$exposurevariables)
      lapply(seq_along(lev), function(i) {
        div(
          selectInput(inputId = paste0("shape", lev[i]),
                                    label = paste0("Parameter Shape", lev[i]),
                                    choices = shapes,
                                    selected = shapes[i]),
          style = "display: inline-block;")
      })
    }
  })
  outputOptions(output, "userdefinedshapeui", suspendWhenHidden=FALSE)
  

  observeEvent(input$colourpointrangereset, {
    shinyjs::reset("colourpointrange")
  })
  observeEvent(input$colourbsvrangereset, {
    shinyjs::reset("colourbsvrange")
  })
  observeEvent(input$stripbackfillreset, {
    shinyjs::reset("stripbackgroundfill")
  })
  observeEvent(input$fillrefareareset, {
    shinyjs::reset("fillrefarea")
  })
  observeEvent(input$colorrefvaluereset, {
    shinyjs::reset("colorrefvalue")
  })

  plotdataprepare  <- metaReactive2({
    shiny::req(formatstats())
    metaExpr({
      summarydata <-  ..(formatstats())
      summarydata [, "covname"] <-
        factor(summarydata [, "covname"], levels = c(..(input$covariates)))
      summarydata [, "label"]   <-
        factor(summarydata[, "label"]   , levels = c(..(input$covvalueorder)))
      summarydata <- summarydata %>%
        filter(label %in% c(..(input$covvalueorder)))
  
      summarydata [, "paramname"]   <-
        factor(summarydata[, "paramname"]   , levels = c(..(input$exposurevariables)))
  
      summarydata
    })
  })

  output$plot <- metaRender2(renderPlot, {
    shiny::req(plotdataprepare())
    shiny::req(length(input$exposurevariables)>=1)
    if(input$colourbyparamname)   shiny::req(input$col1)
    if(input$shapebyparamname)   shiny::req(input$shape1)
    
    major_x_ticks <- NULL
    minor_x_ticks <- NULL
    major_x_labels <- NULL
    if (input$customxticks) {
      tryCatch({
         major_x_ticks <- as.numeric(unique(unlist(strsplit(input$xaxisbreaks, ",")[[1]])))
      }, warning = function(w) {}, error = function(e) {})
      tryCatch({
        minor_x_ticks <- as.numeric(unique(unlist(strsplit(input$xaxisminorbreaks, ",")[[1]])))
      }, warning = function(w) {}, error = function(e) {})
    }
    if (input$customxticks && input$customxlabels) {
      tryCatch({
        major_x_labels <- as.character(unique(unlist(strsplit(input$xaxislabels, ",")[[1]])))
      }, warning = function(w) {}, error = function(e) {})
    }
    x_range <- if (input$userxzoom) c(input$lowerxin, input$upperxin) else NULL
    ref_value <- if (is.na(input$refvalue)) 1 else input$refvalue
    
    ncols <- length(input$exposurevariables)
    
    if(!input$colourbyparamname){
      paramcols <- input$colourpointrange
    }
    if(input$colourbyparamname){
      paramcols <- paste0("c(", paste0("input$col", 1:ncols, collapse = ", "), ")")
      paramcols <- eval(parse(text = paramcols))
    }

    if(!input$shapebyparamname){
      paramshapes <- input$shapepointrange
    }
    if(input$shapebyparamname){
      paramshapes <- paste0("c(", paste0("input$shape", 1:ncols, collapse = ", "), ")")
      paramshapes <- eval(parse(text = paramshapes))
    }
    #staplearrow<- ifelse(input$staplearrow,"staple","none")

    metaExpr({
      summarydata <- ..(plotdataprepare())
      paramcols   <- ..(paramcols)
      paramshapes <- ..(paramshapes)
      #staplearrow <- ..(staplearrow)
    plot <- forest_plot(
        data = summarydata,
        facet_formula = ..(input$facetformula),
        xlabel = ..(input$xaxistitle),
        ylabel = ..(input$yaxistitle),
        x_facet_text_size = ..(input$facettextx),
        y_facet_text_size = ..(input$facettexty),
        x_facet_text_angle = ..(input$facettextxangle),
        y_facet_text_angle = ..(input$facettextyangle),
        x_facet_text_vjust = ..(input$x_facet_text_vjust),
        y_facet_text_vjust = ..(input$y_facet_text_vjust),
        x_facet_text_hjust = ..(input$x_facet_text_hjust),
        y_facet_text_hjust = ..(input$y_facet_text_hjust),
        x_facet_text_col   = ..(input$x_facet_text_col),
        y_facet_text_col   = ..(input$y_facet_text_col),
        xy_facet_text_bold = ..(input$boldfacettext),
        x_label_text_size = ..(input$xlablesize),
        y_label_text_size = ..(input$ylablesize),
        legend_title_size = ..(input$legendtitlesize),
        break_ylabel = ..(input$breakylabel),
        y_label_text_width= ..(input$ylabeltextwidth),
        table_text_size = ..(input$tabletextsize),
        table_text_colour_overwrite = ..(input$tabletextcoloroverwrite),
        table_text_colour = ..(input$tabletextcolour),
        base_size = ..(input$base_size),
        theme_benrich = ..(input$theme_benrich),
        table_title = escape_newline(..(input$custom_table_title)),
        table_title_size = ..(input$table_title_size),
        ref_legend_text = escape_newline(..(input$customlinetypetitle)),
        area_legend_text = escape_newline(..(input$customfilltitle)),
        interval_legend_text = escape_newline(..(input$customcolourtitle)),
        interval_legend_title = escape_newline(..(input$customcolourtitletext)),
        shape_legend_title = escape_newline(..(input$customshapetitletext)),
        interval_bsv_text = escape_newline(..(input$custombsvtitle)),
        legend_order = ..(input$legendordering),
        combine_area_ref_legend = ..(input$combineareareflegend),
        combine_interval_shape_legend = ..(input$combineintervalshapelegend),
        legend_position = ..(input$legendposition),
        show_ref_area = ..(input$showrefarea),
        ref_area = ..(input$refareain),
        show_ref_value = ..(input$showrefvalue),
        ref_value = ..(ref_value),
        ref_area_col = ..(input$fillrefarea),
        ref_value_col = ..(input$colorrefvalue),
        ref_value_size = ..(input$sizerefvalue),
        ref_value_linetype = ..(input$linetyperefvalue),
        interval_col = paramcols,
        interval_size = ..(input$sizepointrange),
        interval_fatten = ..(input$fattenpointrange),
        interval_linewidth = ..(input$linewidthpointrange),
        interval_shape = paramshapes,
        #interval_arrow = staplearrow,
        bsv_col      = ..(input$colourbsvrange),
        bsv_shape = ..(input$shapebsvrange),
        strip_col = ..(input$stripbackgroundfill),
        paramname_shape = ..(input$shapebyparamname),
        paramname_color = ..(input$colourbyparamname),
        legend_shape_reverse = ..(input$legendshapereverse),
        legend_color_reverse = ..(input$legendcoloreverse),
        facet_switch = ..(input$facetswitch),
        facet_scales = ..(input$facetscales),
        facet_space = ..(input$facetspace),
        facet_labeller = ..(input$facetlabeller),
        label_wrap_width = ..(input$labelwrapwidth),
        facet_labeller_multiline = ..(input$facetwrapmultiline),
        strip_placement = ..(input$stripplacement),
        strip_outline = ..(input$removestrip),
        facet_spacing = ..(input$panelspacing),
        major_x_ticks = ..(major_x_ticks),
        major_x_labels = ..(major_x_labels),
        minor_x_ticks = ..(minor_x_ticks),
        x_range = ..(x_range),
        logxscale = ..(input$logxscale),
        show_yaxis_gridlines = ..(input$showyaxisgridlines),
        show_xaxis_gridlines = ..(input$showxaxisgridlines),
        show_table_facet_strip = ..(input$showtablefacetstrips),
        table_facet_switch = ..(input$tablefacetswitch),
        show_table_yaxis_tick_label = ..(input$showtableyaxisticklabel),
        table_panel_border = ..(input$tablepanelborder),
        reserve_table_xaxis_label_space = ..(input$reservetablexaxislabelspace),
        table_position = ..(input$tableposition),
        plot_table_ratio = ..(input$plottotableratio),
        vertical_dodge_height = ..(input$vdodgeheight),
        legend_space_x_mult = ..(input$legendspacex),
        legend_ncol_interval = ..(input$ncolinterval),
        legend_ncol_shape = ..(input$ncolshape),
        plot_margin = c(..(input$margintop),..(input$marginright),
                        ..(input$marginbottom),..(input$marginleft)),
        table_margin = c(..(input$tabletop),..(input$tableright),
                        ..(input$tablebottom),..(input$tableleft)),
        legend_margin = c(..(input$legendtop),..(input$legendright),
                         ..(input$legendbottom),..(input$legendleft)),
        parse_xlabel = ..(input$parsexaxistitle),
        parse_ylabel = ..(input$parseyaxistitle),
        plot_title = escape_newline(..(input$customplottitle))
      )
      plot
    })
  }, height = function() {
    input$height
  })
  
  observeEvent(input$get_code, {
    if (system.file(package = "shinyAce") == "") {
      stop("Please install.packages('shinyAce') and try again.")
    }
    if (system.file(package = "formatR") == "") {
      stop("Please install.packages('formatR') and try again.")
    }
    
    code <- expandChain(
      "# This code assumes you have a data file named `forest_plot_data.csv`.",
      "# You can download this file using the Download button below.",
      "#",
      "# ------",
      "#",
      quote({
        "# Load required packages"
        library(coveffectsplot)
        library(dplyr)
        library(table1)
        "# Load the data (make sure `forest_plot_data.csv` is in your working directory)"
        df <- read.csv("forest_plot_data.csv", na.strings = c("NA", "."))
        "# Helper functions"
        escape_newline <- function(s) {
          gsub("\\\\n", "\\\n", s)
        }
        translate_shape_string <-  function (shape_string) 
        {
          if (nchar(shape_string[1]) <= 1) {
            return(shape_string)
          }
          pch_table <- c(`square open` = 0, `circle open` = 1, `triangle open` = 2, 
                         plus = 3, cross = 4, `diamond open` = 5, `triangle down open` = 6, 
                         `square cross` = 7, asterisk = 8, `diamond plus` = 9, 
                         `circle plus` = 10, star = 11, `square plus` = 12, `circle cross` = 13, 
                         `square triangle` = 14, `triangle square` = 14, square = 15, 
                         `circle small` = 16, triangle = 17, diamond = 18, circle = 19, 
                         bullet = 20, `circle filled` = 21, `square filled` = 22, 
                         `diamond filled` = 23, `triangle filled` = 24,
                         `triangle down filled` = 25,
                         "none" = NA)
          shape_match <- charmatch(shape_string, names(pch_table))
          unname(pch_table[shape_match])
        }
      }),
      "# Manipulate data and plot",
      output$plot()
    )
    
    code <- formatCode(code)
    code <- formatR::tidy_source(text = code, width.cutoff = 80, indent = 2)
    code <- code$text.tidy
    code <- paste(code, collapse = "\n")
    showModal(modalDialog(
      size = "l", easyClose = TRUE,
      shinyAce::aceEditor(
        "code_editor", value = code, wordWrap = TRUE 
      ),
      footer = tagList(
        actionButton("code_copy", "Copy to Clipboard", icon("copy")),
        downloadButton("code_download_data", "Download data file"),
        modalButton("Dismiss")
      )
    ))
  })
  
  observeEvent(input$code_copy, {
    if (system.file(package = "clipr") == "") {
      stop("Please install.packages('clipr') and try again.")
    }
    clipr::write_clip(input$code_editor, object_type = "character")
  })
  
  output$code_download_data <- downloadHandler(
    filename = "forest_plot_data.csv",
    content = function(file) {
      write.csv(maindata(), file, row.names = FALSE)
    }
  )
}
