#' @importFrom colourpicker colourInput
# ' @importFrom ggplot2 translate_shape_string not yet

# Same as base R `which()` function, but return 0 instead of an empty vector
# if there are no TRUE values in the array
which0 <- function(x) {
  result <- which(x)
  if (length(result) == 0) {
    result <- 0
  }
  result
}

translate_shape_string <- function (shape_string) 
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
                 `diamond filled` = 23, `triangle filled` = 24, `triangle down filled` = 25)
  shape_match <- charmatch(shape_string, names(pch_table))
  unname(pch_table[shape_match])
}

label_wrap <- function(width) {
  force(width)
  function(x) {
    unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
                  paste0, collapse = "\n"))
  }
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Forest plot
#'
#' Produce forest plots to visualize covariate effects
#'
#' @param data Data to use.
#' @param facet_formula Facet formula.
#' @param xlabel X axis title.
#' @param ylabel Y axis title.
#' @param x_facet_text_size Facet text size X.
#' @param y_facet_text_size Facet text size Y.
#' @param x_facet_text_angle Facet text angle X.
#' @param y_facet_text_angle Facet text angle Y.
#' @param x_facet_text_vjust Facet text vertical justification.
#' @param y_facet_text_vjust Facet text vertical justification.
#' @param x_facet_text_hjust Facet text horizontal justification.
#' @param y_facet_text_hjust Facet text horizontal justification.
#' @param x_facet_text_col Facet text color default to black.
#' @param y_facet_text_col Facet text color default to black.
#' @param xy_facet_text_bold Bold Facet text. Logical TRUE FALSE.
#' @param x_label_text_size X axis labels size.
#' @param y_label_text_size Y axis labels size.
#' @param legend_title_size Legend title size if present.
#' @param break_ylabel Split Y axis labels into multiple lines. Logical FALSE TRUE.
#' @param y_label_text_width Number of characters to break Y axis labels.
#' @param table_text_size Table text size.
#' @param table_text_colour_overwrite Logical TRUE FALSE. 
#' @param table_text_colour Table text color to be used and overwrites mapped color
#' @param base_size theme_bw base_size for the plot and table.
#' @param theme_benrich apply Benjamin Rich's theming.
#' @param table_title What text to use for table title (theme_benrich has a default).
#' @param table_title_size table title size.
#' @param ref_legend_text Reference legend text.
#' @param area_legend_text Area legend text.
#' @param interval_legend_text Pointinterval legend text.
#' @param interval_legend_title Pointinterval legend title defaults to empty.
#' @param shape_legend_title Shape legend title defaults to empty.
#' @param legend_order Legend order. A four-element vector with the following
#' items ordered in your desired order: "pointinterval", "ref", "area", "shape".
#' if an item is absent the legend will be omitted.
#' @param combine_area_ref_legend Combine reference and area legends if they
#' share the same text?
#' @param combine_interval_shape_legend Combine interval and shape legends?
#' @param legend_position where to put the legend: "top", "bottom","right","none"
#' @param show_ref_area Show reference window?
#' @param ref_area Reference area. Two-element numeric vector multiplying the ref_value.
#' @param ref_area_col Reference area background color.
#' @param show_ref_value Show reference line?
#' @param ref_value X intercept of reference line.
#' @param ref_value_col Reference line color.
#' @param ref_value_size Reference line size.
#' @param ref_value_linetype Reference line linetype.
#' @param ref_value_by_panel The ref_value vary by panel TRUE or FALSE. 
#' @param ref_value_by_panel_data if ref_value_by_panel is TRUE, data.frame to use for Reference (lines).
#' @param interval_col Point range color. One or Multiple values.
#' @param interval_size Point range size. Default to 1
#' @param interval_fatten Point range fatten. Default to 4
#' @param interval_linewidth Point range line width. Default to 1
#' @param interval_shape Shape used for the Point Range. Default to "circle small".
#' @param bsv_col  BSV pointinterval color. One value.
#' @param bsv_shape Shape used for the BSV Point Range. Default to "circle small".
#' @param bsv_text_id Text string(s) to identify BSV. Default to c("BSV","bsv","IIV","Bsv")
#' @param interval_bsv_text BSV legend text.
#' @param strip_col Strip background color.
#' @param paramname_shape Map symbol to parameter(s) name? TRUE or FALSE.
#' @param paramname_color Map color to parameter(s) name? TRUE or FALSE.
#' @param legend_shape_reverse TRUE or FALSE. 
#' @param legend_color_reverse TRUE or FALSE.  
#' @param facet_switch Facet switch to near axis. Possible values: "both", "y",
#' "x", "none".
#' @param facet_scales Facet scales. Possible values: "free_y", "fixed",
#' "free_x", "free".
#' @param facet_space Facet spaces. Possible values: "fixed", "free_x",
#' "free_y", "free".
#' @param facet_labeller Facet Labeller. Default "label_value"
#'  any other valid `facet_grid` labeller can be specified.
#' @param label_wrap_width How many characters before breaking the line. Numeric value.
#'  any other valid `facet_grid` labeller can be specified.
#' @param facet_labeller_multiline break facet strips into multiple lines. Logical TRUE FALSE.
#' @param strip_placement Strip placement. Possible values: "inside", "outside".
#' @param strip_outline Draw rectangle around the Strip. Logical TRUE FALSE.
#' @param facet_spacing Control the space between facets in points.
#' @param major_x_ticks X axis major ticks. Numeric vector.
#' @param major_x_labels X axis labels. Character vector should be same length as major_x_ticks.
#' @param minor_x_ticks X axis minor ticks. Numeric vector.
#' @param x_range Range of X values. Two-element numeric vector.
#' @param logxscale  X axis log scale. Logical TRUE FALSE.
#' @param show_yaxis_gridlines Draw the y axis gridlines. Logical TRUE FALSE.
#' @param show_xaxis_gridlines Draw the x axis gridlines. Logical TRUE FALSE.
#' @param show_table_facet_strip Possible values: "none", "both", "y", "x".
#' @param table_facet_switch Table facet switch to near axis. Possible values: "both", "y",
#' "x", "none". 
#' @param show_table_yaxis_tick_label Show table y axis ticks and labels?
#' @param reserve_table_xaxis_label_space keep space for the x axis label to keep alignment.
#' @param table_panel_border Draw the panel border for the table. Logical TRUE FALSE.
#' @param table_position Table position. Possible values: "right", "below", "none".
#' @param plot_table_ratio Plot-to-table ratio. Suggested value between 1-5.
#' @param vertical_dodge_height Amount of vertical dodging to apply on segments and table text.
#' @param legend_space_x_mult Multiplier to adjust the spacing between legend items.
#' @param legend_ncol_interval Control the number of columns for the pointinterval legend. 
#' @param legend_ncol_shape Control the number of columns for the shape legend.
#' @param plot_margin Control the white space around the main plot. Vector of four numeric values
#' for the top, right, bottom and left sides.
#' @param table_margin Control the white space around the table. Vector of four numeric values
#' for the top, right, bottom and left sides.
#' @param legend_margin Control the white space around the plot legend. Vector of four numeric values
#' for the top, right, bottom and left sides.
#' @param parse_xlabel treat xlabel as an expression. Logical FALSE TRUE.
#' @param parse_ylabel treat ylabel as an expression. Logical FALSE TRUE.
#' @param plot_title main plot title default to a line break.
#' @param return_list What to return if True a list of the main and table plots is returned
#' instead of the gtable/plot.
#' @rawNamespace import(data.table, except = c(last,between,first))
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' 
#' # Example 1
#'
#' plotdata <- get_sample_data("forest-plot-table.csv")
#' plotdata <- plotdata %>%
#'   mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' param <- "BZD AUC"
#' plotdata <-  filter(plotdata,paramname==param)
#' plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
#' plotdata$label <- reorder(plotdata$label,plotdata$scen)
#' covs <- c("WEIGHT","AGE")
#' plotdata <-  filter(plotdata,covname%in%covs)

#' forest_plot(plotdata,
#'             ref_legend_text = "Reference (vertical line)",
#'             area_legend_text = "Reference (vertical line)",
#'             xlabel = paste("Fold Change in", param, "Relative to Reference"),
#'             logxscale = TRUE, major_x_ticks =c(0.1,1,1.5),
#'             show_ref_area = FALSE,
#'             paramname_color =TRUE,
#'             interval_col =c("steelblue","red","steelblue","red"),
#'             facet_formula = "covname~.",
#'             facet_scales = "free_y",
#'             facet_space = "free_y",
#'             show_table_facet_strip = "none",
#'             table_position = "right",
#'             plot_title = "",
#'             plot_table_ratio = 4)
#'
#' # Example 2
#'
#' plotdata <- get_sample_data("forest-plot-table.csv")
#' plotdata <- plotdata %>%
#'   mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' param <- c("BZD AUC","BZD Cmax")
#' plotdata <-  filter(plotdata,paramname%in%param)
#' plotdata <-  filter(plotdata,covname%in%"WEIGHT")
#' plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
#' plotdata$label <- reorder(plotdata$label,plotdata$scen)
#' forest_plot(plotdata,
#'             ref_legend_text = "Reference (vertical line)",
#'             area_legend_text = "Reference (vertical line)",
#'             xlabel = paste("Fold Change of Parameter", "Relative to Reference"),
#'             show_ref_area = FALSE,
#'             facet_formula = "covname~paramname",
#'             facet_scales = "free_y",
#'             facet_space = "free_y",
#'             x_facet_text_size = 10,
#'             y_facet_text_size = 10,
#'             y_label_text_size = 10,
#'             y_label_text_width = 15,
#'             x_label_text_size = 10,
#'             facet_switch = "both",
#'             show_table_facet_strip = "both",
#'             show_table_yaxis_tick_label = TRUE,
#'             table_position = "below",
#'             plot_title = "",
#'             plot_table_ratio = 1)
#'\dontrun{
#'
#' # Example 3a
#' 
#' plotdata <- get_sample_data("forest-plot-table.csv")
#' plotdata <- plotdata %>%
#'   mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
#' plotdata$label <- reorder(plotdata$label,plotdata$scen)
#' 
#' plotdata$compound <- c(rep("1-OH",30),rep("BZD",30))
#' plotdata$paramname <- c(rep("AUC",15),rep("CMAX",15),rep("AUC",15),rep("CMAX",15))
#' 
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'            x_facet_text_size = 13,
#'            y_facet_text_size = 13,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to Parameter",
#'             facet_formula = covname~compound,
#'             facet_switch = "both",
#'             facet_scales = "free",
#'             facet_space = "fixed",
#'             paramname_shape = TRUE,
#'             legend_shape_reverse = TRUE,
#'             interval_shape = c("square","triangle"),
#'             paramname_color =  FALSE,
#'             combine_interval_shape_legend = FALSE,
#'             table_position = "right", plot_title = "",
#'             ref_area_col = rgb( col2rgb("gray50")[1], col2rgb("gray50")[2],col2rgb("gray50")[3],
#'                                 max = 255, alpha = 0.1*255 ) ,
#'             interval_col = c("steelblue"),
#'             strip_col = "lightblue",
#'             plot_table_ratio = 1.5)
#'             
#' # Example 3b
#'         
#' plotdata$paramname <- c(rep("1-OH",30),rep("BZD",30))
#' plotdata$paramname2 <- c(rep("AUC",15),rep("CMAX",15),rep("AUC",15),rep("CMAX",15))
#'forest_plot(plotdata,
#'            show_ref_area = TRUE,
#'            x_facet_text_size = 13,
#'            y_facet_text_size = 13,
#'            ref_legend_text = "Reference (vertical line)",
#'            area_legend_text = "Reference (vertical line)",
#'            xlabel = "Fold Change Relative to Parameter",
#'            facet_formula = covname~paramname2,
#'            facet_switch = "both",
#'            facet_scales = "free",
#'            facet_space = "free",
#'            legend_order = c("shape","pointinterval","ref"),
#'            paramname_shape = TRUE,
#'            interval_shape = c("diamond","diamond filled",
#'                               "diamond","diamond filled"),
#'            paramname_color =  TRUE,
#'            combine_interval_shape_legend = TRUE,
#'            legend_shape_reverse = TRUE,
#'            legend_color_reverse = TRUE,
#'            interval_legend_title="Median (points)\n95% CI (horizontal lines)",
#'            table_position = "right", plot_title = "",
#'            ref_area_col = "gray85" ,
#'            interval_col = c("#ee3124", "#fdbb2f"),
#'            strip_col = "#475c6b",
#'            y_facet_text_col = "white",x_facet_text_col = "white",
#'            major_x_labels         = c("1/2", "0.8","1", "1.25", "2"),
#'            logxscale = TRUE, major_x_ticks =c(0.5,0.8,1,1.25,2),
#'            table_text_size = 5,
#'            plot_table_ratio = 1.5,
#'            ref_value_by_panel = TRUE,
#'            ref_value_by_panel_data = as.data.frame(
#'            plotdata %>% 
#'            distinct(paramname2,covname) %>% 
#'            dplyr::mutate(xintercept=ifelse(paramname2=="CMAX",1,1.2))))
#'
#' # Example 3
#' 
#' plotdata <- get_sample_data("forestplotdatacpidata.csv")
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'             x_facet_text_size = 12,
#'             y_facet_text_size = 12,
#'             y_label_text_size = 10,
#'             x_label_text_size = 10,
#'             table_text_size = 6,
#'             plot_table_ratio = 1.5,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to RHZE",
#'             facet_formula = "covname~paramname",
#'             table_position = "below",
#'             show_table_facet_strip = "both",
#'             show_table_yaxis_tick_label = TRUE)
#'
#' # Example 4
#' plotdata <- get_sample_data("dataforest.csv")
#' plotdata <- plotdata %>%
#'   dplyr::mutate(midlabel = format(round(mid,2), nsmall = 2),
#'          lowerlabel = format(round(lower,2), nsmall = 2),
#'          upperlabel = format(round(upper,2), nsmall = 2),
#'          LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
#' plotdata <- plotdata %>%
#'   filter(covname%in%c("Weight"))
#' plotdata$label <- as.factor(as.character(plotdata$label))
#' plotdata$label <- factor(plotdata$label, c("36.2 kg","66 kg","110 kg"))
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'             x_facet_text_size = 13,
#'             y_facet_text_size = 13,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to Parameter",
#'             facet_formula = "covname~paramname",
#'             facet_switch = "both",
#'             facet_scales = "free",
#'             facet_space = "fixed",
#'             table_position = "below",
#'             plot_table_ratio = 1,
#'             show_table_facet_strip = "both",
#'             show_table_yaxis_tick_label = TRUE)
#'
#' # Example 5
#'
#' forest_plot(plotdata,
#'             ref_area = c(0.8, 1.2),
#'             x_facet_text_size = 13,
#'             y_facet_text_size = 13,
#'             ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
#'             xlabel = "Fold Change Relative to Parameter",
#'             facet_formula = "covname~.",
#'             facet_switch = "both",
#'             facet_scales = "free",
#'             facet_space = "fixed",
#'             paramname_shape = TRUE,
#'             table_position = "none",
#'             ref_area_col = rgb( col2rgb("gray50")[1], col2rgb("gray50")[2],col2rgb("gray50")[3],
#'             max = 255, alpha = 0.1*255 ) ,
#'             interval_col = "steelblue",
#'             strip_col = "lightblue",
#'             plot_table_ratio = 1)
#'}
#' @export
forest_plot <- function(
  data,
  facet_formula = "covname~paramname",
  xlabel = "",
  ylabel = "",
  x_facet_text_size = 13,
  y_facet_text_size = 13,
  x_facet_text_angle = 0,
  y_facet_text_angle = 0,
  x_facet_text_vjust = 0.5,
  y_facet_text_vjust = 0.5,
  x_facet_text_hjust = 0.5,
  y_facet_text_hjust = 0.5,
  x_facet_text_col = "black",
  y_facet_text_col = "black",
  xy_facet_text_bold = TRUE,
  x_label_text_size  = 16,
  y_label_text_size  = 16,
  legend_title_size = 12,
  break_ylabel = FALSE,
  y_label_text_width = 25,
  table_text_size = 7,
  table_text_colour_overwrite = FALSE,
  table_text_colour = "none",
  base_size = 22,
  theme_benrich = FALSE,
  table_title = "",
  table_title_size = 15,
  ref_legend_text = "",
  area_legend_text = "",
  interval_legend_text = "",
  interval_legend_title = "",
  shape_legend_title = "",
  legend_order = c("pointinterval", "ref", "area", "shape"),
  combine_area_ref_legend = TRUE,
  combine_interval_shape_legend = FALSE,
  legend_position = "top",
  show_ref_area = TRUE,
  ref_area = c(0.8, 1.25),
  ref_area_col = "#BEBEBE50",
  show_ref_value = TRUE,
  ref_value = 1,
  ref_value_col = "black",
  ref_value_size = 1,
  ref_value_linetype = "dashed",
  ref_value_by_panel = FALSE,
  ref_value_by_panel_data = NULL,
  interval_col = "blue",
  interval_size = 1,
  interval_fatten = 4,
  interval_linewidth = 1,
  interval_shape =  "circle small",
  bsv_col = "red",
  bsv_shape = "circle small",
  bsv_text_id = c("BSV","bsv","IIV","Bsv"),
  interval_bsv_text = "",
  strip_col = "#E5E5E5",
  paramname_shape = FALSE,
  paramname_color = FALSE,
  legend_shape_reverse = FALSE,
  legend_color_reverse = FALSE,
  facet_switch = c("both", "y", "x", "none"),
  facet_scales = c("fixed", "free_y", "free_x", "free"),
  facet_space = c("fixed", "free_x", "free_y", "free"),
  facet_labeller = "label_value",
  label_wrap_width = 55,
  facet_labeller_multiline = FALSE,
  strip_placement = c("inside", "outside"),
  strip_outline = TRUE,
  facet_spacing = 5.5,
  major_x_ticks = NULL,
  major_x_labels = NULL,
  minor_x_ticks = NULL,
  x_range = NULL,
  logxscale = FALSE,
  show_yaxis_gridlines = TRUE,
  show_xaxis_gridlines = TRUE,
  show_table_facet_strip = "none",
  table_facet_switch = c("both", "y", "x", "none"),
  show_table_yaxis_tick_label = FALSE,
  reserve_table_xaxis_label_space = TRUE,
  table_panel_border = TRUE,
  table_position = c("right", "below", "none"),
  plot_table_ratio = 4,
  vertical_dodge_height = 0.8,
  legend_space_x_mult = 1,
  legend_ncol_interval = 1,
  legend_ncol_shape = 1,
  plot_margin = c(5.5, 5.5, 5.5, 5.5),
  table_margin = c(5.5, 5.5, 5.5, 5.5),
  legend_margin = c(0, 0.1, -0.1, 0),
  parse_xlabel = FALSE,
  parse_ylabel = FALSE,
  plot_title = "\n",
  return_list = FALSE)
{
  ymax = ymin = x = fill = label_wrap_gen = xintercept = NULL
  
  if (!(missing(major_x_labels) || is.null(major_x_labels) ) &&
      !(missing(major_x_ticks)  || is.null(major_x_ticks) )
      ) {
    major_x_labels_vec <- rep_len(major_x_labels , length(major_x_ticks))
  }
  if (missing(major_x_labels) || is.null(major_x_labels)) {
    major_x_labels_vec <- ggplot2::waiver()
  }
  
  plot_margin[ which(is.na(plot_margin) ) ] <- 0
  table_margin[ which(is.na(table_margin) ) ] <- 0
  legend_margin[ which(is.na(legend_margin) ) ] <- 0
  facet_spacing[ which(is.na(facet_spacing) ) ] <- 0
  
  table_position <- match.arg(table_position)
  legend_order <- match.arg(legend_order, several.ok = TRUE)
  facet_switch <- match.arg(facet_switch)
  table_facet_switch <- match.arg(table_facet_switch)
  
  facet_scales <- match.arg(facet_scales)
  facet_space <- match.arg(facet_space)
  strip_placement <- match.arg(strip_placement)
  facet_formula <- stats::as.formula(facet_formula)
  
  y_facet_text_angle<- ifelse(facet_switch %in% c("x","none"),
         y_facet_text_angle-0,
         y_facet_text_angle)

  if (x_facet_text_size <= 0) {
    x.strip.text <- ggplot2::element_blank()
    table.x.strip.text <- x.strip.text
  } else {
    x.strip.text <- ggplot2::element_text(size = x_facet_text_size,
                                          angle= x_facet_text_angle,
                                          face = ifelse(xy_facet_text_bold,"bold","plain"),
                                          hjust = x_facet_text_hjust,
                                          vjust = x_facet_text_vjust,
                                          color =  x_facet_text_col
                                          )
    table.x.strip.text <- x.strip.text

  }
  if (y_facet_text_size <= 0) {
    y.strip.text <- ggplot2::element_blank()
    table.y.strip.text <- y.strip.text
  } else {
    y.strip.text <- ggplot2::element_text(size = y_facet_text_size,
                                          angle= y_facet_text_angle,
                                          face = ifelse(xy_facet_text_bold,"bold","plain"),
                                          hjust = y_facet_text_hjust,
                                          vjust = y_facet_text_vjust,
                                          color =  y_facet_text_col
                                          )
   table.y.strip.text <- y.strip.text
  }
  
  if (theme_benrich && y_facet_text_size >0){
    y.strip.text <- ggplot2::element_text(
      hjust=1,
      vjust=1,
      face="bold",
      size=y_facet_text_size,
      angle= y_facet_text_angle
    )
    
    table.y.strip.text <- ggplot2::element_text(
      hjust=1,
      vjust=1,
      face="bold",
      size=y_facet_text_size,
      angle= y_facet_text_angle
    )
  }

  if ( !is.expression(xlabel) && xlabel == "" ) {
    xlabel <- paste("Changes of Parameter Relative to Reference")
  }
  if ( !is.expression(xlabel) && parse_xlabel) { 
    xlabel <-   parse(text=xlabel)
    }
  if ( !is.expression(ylabel) && parse_ylabel) { 
    ylabel <-   parse(text=ylabel)
  }
  
  if (table_title == "" && theme_benrich) {
    table_title <- "Median [95% CI]"
  }
  if (ref_legend_text == "") {
    ref_legend_text <- "Reference (vertical line)\nClinically relevant limits (colored area)"
  }
  if (area_legend_text == "") {
    area_legend_text <- "Reference (vertical line)\nClinically relevant limits (colored area)"
  }
  if (interval_legend_text == "") {
    interval_legend_text <- "Median (points)\n95% CI (horizontal lines)"
  }
  if (interval_bsv_text == "") {
    interval_bsv_text <- "BSV (points)\nPrediction Intervals (horizontal lines)"
  }
  
  interval_pos <-  which0(legend_order == "pointinterval")[1]
  fill_pos <- which0(legend_order == "area")[1]
  linetype_pos <- which0(legend_order == "ref")[1]
  shape_pos <- which0(legend_order == "shape")[1]
  if (combine_area_ref_legend) {
    fill_pos <- linetype_pos
  }
  guide_fill <- ggplot2::guide_legend("", order = fill_pos)
  guide_linetype <- ggplot2::guide_legend("", order = linetype_pos)
  
  guide_interval <- ggplot2::guide_legend(interval_legend_title, order = interval_pos,
                                          reverse = legend_color_reverse,
                                          ncol = legend_ncol_interval)
  guide_shape    <- ggplot2::guide_legend(shape_legend_title, order = shape_pos,
                                      override.aes = list(linetype = 0,
                                      colour = "gray"),
                                      reverse = legend_shape_reverse,
                                      ncol = legend_ncol_shape)
  
  if (paramname_shape && paramname_color && combine_interval_shape_legend) {
    guide_interval <- ggplot2::guide_legend(interval_legend_title, order = interval_pos,
                                            reverse = legend_shape_reverse,
                                            ncol = legend_ncol_shape)
    guide_shape    <- ggplot2::guide_legend(interval_legend_title, order = interval_pos,
                                            reverse = legend_shape_reverse,
                                            ncol = legend_ncol_shape)
  }
  
  if (!paramname_shape && !paramname_color && combine_interval_shape_legend) {
    guide_interval <- ggplot2::guide_legend(interval_legend_title, order = interval_pos,
                                            reverse = legend_shape_reverse,
                                            ncol = legend_ncol_shape)
    guide_shape    <- ggplot2::guide_legend(interval_legend_title, order = interval_pos,
                                            reverse = legend_shape_reverse,
                                            ncol = legend_ncol_shape)
  }

  if( interval_pos==0) guide_interval = FALSE
  if( fill_pos==0) guide_fill = FALSE
  if( linetype_pos==0) guide_linetype = FALSE
  if( shape_pos==0) guide_shape = FALSE
  
  data$label <- factor(data$label)
  data$pointintervalcolor <-  ifelse( !data$covname%in% bsv_text_id,
                                      interval_legend_text,
                                      interval_bsv_text)
  data$pointintervalcolor <- factor(data$pointintervalcolor,
                                    levels =c(interval_legend_text,
                                              interval_bsv_text)[!duplicated(c(interval_legend_text,
                                                                               interval_bsv_text))]
                                    )
  
  data$pointintervalshape <-  data$pointintervalcolor
  
 colbreakvalues<- c(interval_legend_text, interval_bsv_text)
 if(paramname_color) {
   data$pointintervalcolor <- data$paramname
   if( is.factor( data$pointintervalcolor)) colbreakvalues<- levels(data$pointintervalcolor)
   if(!is.factor( data$pointintervalcolor)) colbreakvalues<- unique(data$pointintervalcolor)
 }
 
 shapebreakvalues<- c(interval_legend_text, interval_bsv_text)
 if(paramname_shape) {
   data$pointintervalshape <- data$paramname
   if( is.factor( data$pointintervalshape)) shapebreakvalues<- levels(data$pointintervalshape)
   if(!is.factor( data$pointintervalshape)) shapebreakvalues<- unique(data$pointintervalshape)
 }

  main_plot <-
    ggplot2::ggplot(data = data, ggplot2::aes_string(
      y = "label",
      x = "mid",
      xmin = "lower",
      xmax = "upper"
    )) +
    ggplot2::geom_blank(
      position = ggplot2::position_dodge(width = vertical_dodge_height),
      ggplot2::aes_string(color = "pointintervalcolor"),
    )# dummy to prevent a scales bug that I reported to ggplot2 maintainers

  if (show_ref_area && !ref_value_by_panel) {
    main_plot <- main_plot +
      ggplot2::annotate(
        "rect",
        xmin = ref_value*min(ref_area),
        xmax = ref_value*max(ref_area),
        ymin = -Inf,
        ymax = Inf,
        fill = ref_area_col
      ) +
    ggplot2::geom_ribbon(
      data = data.frame(x = ref_value,
                        ymax = Inf,
                        ymin = -Inf,
                        fill = area_legend_text),
      ggplot2::aes(
        x = x,
        ymax = ymax,
        ymin = ymin,
        fill = fill
      ),
      size = 1,
      inherit.aes = FALSE
    ) 
}
# fake ribbon for fill legend
  
  if (show_ref_value && !ref_value_by_panel) {
    main_plot <- main_plot +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = ref_value,
                     linetype = ref_legend_text),
        size = ref_value_size,
        color = ref_value_col 
      )
  }
  if (ref_value_by_panel &&  !is.null(ref_value_by_panel_data) ) {
    if(show_ref_area){
    main_plot <- main_plot +
      ggplot2::geom_rect(data=ref_value_by_panel_data,
                          ggplot2::aes(xmin = xintercept*min(ref_area),
                                       xmax = xintercept*max(ref_area),
                                       ymin = -Inf,
                                       ymax = Inf),
                         fill = ref_area_col,
                         inherit.aes = FALSE,
      )+
      ggplot2::geom_ribbon(
        data =  cbind(as.data.frame(ref_value_by_panel_data),
                          x = ref_value_by_panel_data$xintercept,
                          ymax = Inf,
                          ymin = -Inf,
                          fill = area_legend_text),
        ggplot2::aes(
          x = x,
          ymax = ymax,
          ymin = ymin,
          fill = fill
        ),
        size = 1,
        inherit.aes = FALSE
      )      
    }
    if(show_ref_value){
      main_plot <- main_plot +
        ggplot2::geom_vline(data=ref_value_by_panel_data,
                            ggplot2::aes(xintercept = xintercept,
                                         linetype = ref_legend_text),
                            size = ref_value_size,
                            color = ref_value_col 
        )
    }
  }
  main_plot <- main_plot+
    ggplot2::geom_pointrange(
      position = ggplot2::position_dodge(width = vertical_dodge_height),
      ggplot2::aes_string(color = "pointintervalcolor",
                          shape = "pointintervalshape"),
      size = interval_size, fatten = interval_fatten,
      linewidth  = interval_linewidth,
      key_glyph = "pointrange"
    )

  l_u <- length(unique(as.character(data$paramname)))
  interval_col_values   <- c(interval_col,bsv_col)
  interval_shape_values <- c(translate_shape_string(interval_shape),
                             translate_shape_string(bsv_shape))
  if(paramname_color && (length(interval_col) < l_u))  interval_col_values <- rep(interval_col_values, l_u)
  if(paramname_shape && (length(interval_shape) < l_u))  interval_shape_values <- rep(interval_shape_values, l_u)

      main_plot <- main_plot +
      ggplot2::scale_colour_manual("", breaks = colbreakvalues,
                                   values = c(interval_col_values))
      main_plot <- main_plot +
      ggplot2::scale_shape_manual("", breaks = shapebreakvalues,
                                    values= interval_shape_values) 


    main_plot <- main_plot +
    ggplot2::scale_linetype_manual("", breaks = ref_legend_text, values = ref_value_linetype) +
    ggplot2::scale_fill_manual("", breaks = area_legend_text, values = ref_area_col)+
    ggplot2::guides(colour = guide_interval,
                    linetype = guide_linetype,
                    fill = guide_fill,
                    shape = guide_shape )

  if (!show_ref_area) {
    main_plot <- main_plot +
      ggplot2::guides(colour = guide_interval,
                      linetype = guide_linetype,
                      shape = guide_shape,
                      fill = NULL)

  }

  main_plot <- main_plot +
    ggplot2::aes_string(group = "paramname")
  if (paramname_shape) {
    main_plot <- main_plot +
      ggplot2::aes_string(shape = "paramname")
  }
  if (!is.function(facet_labeller)) {
  if (facet_labeller != "label_wrap_gen") {
  if (facet_switch != "none") {
    main_plot <- main_plot +
      ggplot2::facet_grid(facet_formula,
                          scales = facet_scales,
                          space = facet_space,
                          switch = facet_switch,
                          labeller = eval(parse(
                            text=paste0("function(labs){",facet_labeller,
                                        "(labs, multi_line = ",facet_labeller_multiline,")}")
                          ,keep.source = FALSE))
                          )
  } else {
    main_plot <- main_plot +
      ggplot2::facet_grid(facet_formula,
                          scales = facet_scales,
                          space = facet_space,
                          switch = NULL,
                          labeller = eval(parse(
                            text=paste0("function(labs){",facet_labeller,
                                        "(labs, multi_line = ",facet_labeller_multiline,")}")
                            ,keep.source = FALSE))
                          )
  }
  }
  if (facet_labeller == "label_wrap_gen") {
    if (facet_switch != "none") {
      main_plot <- main_plot +
        ggplot2::facet_grid(facet_formula,
                            scales = facet_scales,
                            space = facet_space,
                            switch = facet_switch,
                            labeller = ggplot2::label_wrap_gen(width =
                                                        label_wrap_width,
                                                      multi_line =
                                                        facet_labeller_multiline)
        )
    } else {
      main_plot <- main_plot +
        ggplot2::facet_grid(facet_formula,
                            scales = facet_scales,
                            space = facet_space,
                            switch = NULL,
                            labeller = ggplot2::label_wrap_gen(width =
                                                       label_wrap_width,
                                                      multi_line =
                                                        facet_labeller_multiline)
        )
    }
  }
  }
  if (is.function(facet_labeller)) {
      if (facet_switch != "none") {
        main_plot <- main_plot +
          ggplot2::facet_grid(facet_formula,
                              scales = facet_scales,
                              space = facet_space,
                              switch = facet_switch,
                              labeller = facet_labeller
          )
      } else {
        main_plot <- main_plot +
          ggplot2::facet_grid(facet_formula,
                              scales = facet_scales,
                              space = facet_space,
                              switch = NULL,
                              labeller = facet_labeller
                              )
      }
    }
  main_plot <- main_plot +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        angle = 0,
        size = y_label_text_size
      ),
      axis.text.y.left = ggplot2::element_text(
        angle = 0,
        size = y_label_text_size
      ),
      axis.text.x = ggplot2::element_text(size = x_label_text_size),
      legend.position = legend_position,
      legend.justification = c(0.5, 0.5),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(size = legend_title_size),
      legend.key.width = ggplot2::unit(3, "line"),
      strip.text.x = x.strip.text,
      strip.text.y = y.strip.text,
      strip.text.y.left = y.strip.text,
      panel.grid.minor = ggplot2::element_line(colour = "gray", linetype = "dotted"),
      panel.grid.major = ggplot2::element_line(colour = "gray", linetype = "solid"),
      strip.background = ggplot2::element_rect(fill = strip_col),
      panel.spacing = ggplot2::unit(facet_spacing, "pt"),
      strip.placement  = strip_placement,
      legend.spacing.x = ggplot2::unit(legend_space_x_mult*11, "pt"),
      legend.margin = ggplot2::margin(t = legend_margin[1],
                                      r = legend_margin[2],
                                      b = legend_margin[3],
                                      l = legend_margin[4],
                                      unit='pt'),
      plot.margin =  ggplot2::margin(t = plot_margin[1],
                                     r = plot_margin[2],
                                     b = plot_margin[3],
                                     l = plot_margin[4],
                                     unit='pt')
    )  

  if (!is.null(plot_title) & plot_title!="") {
    main_plot <- main_plot +
      ggplot2::ggtitle(plot_title)
  }
  
  if (!strip_outline) {
    main_plot <- main_plot +
      ggplot2::theme(strip.background=ggplot2::element_blank())
  }
  
  if (!show_yaxis_gridlines) {
    main_plot <- main_plot +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor.y = ggplot2::element_blank())
  }
  if (!show_xaxis_gridlines) {
    main_plot <- main_plot +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     panel.grid.minor.x = ggplot2::element_blank())
  }
  
  main_plot <- main_plot +
    ggplot2::xlab(xlabel)
  
  main_plot <- main_plot +
    ggplot2::ylab(ylabel)
  
  if (grepl("^\\s+$", ylabel) ){
    main_plot <- main_plot +
      ggplot2::theme(axis.title.y=ggplot2::element_blank())
    }
  if (grepl("^\\s+$", xlabel) ){
    main_plot <- main_plot +
      ggplot2::theme(axis.title.x=ggplot2::element_blank())
  }
  
  main_plot <- main_plot +
    ggplot2::scale_x_continuous(trans = ifelse(logxscale,"log","identity"))
  if (break_ylabel) {
    main_plot <- main_plot +
      ggplot2::scale_y_discrete(labels = label_wrap(y_label_text_width))
  }
  if (length(major_x_ticks) || length(minor_x_ticks)) {

    main_plot <- main_plot +
      ggplot2::scale_x_continuous(trans = ifelse(logxscale,"log","identity"),
        breaks = major_x_ticks,
        labels = major_x_labels_vec,
        minor_breaks = minor_x_ticks
      )
  }

  if (!is.null(x_range)) {
    main_plot <- main_plot +
      ggplot2::coord_cartesian(xlim = x_range)
  }
  
  if (theme_benrich){
    main_plot <- main_plot +
      ggplot2::theme(
       panel.spacing=ggplot2::unit(0, "pt"),
       panel.grid = ggplot2::element_blank(),
       panel.grid.minor = ggplot2::element_blank(),
       strip.background = ggplot2::element_blank(),
       strip.text.y = y.strip.text,
       strip.text.y.left = y.strip.text,
       strip.text.x= x.strip.text,
       plot.margin = ggplot2::margin(t=3,r=3,b=3,l=3,unit="pt")
       )
  }

  if (table_position != "none") {
    table_plot <- ggplot2::ggplot(data = data,
                                  ggplot2::aes_string(y = "label"))
    if ( !paramname_color && !table_text_colour_overwrite) {
      table_plot <- table_plot +
      ggplot2::aes_string(group = "paramname") +
      ggplot2::geom_text(
        ggplot2::aes_string(
          x = 1,
          label = "LABEL",
          hjust = 0.5
        ),
        size = table_text_size,
        position = ggplot2::position_dodge(width = vertical_dodge_height)
      )
    }
    
    if ( paramname_color && !table_text_colour_overwrite) {
      table_plot <- table_plot +
        ggplot2::aes_string(group = "paramname", colour = "paramname") +
        ggplot2::geom_text(
          ggplot2::aes_string(
            x = 1,
            label = "LABEL",
            hjust = 0.5
          ),
          size = table_text_size,
          position = ggplot2::position_dodge(width = vertical_dodge_height)
        ) +
          ggplot2::scale_colour_manual("", breaks = colbreakvalues,
                                       values = interval_col_values)
      
    }
    if ( table_text_colour_overwrite) {
      table_plot <- table_plot +
        ggplot2::aes_string(group = "paramname") +
        ggplot2::geom_text(
          ggplot2::aes_string(
            x = 1,
            label = "LABEL",
            hjust = 0.5
          ),
          size = table_text_size, colour = table_text_colour,
          position = ggplot2::position_dodge(width = vertical_dodge_height)
        ) 
    }
    
    if ( !is.function(facet_labeller))  {
    if (facet_labeller != "label_wrap_gen") {
    if (table_facet_switch != "none") {
      table_plot <- table_plot +
        ggplot2::facet_grid(facet_formula,
                            scales = facet_scales,
                            space = facet_space,
                            switch = table_facet_switch,
                            labeller = eval(parse(
                              text=paste0("function(labs){",facet_labeller,
                                          "(labs, multi_line = ",facet_labeller_multiline,")}")
                              ,keep.source = FALSE))
                            )
    } else {
      table_plot <- table_plot +
        ggplot2::facet_grid(facet_formula,
                            scales = facet_scales,
                            space = facet_space,
                            switch = NULL,
                            labeller =  eval(parse(
                              text=paste0("function(labs){",facet_labeller,
                                          "(labs, multi_line = ",facet_labeller_multiline,")}")
                              ,keep.source = FALSE))
                            )
    }
    }
    if (facet_labeller == "label_wrap_gen") {
      if (table_facet_switch != "none") {
        table_plot <- table_plot +
          ggplot2::facet_grid(facet_formula,
                              scales = facet_scales,
                              space = facet_space,
                              switch = table_facet_switch,
                              labeller = ggplot2::label_wrap_gen(width =
                                                          label_wrap_width,
                                                        multi_line =
                                                          facet_labeller_multiline)
                              )
      } else {
        table_plot <- table_plot +
          ggplot2::facet_grid(facet_formula,
                              scales = facet_scales,
                              space = facet_space,
                              switch = NULL,
                              labeller = ggplot2::label_wrap_gen(width =
                                                          label_wrap_width,
                                                        multi_line =
                                                          facet_labeller_multiline)
                              )
      }
    }
    }
    if (is.function(facet_labeller)) {
      if (facet_switch != "none") {
        table_plot <- table_plot +
          ggplot2::facet_grid(facet_formula,
                              scales = facet_scales,
                              space = facet_space,
                              switch = facet_switch,
                              labeller = facet_labeller
          )
      } else {
        table_plot <- table_plot +
          ggplot2::facet_grid(facet_formula,
                              scales = facet_scales,
                              space = facet_space,
                              switch = NULL,
                              labeller = facet_labeller
          )
      }
    }
    table_plot <- table_plot +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(
          angle = 0,
          size = y_label_text_size
        ),
        axis.text.y.left = ggplot2::element_text(
          angle = 0,
          size = y_label_text_size
        ),
        strip.text.x = table.x.strip.text,
        axis.text.x = ggplot2::element_text(size = x_label_text_size),
        axis.ticks.x= ggplot2::element_blank(),
        strip.text.y = table.y.strip.text,
        strip.text.y.left = table.y.strip.text,
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.spacing = ggplot2::unit(facet_spacing, "pt"),
        strip.background = ggplot2::element_rect(fill = strip_col),
        strip.placement  = strip_placement,
        plot.margin =  ggplot2::margin(t = table_margin[1],
                                       r = table_margin[2],
                                       b = table_margin[3],
                                       l = table_margin[4],
                                       unit='pt')
      ) +
      ggplot2::theme(legend.position = "none")+
      ggplot2::scale_x_continuous(breaks=c(1),label="",limits =c(0.99, 1.01) )

    if (!strip_outline) {
      table_plot <- table_plot +
        ggplot2::theme(strip.background=ggplot2::element_blank())
    }

    if (show_table_facet_strip=="none") {
      table_plot <- table_plot +
        ggplot2::theme(
          strip.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank(),
          strip.text.y.left = ggplot2::element_blank(),
          strip.background.x = ggplot2::element_blank(),
          strip.background.y = ggplot2::element_blank()
        )
    }
    
    if (show_table_facet_strip=="y") {
      table_plot <- table_plot +
        ggplot2::theme(
          strip.text.x = ggplot2::element_blank(),
          strip.background.x = ggplot2::element_blank()
        )
    }
    
    if (show_table_facet_strip=="x") {
      table_plot <- table_plot +
        ggplot2::theme(
          strip.text.y = ggplot2::element_blank(),
          strip.text.y.left = ggplot2::element_blank(),
          strip.background.y = ggplot2::element_blank()
        )
    }
    
    
    if (!show_table_yaxis_tick_label) {
      table_plot <- table_plot +
        ggplot2::theme(
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.text.y.left = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
    }
    if (!reserve_table_xaxis_label_space) {
      table_plot <- table_plot +
        ggplot2::theme(
          axis.text.x= ggplot2::element_blank(),
          axis.ticks.x= ggplot2::element_blank()
        )
    }
    if (table_title!="") {
      table_plot <- table_plot +
      ggplot2::ggtitle(table_title)+
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = table_title_size, hjust=0.5, vjust=1,
            margin = ggplot2::margin(b = ggplot2::unit(6, "pt")))
        )
    }
    
    if (!table_panel_border) {
      table_plot <- table_plot +
        ggplot2::theme(
          panel.border = ggplot2::element_blank()
        )
    }
    
    if (theme_benrich){
      table_plot <- table_plot +
        ggplot2::ggtitle(table_title)+
         ggplot2::theme(
           plot.title=ggplot2::element_text(
             size=table_title_size,hjust=0.5, vjust=1, margin=
               ggplot2::margin(b=ggplot2::unit(6, "pt"))),
           strip.background=ggplot2::element_blank(),
           panel.border = ggplot2::element_blank(),
           panel.spacing = ggplot2::unit(0, "pt"),
           axis.ticks = ggplot2::element_blank(),
           plot.margin = ggplot2::margin(t=3,r=3,b=3,l=3,unit="pt")
         )
      if (show_table_facet_strip %in% c("y")) {
        table_plot <- table_plot +
          ggplot2::theme(
            strip.text.y= table.y.strip.text
          )
      }
      if (show_table_facet_strip %in% c("x")) {
        table_plot <- table_plot +
          ggplot2::theme(
            strip.text.x = table.x.strip.text
          )
      }  
      if (show_table_facet_strip %in% c("both")) {
        table_plot <- table_plot +
          ggplot2::theme(
            strip.text.y= table.y.strip.text,
            strip.text.x= table.x.strip.text
          )
      }    
      
      if (break_ylabel) {
        table_plot <- table_plot +
          ggplot2::scale_y_discrete(labels = label_wrap(y_label_text_width)) 
      }
    }
  }

  if (table_position == "none") {
    result <- main_plot
  } else if (table_position == "right") {
    result <- egg::ggarrange(
      main_plot,
      table_plot,
      nrow = 1,
      widths = c(plot_table_ratio, 1)
    )
  } else if (table_position == "below") {
    result <- egg::ggarrange(
      main_plot,
      table_plot,
      nrow = 2,
      heights = c(plot_table_ratio, 1)
    )
  }
if (return_list){
  if(table_position == "none") result <-  list(main_plot)
  if(table_position != "none") result <-  list(main_plot,table_plot)
}
  if (!return_list){
  result <- result
  }
  result
}

#' Horizontal key drawing functions from ggstance in case it is deprecated
#'
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @name draw_key
globalVariables(c("alpha", ".pt"))

#' @rdname draw_key
#' @export
draw_key_hpath <- function(data, params, size) {
  grid::segmentsGrob(0.1, 0.5, 0.9, 0.5,
               gp = grid::gpar(
                 col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
                 lwd = (data$size %||% 0.5) * .pt,
                 lty = data$linetype %||% 1,
                 lineend = "butt"
               ),
               arrow = params$arrow
  )
}

#' @rdname draw_key
#' @export
draw_key_pointrangeh <- function(data, params, size) {
  grid::grobTree(
    draw_key_hpath(data, params, size),
    ggplot2::draw_key_point(transform(data, size = (data$size %||% 1.5) * 4), params)
  )
}

