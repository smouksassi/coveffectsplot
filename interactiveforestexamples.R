source("interactiveforest.R")

library(dplyr)


plotdata <- read.csv("forest-plot-table.csv")
plotdata <- plotdata %>%
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))


param <- "BZD AUC"
plotdata <-  filter(plotdata,paramname==param)
plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
plotdata$label <- reorder(plotdata$label,plotdata$scen)

forest_plot(plotdata,
            ref_legend_text = "Reference (vertical line)",
            area_legend_text = "Reference (vertical line)",
            xlabel = paste("Fold Change in", param, "Relative to Reference"),
            show_ref_area = FALSE,
            facet_formula = "covname~.",
            facet_scales = "free_y",
            facet_space = "free_y",
            show_table_facet_strip = FALSE,
            table_position = "right",
            plot_table_ratio = 4)

plotdata <- read.csv("forest-plot-table.csv")
plotdata <- plotdata %>%
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
param <- c("BZD AUC","BZD Cmax")
plotdata <-  filter(plotdata,paramname%in%param)
plotdata <-  filter(plotdata,covname%in%"WEIGHT")

plotdata$covname <- reorder(plotdata$covname,plotdata$upper,FUN =max)
plotdata$label <- reorder(plotdata$label,plotdata$scen)

forest_plot(plotdata,
            ref_legend_text = "Reference (vertical line)",
            area_legend_text = "Reference (vertical line)",
            xlabel = paste("Fold Change in", param[1], "Relative to Reference"),
            show_ref_area = FALSE,
            facet_formula = "covname~paramname",
            facet_scales = "free_y",
            facet_space = "free_y",
            x_facet_text_size = 14,
            y_facet_text_size = 14,
            facet_switch = "both",
            show_table_facet_strip = TRUE,
            table_position = "below",
            plot_table_ratio = 1)


plotdata <- read.csv("forestplotdatacpidata.csv")
forest_plot(plotdata,
            ref_area = c(0.8, 1.2),
            x_facet_text_size = 13,
            y_facet_text_size = 13,
            ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            xlabel = "Fold Change Relative to RHZE",
            facet_formula = "covname~paramname",
            table_position = "below")




plotdata<- read.csv("./data/dfall.csv")
plotdata <- plotdata %>%
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
plotdata <- plotdata %>%
  filter(paramname%in%c("CL"))

forest_plot(plotdata,
            ref_area = c(0.8, 1.2),
            x_facet_text_size = 13,
            y_facet_text_size = 13,
            ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            xlabel = "Fold Change Relative to RHZE",
            facet_formula = "covname~.",
            facet_switch = "none",
            facet_scales = "free_y",
            facet_space = "fixed",
            table_position = "right",
            plot_table_ratio = 4)


########
plotdata<- read.csv("dataforest.csv")
plotdata <- plotdata %>%
  mutate(midlabel = format(round(mid,2), nsmall = 2),
         lowerlabel = format(round(lower,2), nsmall = 2),
         upperlabel = format(round(upper,2), nsmall = 2),
         LABEL = paste0(midlabel, " [", lowerlabel, "-", upperlabel, "]"))
plotdata <- plotdata %>%
  filter(covname%in%c("Baseline TTR","Weight"))

forest_plot(plotdata,
            ref_area = c(0.8, 1.2),
            x_facet_text_size = 13,
            y_facet_text_size = 13,
            ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            xlabel = "Fold Change Relative to Parameter",
            facet_formula = "covname~paramname",
            facet_switch = "both",
            facet_scales = "free",
            facet_space = "fixed",
            table_position = "below",
            plot_table_ratio = 1)

forest_plot(plotdata,
            ref_area = c(0.8, 1.2),
            x_facet_text_size = 13,
            y_facet_text_size = 13,
            ref_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            area_legend_text = "Reference (vertical line)\n+/- 20% limits (colored area)",
            xlabel = "Fold Change Relative to Parameter",
            facet_formula = "covname~.",
            facet_switch = "both",
            facet_scales = "free",
            facet_space = "fixed",
            paramname_shape = TRUE,
            table_position = "below",
            plot_table_ratio = 1)
