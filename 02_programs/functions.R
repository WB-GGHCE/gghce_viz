# This file contains a number of functions useful for making standardized charts for GGHCE

# Load necessary libraries
library(tidyverse) #for data manipulation
library(here) #for file paths
library(haven) #for reading in stata files
library(wbstats) #for world bank data
library(ggthemes) #for ggplot themes
library(ggtext) #for ggplot text formatting
library(assertthat) #for assertions of data quality
library(ggrepel) #for ggplot labels that do not overlap
library(janitor) #for cleaning column names
library(zoo) #for na.locf function. Last Observation Carried Forward
library(haven) #for data saving to stata
library(Hmisc) #for data saving to stata
library(estimatr) #for robust standard errors in regression
library(hrbrthemes) #for ggplot themes
library(readxl) #for reading in excel files
library(paletteer) #for color palettes
library(patchwork) #for combining ggplots
library(quantreg) #for quantile regression


# set color palletes
# set region colors
region_colors <- c(
    "North America" = "#1f77b4",
    "Europe & Central Asia" = "#ff7f0e",
    "Sub-Saharan Africa" = "#2ca02c",
    "East Asia & Pacific" = "#d62728",
    "Latin America & Caribbean" = "#9467bd",
    "Middle East & North Africa" = "#8c564b",
    "South Asia" = "#e377c2"
)

# set income colors
income_colors <- c(
    "Low income" = "#fb8500",
    "Lower middle income" = "#ffb703",
    "Upper middle income" = "#219ebc",
    "High income" = "#023047"
)


######################################
# Create a function to add equations to plots
######################################
# add equations to plots
# This function takes in a dataset, an input variable, and a dependent variable
# It then calculates the equation of the line of best fit
# It returns the equation of the line of best fit
eq_plot_txt <- function(data, inp, var) {
    eq <- lm_robust(data[[var]] ~ data[[inp]], data = data, se_type = "HC2")
    coef <- round(coef(eq), 2)
    std_err <- round(sqrt(diag(vcov(eq))), 2)
    r_2 <- round(summary(eq)$r.squared, 2)
    sprintf(" y = %.2f + %.2f x, R<sup>2</sup> = %.2f <br> (%.2f) <span style='color:white'> %s</span> (%.2f) ", coef[1], coef[2], r_2[1], std_err[1], "s", std_err[2])
}




# Create a theme for ggplot useful for many plots
theme_bws <- function() {
    font <- "Andes"

      theme_minimal() %+replace%
        theme(
            plot.caption = element_text(hjust = 0),
            plot.title = element_blank(), # remove all titles from plots (sometimes we may need to bring title outside plot)

            # This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
            plot.subtitle = ggplot2::element_text(
                family = font,
                size =  16,
                margin = ggplot2::margin(9, 0, 9, 0)
            ),
            # plot.caption = ggplot2::element_blank(),
            # This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
            # Legend format
            # This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
            legend.position = "top",
            legend.text.align = 0,
            legend.background = ggplot2::element_blank(),
            legend.title = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(
                family = font,
                size = 14,
                color = "#222222"
            ),
    
            # Axis format
            # This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
            axis.title = ggplot2::element_text(
                family = font,
                size = 12,
                color = "#222222"
            ),
            axis.text = ggplot2::element_text(
                family = font,
                size = 11,
                color = "#222222"
            )
        )
}
# define stle for ggplot based on BBC plotting styles
bbc_style <- function() {
    font <- "Andes"

    ggplot2::theme(

        # Text format:
        # This sets the font, size, type and colour of text for the chart's title
        plot.title = ggplot2::element_text(
            family = font,
            size = 18,
            face = "bold",
            color = "#222222"
        ),
        # This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(
            family = font,
            size =  16,
            margin = ggplot2::margin(9, 0, 9, 0)
        ),
        # plot.caption = ggplot2::element_blank(),
        # This leaves the caption text element empty, because it is set elsewhere in the finalise plot function

        # Legend format
        # This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
        legend.position = "top",
        legend.text.align = 0,
        legend.background = ggplot2::element_blank(),
        legend.title = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(
            family = font,
            size = 14,
            color = "#222222"
        ),

        # Axis format
        # This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
        axis.title = ggplot2::element_text(
            family = font,
            size = 12,
            color = "#222222"
        ),
        axis.text = ggplot2::element_text(
            family = font,
            size = 11,
            color = "#222222"
        ),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
        axis.ticks = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),

        # Grid lines
        # This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.y = ggplot2::element_blank(),

        # Blank background
        # This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
        panel.background = ggplot2::element_blank(),

        # Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
        strip.background = ggplot2::element_rect(fill = "white"),
        strip.text = ggplot2::element_text(size = 22, hjust = 0)
    )
}


######################################
# Create a function to plot a beeswarm plot
######################################

#This function takes in a dataset, an indicator, the name of the indicator, and a title
#It then creates a beeswarm plot of the indicator
#It then adds a line for the global average and regional averages
#It returns the plot
beeswarm_plotter <- function(data, indicator, indicator_name, title){
  data <- get(data) %>%
    mutate(
      value= !!sym(indicator)
    )
  # calculate global average
  world_avg <-
    data %>%
    ungroup() %>%
    summarise(avg = wtd.mean(value, na.rm = T, weights=pop_0_4_2025)) %>%
    pull(avg)
  
  world_p25 <- round(quantile(data$value, p = 0.25, na.rm = TRUE),2)
  world_p50 <- round(quantile(data$value, p = 0.5, na.rm = TRUE),2)
  world_p75 <- round(quantile(data$value, p = 0.75, na.rm = TRUE),2)
  
  # calculate regional average
  reg_avg <-
    data %>%
    filter(!is.na(region)) %>%
    group_by(region) %>%
    summarise(avg = wtd.mean(value, na.rm = T, weights=pop_0_4_2025)) %>%
    arrange(-avg)
  
  reg_avg <- reg_avg %>%
    mutate(region = factor(region, levels = reg_avg$region))
  
  # set annotatino arrows
  arrows <-
    tibble(
      x1 = c(5),
      x2 = c(4.6),
      y1 = c(world_avg - 1.5*sd(reg_avg$avg)),
      y2 = c(world_avg - .15*sd(reg_avg$avg))
    )
  
  data %>%
    filter(!is.na(region)) %>%
    mutate(region = factor(region, levels = reg_avg$region)) %>%
    ggplot(aes(y = value, x = region, color = region)) +
    geom_segment(
      data = reg_avg,
      aes(
        x = region, xend = region,
        y = world_avg, yend = avg
      ),
      size = 0.8
    ) +
    geom_jitter(aes(size = pop_0_4_2025), alpha = 0.5, width = 0.2) +
    geom_text(data = reg_avg, aes(y = avg, x = region, label = scales::comma(round(avg, 3))), color = "black", vjust=-.8,position = position_dodge(), size=4) +
    geom_hline(aes(yintercept = world_avg), color = "gray70", size = 0.6) +
    annotate( # annotations
      "text",
      x = 5.5, y = world_avg-1.5*sd(reg_avg$avg), family = "Andes", size = 4, color = "gray20", lineheight = .9,
      label = glue::glue("Average across all countries:\n{round(world_avg, 3)}")
    ) +
    geom_curve( # add curves for annotatino
      data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
      arrow = arrow(length = unit(0.08, "inch")), size = 0.5,
      color = "gray20", curvature = -0.3
    ) +
    labs(caption = str_wrap(paste0("The 25th percentile of the indicator is ", world_p25, ". The median is ", world_p50, " and the 75th percentile is ", world_p75, ".", " Means are weighted by under 5 population.  Circles are sized according to under 5 population"), 80)) +
    theme_bw() +
    scale_color_manual(
      values=region_colors
    ) +
    coord_flip() +
    ylab(indicator_name) +
    scale_y_continuous(
      labels=scales::comma
    ) +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      text = element_text(size = 14)
    )
  
}

######################################
#scatterplot functions
######################################

#This function takes in a dataset, an indicator, an x variable, the name of the x variable, the name of the indicator, and a title
#It then creates a scatterplot of the indicator on the x variable
#It then adds a line of best fit 
#It returns the plot
scatter_best_fit_plotter <- function(data, indicator, xvar, indicator_name, xvar_name, title){
  data <- get(data) %>%
    mutate(
      value= !!sym(indicator),
      xvar = !!sym(xvar)
    ) %>%
    filter(!is.na(region))
  
  eq_location <- data.frame(
    y = min(data$value, na.rm=TRUE) * 1.1,
    x = max(data$xvar, na.rm=TRUE) * 0.6
  )

    data  %>%
    mutate(region = factor(region, levels = unique(data$region))) %>%
    ggplot(aes(y = value, x = xvar)) +
    geom_point(aes(size = pop_0_4_2025, color = region), alpha = 0.75, width = 0.2) +
    geom_text(aes(label=country, color = region), check_overlap = TRUE, show_guide=FALSE) +
    geom_smooth(method='lm',formula = y ~ splines::bs(x, 3), se=FALSE,  aes(linetype='Line of Best Fit')) +
    scale_x_log10(labels = scales::comma) +
    # geom_richtext(
    #   data = eq_location, aes(x = x, y = y, label = eq_plot_txt(data, xvar, indicator)), hjust = 0.2
    # ) +    
    theme_bws() +
    scale_color_manual(
      values=region_colors
    ) +
    xlab(xvar_name) +
    ylab(indicator_name) +
    scale_y_continuous(
      labels=scales::comma
    ) +
    labs(linetype="Legend")+
    scale_linetype_manual(values=c('Line of Best Fit'='solid')) +
    scale_size(guide=FALSE) +
    #expand_limits(y=c(0,1)) +
    theme(
      #axis.title.y = element_blank(),
      text = element_text(size = 12),
      legend.position="bottom",
      legend.box='vertical'
    ) +
    guides(color=guide_legend(
      ncol=4,
        theme=theme(
          #legend.position.inside=c(min(data$xvar, na.rm=TRUE)+2*sd(data$xvar, na.rm=TRUE),min(data$value, na.rm=TRUE)+.1*sd(data$value, na.rm=TRUE)),
          #legend.position=c(.8,.7),
          legend.text=element_text(size=10)
        )
      ),
      linetype=guide_legend(
      ncol=2,
        theme=theme(
          legend.position="bottom",
          legend.text=element_text(size=10)
      )
      )
    )
  
}

#This function takes in a dataset, an indicator, an x variable, the name of the x variable, the name of the indicator, and a title
#It then creates a scatterplot of the indicator on the x variable
#It then adds a line of best fit and a 90th percentile prediction interval (PPF)
#It returns the plot
scatter_ppf_plotter <- function(data, indicator, xvar, indicator_name, xvar_name, title){
  data <- get(data) %>%
    mutate(
      value= !!sym(indicator),
      xvar = !!sym(xvar)
    ) %>%
    filter(!is.na(region))
  
  eq_location <- data.frame(
    y = min(data$value, na.rm=TRUE) * 1.1,
    x = max(data$xvar, na.rm=TRUE) * 0.6
  )

    data  %>%
    mutate(region = factor(region, levels = unique(data$region))) %>%
    ggplot(aes(y = value, x = xvar)) +
    geom_point(aes(size = pop_0_4_2025, color = region), alpha = 0.75, width = 0.2) +
    geom_text(aes(label=country, color = region), check_overlap = TRUE, show_guide=FALSE) +
    geom_smooth(method='lm',formula = y ~ splines::bs(x, 3), se=FALSE,  aes(linetype='Line of Best Fit')) +
    geom_quantile(quantiles=c(0.9),formula = y ~ splines::bs(x, 3), se=FALSE,  aes(linetype='PPF')) +
    scale_x_log10(labels = scales::comma) +
    # geom_richtext(
    #   data = eq_location, aes(x = x, y = y, label = eq_plot_txt(data, xvar, indicator)), hjust = 0.2
    # ) +    
    theme_bws() +
    scale_color_manual(
      values=region_colors
    ) +
    xlab(xvar_name) +
    ylab(indicator_name) +
    scale_y_continuous(
      labels=scales::comma
    ) +
    labs(linetype="Legend")+
    scale_linetype_manual(values=c('Line of Best Fit'='solid', "PPF"='dashed')) +
    scale_size(guide=FALSE) +
    #expand_limits(y=c(0,1)) +
    theme(
      #axis.title.y = element_blank(),
      text = element_text(size = 12),
      legend.position="bottom",
      legend.box='vertical'
    ) +
    guides(color=guide_legend(
      ncol=4,
        theme=theme(
          #legend.position.inside=c(min(data$xvar, na.rm=TRUE)+2*sd(data$xvar, na.rm=TRUE),min(data$value, na.rm=TRUE)+.1*sd(data$value, na.rm=TRUE)),
          #legend.position=c(.8,.7),
          legend.text=element_text(size=10)
        )
      ),
      linetype=guide_legend(
      ncol=2,
        theme=theme(
          legend.position="bottom",
          legend.text=element_text(size=10)
      )
      )
    )
  
}

#another scatterploter but not producing ppf or line of best fit
#This function takes in a dataset, an indicator, an x variable, the name of the x variable, the name of the indicator, and a title
#It then creates a scatterplot of the indicator on the x variable
#It then adds a 45 degree line
#It returns the plot
scatter_45degree_plotter <- function(data, indicator, xvar, indicator_name, xvar_name, title){
  data <- get(data) %>%
    mutate(
      value= !!sym(indicator),
      xvar = !!sym(xvar)
    ) %>%
    filter(!is.na(region))
  
  eq_location <- data.frame(
    y = min(data$value, na.rm=TRUE) * 1.1,
    x = max(data$xvar, na.rm=TRUE) * 0.6
  )

    data  %>%
    mutate(region = factor(region, levels = unique(data$region))) %>%
    ggplot(aes(y = value, x = xvar)) +
    geom_point(aes(size = pop_0_4_2025, color = region), alpha = 0.75, width = 0.2) +
    geom_text(aes(label=country, color = region), check_overlap = TRUE, show_guide=FALSE) +
    #add 45 degree line
    geom_abline(intercept = 0, slope = 1, aes(linetype='45 degree line')) +
    theme_bws() +
    scale_color_manual(
      values=region_colors
    ) +
    xlab(xvar_name) +
    ylab(indicator_name) +
    scale_y_continuous(
      labels=scales::comma
    ) +
    labs(linetype="Legend")+
    scale_linetype_manual(values=c('45 degree line'='dotdash')) +
    scale_size(guide=FALSE) +
    #expand_limits(y=c(0,1)) +
    theme(
      #axis.title.y = element_blank(),
      text = element_text(size = 12),
      legend.position="bottom",
      legend.box='vertical'
    ) +
    guides(color=guide_legend(
      ncol=4,
        theme=theme(
          #legend.position.inside=c(min(data$xvar, na.rm=TRUE)+2*sd(data$xvar, na.rm=TRUE),min(data$value, na.rm=TRUE)+.1*sd(data$value, na.rm=TRUE)),
          #legend.position=c(.8,.7),
          legend.text=element_text(size=10)
        )
      ),
      linetype=guide_legend(
      ncol=2,
        theme=theme(
          legend.position="bottom",
          legend.text=element_text(size=10)
      )
      )
    )
  
}


######################################
# Create a function to plot a map
######################################
# Load the maps

load(here("01_raw_data","misc", 'maps.Rdata'))
standard_crop_wintri <- function() {
  l <- list(
    left=-12000000, right=16396891,
    top=9400000, bottom=-6500000
  )
  l$xlim <- c(l$left, l$right)
  l$ylim <- c(l$bottom, l$top)
  l
}

# Create a function to plot a map
# This function takes in a dataset, an indicator, the name of the indicator, and a title
# It then creates a map of the indicator
# It returns the plot
map_plotter <- function(data, indicator, indicator_name, title){
  
  
  map_df <- get(data) %>%
    filter(!(country %in% c('Greenland'))) %>% #drop a few countries for which we do not collect data.
    group_by( country) %>%
    #summarise(across(!! indicator,last)) %>%
    rename(value=!! indicator) %>%
    select(iso3c, value) %>%
    right_join(country_metadata) %>%
    mutate(value=if_else(is.na(value), as.numeric(NA), as.numeric(value))) 
  
  
  
  p1<-ggplot() +
    geom_map(data = map_df, aes(map_id = iso3c, fill = value), map = maps$countries) + 
    geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") + 
    geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white")  +
    geom_path(data = maps$boundaries,
              aes(long, lat, group = group),
              color = "white",
              size = 0.3,
              lineend = maps$boundaries$lineend,
              linetype = maps$boundaries$linetype) +
    scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
    scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
    coord_equal() +
    theme_map(base_size=12) +
    theme(legend.position = "bottom",
          #extend length of legend
          legend.key.width=unit(1.5,'cm')) +
    labs(title = title, fill = indicator_name) +
    scale_fill_paletteer_binned("ggthemes::Temperature Diverging", name = indicator_name, na.value = "grey50", direction=-1) 
  
  p1
}


######################################
#Create charts for over and under performers
######################################
#This function takes in a dataset, an indicator, an x variable, the name of the x variable, the name of the indicator, and a title
#It then calculates the residuals from a cubic spline regression of the indicator on the x variable
#It then selects the top 10 overperformers and underperformers and plots them
#It also selects the top overperformer and underperformer from each region and plots them
#It then plots the overperformers and underperformers
#It returns the plot
overperformers_plotter <- function(data, indicator, xvar, xvar_name, indicator_name, title){
  data=get(data)
  form <- paste(indicator,' ~ splines::bs(',xvar,', 3)', sep="")
  model <- lm(form , data = data) 
  
  overperformers_df <- data %>%
    #drop small countries
    filter(pop_2025>1e6) %>%
    modelr::add_residuals(model) %>%
    modelr::add_predictions(model) %>%
    arrange(-resid) %>%
    head(10)
  
  overperformers_region <- data %>%
    #drop small countries
    filter(pop_2025>1e6) %>%
    filter(!is.na(region)) %>%
    modelr::add_residuals(model) %>%
    modelr::add_predictions(model) %>%
    filter(resid>0) %>%
    arrange(-resid) %>%
    group_by(region) %>%
    slice_head(n=1)
  

  underperformers_df <- data %>%
    #drop small countries
    filter(pop_2025>1e6) %>%
    modelr::add_residuals(model) %>%
    modelr::add_predictions(model) %>%
    arrange(resid)   %>%
    head(10)

  
  underperformers_region <- data %>%
    #drop small countries
    filter(pop_2025>1e6) %>%
    filter(!is.na(region)) %>%
    modelr::add_residuals(model) %>%
    modelr::add_predictions(model) %>%
    filter(resid<0) %>%
    arrange(resid) %>%
    group_by(region) %>%
    slice_head(n=1)
  
  
  overperformers_plot <-  overperformers_df %>%
    bind_rows(overperformers_region) %>%
    bind_rows(underperformers_region) %>%
    bind_rows(underperformers_df) %>%
    arrange(resid) %>%
    #drop duplicates by country
    distinct(country, .keep_all = TRUE) 
  
  overperformers_plot <- overperformers_plot %>%
    mutate(country=factor(country, levels=unique(overperformers_plot$country)),
           group=if_else(resid>=0, 'above','below')) %>%
    ggplot(aes(x=country, y=resid, color=group)) +
    geom_segment( aes(x=country ,xend=country, y=0, yend=resid), color="black") +
    geom_point(size=3) +
    scale_color_manual(name=indicator_name,
                       labels = c("Over-performers", "Under-performers"),
                       values = c('above'="#1A9850", 'below'="#D73027")) +
    coord_flip() +
    theme_bws() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = 'bottom',
      axis.text.y = element_text(size=10),
      axis.title.x=element_text(size=10)
    ) +
    xlab("") +
    ylab(paste0("Difference from Expected Based on ", xvar_name)) +
    labs(subtitle=indicator_name,
         caption=str_wrap(paste0('Over and under performers calculated for Indicators by using cubic spline regression of ',indicator_name,' on ',xvar_name , ' and calculting residuals from this regression.  '),100))
  #subtitle = 'Difference from Expected shown as Percent of Predicted Value')
  
  return(overperformers_plot)
}

