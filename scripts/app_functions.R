# Functions to make plots for the Shiny app


# LIBRARIES --------------------------------------------------------------
library(boot)
library(forcats)
library(dplyr)
library(ggplot2)


# COLOR PALETTE ----------------------------------------------------------
color_palette <- c("1" = "#a6611a", "2" = "#dfc27d", "3" = "#f5f5f5", "4" = "#80cdc1", "5" = "#018571")


# HTML checkbox label formatting ------------------------------------------

# content of the map rating labels
list_content <- function(col,content){
  paste0('<div style="display:flex;"><i class="fa fa-circle"
                                         style="color:',
         col,';margin-top:3px;opacity:0.8;"></i><div style="color:black;padding-left:5px;">',
         content,'</div></div>')
}

htmlchoicenames <- list(HTML(list_content(color_palette[5],"5 (accept)")),
     HTML(list_content(color_palette[4],"4")),
     HTML(list_content(color_palette[3],"3")),
     HTML(list_content(color_palette[2], "2")),
     HTML(list_content(color_palette[1], "1 (reject)")))

# FACTOR LEVELS ----------------------------------------------------------
education_levels <- c("High school" = "Some high school, no diploma", "High school" = "High school diploma", "Some coll." = "Some college, no degree", "Associate" = "Associate degree", "Bachelor's" = "Bachelor's degree", "Graduate" = "Graduate degree")

age_bin_levels <- c("18-30" = "18-30", "31-40" = "31-40", "41-50" = "41-50", "51-60" = "51-60", "61+" = "61-70", "61+" = "71-80", "61+" = "81-90", "61+" = "91-100")

income_levels <- c("1-12.5", "12.5-25", "25-37.5", "37.5-50", "50-62.5", "62.5-75", "75-87.5", "87.5-100", "100+")

urban_rural_levels <- c("Rural \n (pop < 15K)", "UC \n (pop 15-50K)", "Urban \n (pop > 50K)")

demographic_vars <- c("Age group" = "AgeBin", "Gender", "Education", "Income bracket" = "Income", "Race", "ANAE dialect region" = "RegANAE", "Carver dialect region" = "CarverReg") # demographic var choices for the in-app dropdown menu


# MEAN FUNCTION ----------------------------------------------------------
mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE) # mean function for use in bootstrap (for some reason I had to redefine it, can't just use mean())

# LABELER ----------------------------------------------------------------
labeler <- function(df, xvar, yvar, order_by_mean = F, levels = NULL){
  labsdf <- df %>%
    filter(!is.na(.data[[xvar]])) %>% # remove NA's for the variable of interest
    group_by(.data[[xvar]]) %>% # group by the variable
    summarize(n = n(), # get sample size
              mean = mean(.data[[yvar]]), # mean
              sd = sd(.data[[yvar]]), # standard deviation 
              ylower = quantile(boot(.data[[yvar]], # lower 95 ci bound
                                     mean.fun, 
                                     R = 1000, 
                                     sim="ordinary")$t, 
                                0.025),
              yupper = quantile(boot(.data[[yvar]], # upper 95 ci bound
                                     mean.fun, 
                                     R = 1000, 
                                     sim = "ordinary")$t, 
                                0.975)) %>%
    mutate(lab = paste0(.data[[xvar]], " \n (n = ", n, ")")) %>% # make sample size labels
    {if(order_by_mean) arrange(., -mean) else .} %>% # arrange by mean
    {if(order_by_mean) mutate(., !!xvar := factor(.data[[xvar]], levels = unique(.data[[xvar]]))) else .} %>% # now that we've ordered by mean, set factor levels to keep this order.
    {if(!order_by_mean & !is.null(levels)) mutate(., !!xvar := factor(.data[[xvar]], levels = levels)) else .} %>% # level
    {if(!order_by_mean) arrange(., .data[[xvar]]) else .}
  return(labsdf)
}


# SENTENCE BARPLOTS ------------------------------------------------------
sentence_barplot <- function(df, xvar, yvar, 
                             order_by_mean = F, levels = NULL, group_other = F,
                             xlab = F, ylab = F, show_legend = T, show_n = T){
  # Group small sample sizes into an "other" category
  if(group_other){
    df <- df %>%
      group_by(.data[[xvar]]) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(!!xvar := case_when(n < 10 ~ "Other",
                                 TRUE ~ .data[[xvar]])) %>%
      dplyr::select(-n)
  }
  
  # Generate sample size labels
  l <- labeler(df, {{xvar}}, {{yvar}},
               order_by_mean = order_by_mean, levels = levels)
  
  plot <- df %>% 
    mutate(!!xvar := factor(.data[[xvar]])) %>% # make x a factor
    mutate(!!xvar := fct_relevel(.data[[xvar]], levels(l %>% pull(xvar)))) %>% # relevel factor variable x
    mutate(!!yvar := factor(.data[[yvar]])) %>% # factor y
    ggplot(aes(fill = .data[[yvar]], x = .data[[xvar]]))+
    geom_bar(position = position_fill(reverse = T), stat = "count", col = "black")+
    scale_fill_manual(values = color_palette)+
    labs(fill = "Judgment")+
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          text = element_text(size = 12)
    )+
    {if(show_n) scale_x_discrete(labels = l$lab)}+ # this is a hack: only works if labeler sorts by factor levels when order_by_mean = T
    {if(xlab) xlab(xvar) else theme(axis.title.x = element_blank())}+
    {if(ylab) ylab("Proportion") else theme(axis.title.y = element_blank())}+
    {if(!show_legend) guides(fill = FALSE)}+
    NULL
  return(plot)
}
