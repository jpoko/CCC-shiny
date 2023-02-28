# global.R

# LIBRARIES ----

library(tidyverse)
library(lubridate)    # to deal with dates
library(markdown)     # to include markdown files
library(shinythemes)
library(leaflet)      # for interactive maps

library(sjlabelled)   # add label to variable names
library(ggeasy)       # add label names to ggplot

library(plotly)       # interactive plots
library(viridis)      # color palette
library(ggforce)      # sina plots

library(psych)        # correlations with p values
library(ggcorrplot)   # correlation matrices
library(corrplot)     # view correlation matrices as heatmap
library(ppcor)        # partial correlations

library(ggcorset)     # corset plots

library(shinyhelper)  # helper pop-ups
library(fontawesome)  # icons
library(shinyWidgets) # custom input widgets
library(shinyjs)      # extend shiny

library(conflicted)   # checks if have conflicting function names


# LOAD DATA ----
dat.long <- readRDS("./data/app_data_long.rds")
zipcode.dat <- readRDS("./data/zipcode_data.rds")
dat.mini <- readRDS("./data/app_mini_data.rds")
scale.correlations.heatmap.dat <- load("./data/app_btwScaleCorrelations_data.RData")
meditation.correlations.heatmap.dat <- load("./data/app_btwMeditationCorrelations_data.RData")

# Add flag for when no demographic category is selected (for scatter plot), all
# individuals are shown
dat.long <- dat.long |>
    mutate(no.demo = "All")

#to test code:
# input <- list(demo.treemaps.var = "Gender",
#               scale.type.var = "pss",
#               meditation.var = "med.years",
#               timepoint.var = "T1",
#               scale.complete = "All",
#               fit = TRUE,
#               demographic.var = "gender",
#               corr.matrix.sig.p.val = 0.05,
#               corr.matrix.type = "meditation variables",
#               scale.type.var.single = "stai"
# )

# PREP DATA ----

## TREEMAPS ----

# create demographic treemaps

demographic.treemaps <- list(

    `Age (binned)` = plot_ly(
        type = "treemap",
        labels = count(dat.mini, `Age (binned)`)$`Age (binned)`,
        values = count(dat.mini, `Age (binned)`)$n,
        parents = rep("", nrow(count(dat.mini, `Age (binned)`))),
        marker = list(
            colors = viridis(nrow(count(dat.mini, `Age (binned)`))),
            line = list(color = "#000000",
                        width = 1)),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", count(dat.mini, `Age (binned)`)$n)
    ),
    Gender = plot_ly(
        type = "treemap",
        labels = count(dat.mini, Gender)$Gender,
        values = count(dat.mini, Gender)$n,
        parents = rep("", nrow(count(dat.mini, Gender))),
        marker = list(
            colors = viridis(nrow(count(dat.mini, Gender))),
            line = list(color = "#000000",
                        width = 1)),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", count(dat.mini, Gender)$n)
    ),
    Ethnicity = plot_ly(
        type = "treemap",
        labels = count(dat.mini, Ethnicity)$Ethnicity,
        values = count(dat.mini, Ethnicity)$n,
        parents = rep("", nrow(count(dat.mini, Ethnicity))),
        marker = list(
            colors = viridis(nrow(count(dat.mini, Ethnicity))),
            line = list(color = "#000000",
                        width = 1)),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", count(dat.mini, Ethnicity)$n)
    ),
    Orientation = plot_ly(
        type = "treemap",
        labels = count(dat.mini, Orientation)$Orientation,
        values = count(dat.mini, Orientation)$n,
        parents = rep("", nrow(count(dat.mini, Orientation))),
        marker = list(
            colors = viridis(nrow(count(dat.mini, Orientation))),
            line = list(color = "#000000",
                        width = 1)),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", count(dat.mini, Orientation)$n)
    ),
    Education = plot_ly(
        type = "treemap",
        labels = count(dat.mini, Education)$Education,
        values = count(dat.mini, Education)$n,
        parents = rep("", nrow(count(dat.mini, Education))),
        marker = list(
            colors = viridis(nrow(count(dat.mini, Education))),
            line = list(color = "#000000",
                        width = 1)),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", count(dat.mini, Education)$n)
    ),
    Income = plot_ly(
        type = "treemap",
        labels = count(dat.mini, Income)$Income,
        values = count(dat.mini, Income)$n,
        parents = rep("", nrow(count(dat.mini, Income))),
        marker = list(
            colors = viridis(nrow(count(dat.mini, Income))),
            line = list(color = "#000000",
                        width = 1)),
        textposition = "inside",
        textinfo = "label",
        hoverinfo = "text",
        text = ~paste("n=", count(dat.mini, Income)$n)
    )
)


## CORRELATION MATRICES ----

# View correlation matrices using corrplot package - pass already computed
# correlation and p-value matrices 
correlation.heatmap <- function(corr.time, p.mat.time, sig.level, ...) {
    corrplot(
        corr.time,
        method = "color",
        col = heat.col(200),
        addCoef.col = "black",
        tl.col = "black",
        tl.srt = 45,
        p.mat = p.mat.time,
        sig.level = sig.level,
        insig = "blank",
        diag = FALSE
    )
}

# create nested list for correlation coefficient values - name should match
# input$corr.matrix.type + time point (t1-t4)

nested_corr <- list(
    list(data = corr.t1, name = "well-being scales t1"),
    list(data = med.corr.t1, name = "meditation variables t1"),
    list(data = corr.t2, name = "well-being scales t2"),
    list(data = med.corr.t2, name = "meditation variables t2"),
    list(data = corr.t3, name = "well-being scales t3"),
    list(data = med.corr.t3, name = "meditation variables t3"),
    list(data = corr.t4, name = "well-being scales t4"),
    list(data = med.corr.t4, name = "meditation variables t4")
)

# create nested list for correlation coefficient p values - name should match
# input$corr.matrix.type + time point (t1-t4)
nested_p <- list(
    list(data = p.mat.t1, name = "well-being scales t1"),
    list(data = med.p.mat.t1, name = "meditation variables t1"),
    list(data = p.mat.t2, name = "well-being scales t2"),
    list(data = med.p.mat.t2, name = "meditation variables t2"),
    list(data = p.mat.t3, name = "well-being scales t3"),
    list(data = med.p.mat.t3, name = "meditation variables t3"),
    list(data = p.mat.t4, name = "well-being scales t4"),
    list(data = med.p.mat.t4, name = "meditation variables t4")
)




## CORSET PLOT DATA


# For scales with 4 time points
calculate.changes.4tmpts <- function(list.scales) {
    
    for (i in seq_along(list.scales)) {
    
    ## Time t1 vs t2
    
    # name for change column 
    col_name <- paste(list.scales[i], "change", "t1", "t2", sep = ".")
    
    dat.mini <- dat.mini |>
        mutate({{col_name}} := !!sym(paste0(list.scales[i], ".total.t2")) - !!sym(paste0(list.scales[i], ".total.t1")))
    
    # name for direction column
    direction_name <- paste(list.scales[i], "direction", "t1", "t2", sep = ".")
    
    dat.mini[[direction_name]] <- ifelse(
        dat.mini[[col_name]] < 0, "Decrease",
        ifelse(dat.mini[[col_name]] > 0, "Increase",
               "No change"))
    
    colnames(dat.mini)[which(colnames(dat.mini) == direction_name)] <-
        direction_name
    
    
    ## Time t2 vs t3
    
    # name for change column 
    col_name <- paste(list.scales[i], "change", "t2", "t3", sep = ".")
    
    dat.mini <- dat.mini |>
        mutate({{col_name}} := !!sym(paste0(list.scales[i], ".total.t3")) - !!sym(paste0(list.scales[i], ".total.t2")))
    
    # name for direction column
    direction_name <- paste(list.scales[i], "direction", "t2", "t3", sep = ".")
    
    dat.mini[[direction_name]] <- ifelse(
        dat.mini[[col_name]] < 0, "Decrease",
        ifelse(dat.mini[[col_name]] > 0, "Increase",
               "No change"))
    
    colnames(dat.mini)[which(colnames(dat.mini) == direction_name)] <-
        direction_name
    
    ## Time t3 vs t4
    
    # name for change column 
    col_name <- paste(list.scales[i], "change", "t3", "t4", sep = ".")
    
    dat.mini <- dat.mini |>
        mutate({{col_name}} := !!sym(paste0(list.scales[i], ".total.t4")) - !!sym(paste0(list.scales[i], ".total.t3")))
    
    # name for direction column
    direction_name <- paste(list.scales[i], "direction", "t3", "t4", sep = ".")
    
    dat.mini[[direction_name]] <- ifelse(
        dat.mini[[col_name]] < 0, "Decrease",
        ifelse(dat.mini[[col_name]] > 0, "Increase",
               "No change"))
    
    colnames(dat.mini)[which(colnames(dat.mini) == direction_name)] <-
        direction_name
    
    
    ## Time t1 vs t4
    
    # name for change column 
    col_name <- paste(list.scales[i], "change", "t1", "t4", sep = ".")
    
    dat.mini <- dat.mini |>
        mutate({{col_name}} := !!sym(paste0(list.scales[i], ".total.t4")) - !!sym(paste0(list.scales[i], ".total.t1")))
    
    # name for direction column
    direction_name <- paste(list.scales[i], "direction", "t1", "t4", sep = ".")
    
    dat.mini[[direction_name]] <- ifelse(
        dat.mini[[col_name]] < 0, "Decrease",
        ifelse(dat.mini[[col_name]] > 0, "Increase",
               "No change"))
    
    colnames(dat.mini)[which(colnames(dat.mini) == direction_name)] <-
        direction_name
    }
    dat.mini
}



## For scales with 2 time points
calculate.changes.2tmpts <- function(list.scales) {
    
    for (i in seq_along(list.scales)) {
        
        ## Time t1 vs t4
        
        # name for change column 
        col_name <- paste(list.scales[i], "change", "t1", "t4", sep = ".")
        
        dat.mini <- dat.mini |>
            mutate({{col_name}} := !!sym(paste0(list.scales[i], ".total.t4")) - !!sym(paste0(list.scales[i], ".total.t1")))
        
        # name for direction column
        direction_name <- paste(list.scales[i], "direction", "t1", "t4", sep = ".")
        
        dat.mini[[direction_name]] <- ifelse(
            dat.mini[[col_name]] < 0, "Decrease",
            ifelse(dat.mini[[col_name]] > 0, "Increase",
                   "No change")
        )
        
        colnames(dat.mini)[which(colnames(dat.mini) == direction_name)] <-
            direction_name
    }
dat.mini
}


# 4 time point scales
list.scales <- c("pss", "stai", "cesd", "pcl", "uls", "risc", "ptgi", "mhc")

dat.mini <- calculate.changes.4tmpts(list.scales)

# 2 time point scales
list.scales <- "sc"

dat.mini <- calculate.changes.2tmpts(list.scales)



## PASS VARIABLES ----

# Time point labels (used in MH tab text updating what time point is selected)
timepoint.label <- c(
    "baseline"          = "T1",
    "4-month follow-up" = "T2",
    "8-month follow-up" = "T3",
    "1-year follow-up"  = "T4") 

# Meditation labels (used in MH text updating what is plotted)
meditation.label <-  c(
    "years of meditation"           = "med.years",
    "days/week practice meditation" = "days.week.prac",
    "average minutes/sit"           = "total.mins.sit",
    "average minutes/week practice meditation" = "mins.week.prac")

scale.label <- c(
    "perceived stress"      = "pss",
    "anxiety"               = "stai",
    "depression"            = "cesd",
    "post-traumatic stress" = "pcl",
    "loneliness"            = "uls",
    "social connectedness"  = "sc",
    "resilience"            = "risc",
    "post-traumatic growth" = "ptgi",
    "flourishing"           = "mhc"
)


# pass 'score' as variable to use in plots (scale scores)
score.option <- "score"




### COLORS ----

# circle question mark icon
info.icon.color = "#cc0000"   # red color
    
# correlation matrices
heat.col <- colorRampPalette(c("#482576", "#287d8e","#FFFFFF", "#95d840","#fde725"))  


color.palette.tmpt <- c(
    "T1" = "#fde725", # yellow
    "T2" = "#35b779", # green
    "T3" = "#316883", # blue
    "T4" = "#472c7a" # purple
)

# color of difference histograms
histo.color <- '#277f8e'

mean.color <- '#ff9966'
    
