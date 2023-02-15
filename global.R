# global.R

# LIBRARIES ----

library(tidyverse)
library(shinythemes)
library(leaflet)      # for interactive maps

library(sjlabelled)   # add label to variable names
library(ggeasy)       # add label names to ggplot

library(plotly)       # interactive plots
library(viridis)      # color palette

library(psych)        # correlations with p values
library(ggcorrplot)   # correlation matrices
library(corrplot)     # view correlation matrices as heatmap
library(ppcor)        # partial correlations

library(shinyhelper)  # helper pop-ups
library(fontawesome)  # icons

library(conflicted)   # checks if have conflicting function names


# LOAD DATA ----
dat.long <- readRDS("./data/app_data_long.rds")
zipcode.dat <- readRDS("./data/zipcode_data.rds")
dat.mini <- readRDS("./data/app_mini_data.rds")
scale.descriptions <- readRDS("./data/app_scale_descriptions.rds")
scale.correlations.heatmap.dat <- load("./data/app_btwScaleCorrelations_data.RData")

# Add flag for when no demographic category is selected (for scatter plot), all
# individuals are shown
dat.long <- dat.long |>
    mutate(no.demo = "All")

# to test code:
# input <- list(demo.treemaps.var = "Gender", 
#               scale.type.var = "pss", 
#               meditation.var = "med.years", 
#               timepoint.var = "T1", 
#               scale.complete = "All", 
#               fit = TRUE, 
#               demographic.var = "gender")

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
correlation.heatmap <- function(corr.time, p.mat.time, ...) {
    corrplot(
        corr.time,
        method = "color",
        col = heat.col(200),
        addCoef.col = "black",
        tl.col = "black",
        tl.srt = 45,
        p.mat = p.mat.time,
        sig.level = 0.01,
        insig = "blank",
        diag = FALSE
    )
}



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



