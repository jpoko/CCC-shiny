
# UI ----

shinyUI(fluidPage(
    # add google analytics/tracking to shiny app
    tags$head(includeHTML(("google-analytics.html"))),
    
    theme = shinytheme("flatly"),
    shinyjs::useShinyjs(),

    # to add css styling
    #theme = "style.css",
    
    navbarPage(
        # Application title
        title = div(
            span("Contemplative Coping during Covid-19",
                 style = "position: relative; top: 50%; transform: translateY(-50%);")
        ), # close application title (NOTE: close parentheses added in UI for clarity)
        
        
        ## About tab ----
        tabPanel(
            "About", 
            
            # Add landing page text from md file
            includeMarkdown("./text_mds/landing_page.md"),
            
            # circled question mark icon with explanation
            tags$div(
                tags$i(class = "fa-regular fa-circle-question",
                       style = "color:#cc0000; font-size: 2rem;"),
                HTML("&nbsp;"),
                tags$span("If you see a red question mark icon like the one to 
                          the left, you can click on the icon for further 
                          information about options available to you or what is 
                          presented in the plot or table."
                ),
                br(),br()
            )
        ), # close About tab
        

        ## Demographics tab ----
        
        # Demographics tab with subtabs
        tabPanel(
            "Demographics",
            
            tabsetPanel(
                type = "pills",
                
                
                ### Interactive map subtab ----
                tabPanel(
                    HTML("Location<br/>of participants<br/>(map)"),
                    
                    br(),
                    leafletOutput("zipcodeMap") |>
                        shinyhelper::helper(
                            icon = "circle-question",
                            colour = info.icon.color,
                            type = "markdown",
                            content = "demographic_map_info"
                        ),
                    br()
                ), # close Map subtab
                
                
                
                ### Participant demographics subtab ----
                tabPanel(
                    HTML("Participant<br/>demographics"),

                    h3("General demographic information about participants"),
                    br(),
                    
                    #### Sidebar ---- 
                    # Demographic options to visualize
                    sidebarPanel(
                        radioButtons(
                            inputId = "demo.treemaps.var",
                            label = "Select a demographic characteristic",
                            choices = names(demographic.treemaps)
                        )
                    ),
                    
                    #### Main panel ---- 
                    # Visualization (treemap) of demographic category
                    mainPanel(
                        plotlyOutput("demographic.treemap.plots") |>
                            shinyhelper::helper(
                                icon = "circle-question",
                                colour = info.icon.color,
                                type = "markdown",
                                content = "demographic_treemap_info"
                            ),
                        br(),
                        
                        # Table of demographic category counts
                        DT::dataTableOutput("demographic.table") |>
                            shinyhelper::helper(
                                icon = "circle-question",
                                colour = info.icon.color,
                                type = "markdown",
                                content = "demographic_table_info"
                            ),
                        br()
                    )
                ),  # close Participant Demographics subtab
                
                ### Explain demographics subtab ----
                tabPanel(
                    HTML("Explanation<br/>of demographic<br/>variables"),
                    
                    h3("Information about demographic data"),
                    
                    includeMarkdown("./text_mds/demographics_descriptions/main_description_overview.md"),
                   
                    br(),
                    
                    #### Sidebar ---- 
                    # Demographic options to visualize
                    sidebarPanel(
                        radioButtons(
                            inputId = "demo_category_explain",
                            label = "Demographic variable",
                            choices = c("age", "gender", "ethnicity",
                                        "orientation", "education",
                                        "income")
                        )
                    ),
                    
                    #### Main panel ---- 
                    # Text explaining demographic category
                    mainPanel(
                        
                        tabsetPanel(
                            id = "explain.demo.tabs",
                            type = "pills",
                            
                            tabPanel(
                                "Description",
                                value = "descriptionTab",
                                uiOutput("demographic.category.description")
                            ),
                            tabPanel(
                                "Visualization",
                                value = "sankeyTab",
                                h4("Sankey diagrams"),
                                h5("Sankey diagrams showing the original responses
                                   from the participants (left) and the categories
                                   that we have used (right). The diagram is
                                   interactive - hover over parts of the diagram
                                   for more information."),
                                
                                h5("You can select the 'Stage' to see
                                   participant responses from different stages
                                   of recruitment, funneled through from applied
                                   to participate in the study to actually starting
                                   in the study. Click on the help icon for
                                   more detailed information on each of the
                                   participant recruitment stages"),
                                uiOutput("sankey.diagram.stage") |>
                                    shinyhelper::helper(
                                        icon = "circle-question",
                                        colour = info.icon.color,
                                        type = "markdown",
                                        content = "demo_explain_sankey_stage"
                                    ),
                                sankeyNetworkOutput("sankey.diagram.demographic.category",
                                                    height = "1200px") |>
                                    shinyhelper::helper(
                                        icon = "circle-question",
                                        colour = info.icon.color,
                                        type = "markdown",
                                        content = "demo_explain_sankey_general_info"
                                    )
                            )
                        )
                        
                    ) # close Explain demographics main panel
  
                ) # close Explain demographics subtab
                
                
            ), # close all Demographics subtabs
        ), # close Demographics tab
        
        
        
       
        
   
        
        ## Individual scales ----
        tabPanel(
            HTML("Individual<br/>scales"),
        
        ### Sidebar ----
        sidebarPanel(
            
            selectInput(
                inputId = "scale.type.var.single",
                label = "Well-being scale",
                choice = scale.label,
                selected = "pss"
            ) |>
                shinyhelper::helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "single_scale_var"
                ),
            
            pickerInput(
                inputId = "single_scale_view",
                label = "What to view",
                choices = list(
                    General = c("Description" = "description",
                                "Descriptives" = "descriptives",
                                "Item response distributions" = "item_response_distribution"),
                    Distribution = c("Raincloud plot" = "raincloud_plot",
                                     "Histogram" = "histogram_plot",
                                     "Estimated density plot" = "estimated_density_plot",
                                     "Box plot" = "box_plot",
                                     "Violin plot" = "violin_plot"),
                    `Over time` = c("Scores by time point" = "scores_by_time_point",
                                    "Scores by date" = "scores_by_date",
                                    "Scores by date and time point" = "scores_by_date_by_time_point"),
                    `Missing data` = c("Missing all items" = "missing_all_items",
                                       "Missing individual items" = "missing_individual_items",
                                       "Missing some or all items" = "missing_some_all_items")
                )
            ) |>
                shinyhelper::helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "single_scale_view")
            
        ), # close sidebarPanel of explore scales
        
        ### Main panel ----
        mainPanel(
            # conditional panels for different things to show 
            # about the selected scale
            
            #### Description ----
            conditionalPanel(
                condition = "input.single_scale_view=='description'",
                
                # show description of scale
                uiOutput("long.scale.description"),
            ),
            
            #### Descriptives ----
            conditionalPanel(
                condition = "input.single_scale_view=='descriptives'",
                h4("Descriptive stats of selected scale"),
                br(),
                # show descriptives table
                DT::dataTableOutput("descriptives.table") 
            ),
            
            #### Response distribution ----
            conditionalPanel(
                condition = "input.single_scale_view == 'item_response_distribution'",
                
                h4("Feature forthcoming")
            ),
            
            #### Raincloud plots ----
            conditionalPanel(
                condition = "input.single_scale_view == 'raincloud_plot'",
                h4("Raincloud plots"),
                
                plotlyOutput("single.scale.raincloudPlot",
                             height = 800) |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_raincloud_plot_info"
                    )
            ),
            
            #### Histogram ----
            conditionalPanel(
                condition = "input.single_scale_view == 'histogram_plot'",
                h4("Histogram - Frequency of scores"),
                sliderInput("histo.bin.size", "Select bin size:",
                            min = 1, max = 30,
                            value = 1),
                br(),
                plotlyOutput("single.scale.histogramPlot") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_histogram_info"
                    )
            ),
            
            #### Estimated density ----
            conditionalPanel(
                condition = "input.single_scale_view == 'estimated_density_plot'",
                br(),
                radioButtons(
                    inputId = "estimated.density.single.faceted",
                    label = "Plot as single plot or multiple (by time point)",
                    choices = c("Single", "Multiple"),
                    selected = "Single"),
                
                plotlyOutput("single.scale.estimateddensityPlot") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_estimated_density_info"
                    )
            ),
            
            #### Box plot ----
            conditionalPanel(
                condition = "input.single_scale_view == 'box_plot'",
                br(),
                plotlyOutput("single.scale.boxPlot") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_box_plot_info"
                    )
            ),
            
            #### Violin plot ----
            conditionalPanel(
                condition = "input.single_scale_view == 'violin_plot'",
                br(),
                plotlyOutput("single.scale.violinPlot") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_violin_plot_info"
                    )
            ),
            
            #### Scores by time point ----
            conditionalPanel(
                condition = "input.single_scale_view == 'scores_by_time_point'",
                
                h3("Change in scores between time points"),
                h5("Top: Corset plots showing individual change in scores between 
                   the 2 time points selected, faceted by scores that decreased, 
                   increased, or did not change."),
                h5("Bottom: Histogram of the distribution of change in scores 
                   between the 2 time points selected."),
                br(),
                
                uiOutput("corset.tmpt.compare.options"),
                
                br(),
                h4("Corset plots"),
                plotOutput("single.scale.corsetPlot") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_corset_plot_info"
                    ),
                
                br(),
                h4("Histogram of distribution of change"),
                plotlyOutput("single.scale.corset.change.histoPlot") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_corset_plot_histogram_info"),
                br()
            ),
            
            
            #### Scores by date ----
            conditionalPanel(
                condition = "input.single_scale_view == 'scores_by_date'",
                h4("Scores by date"),
                h5("Line plot of scores by month. Size of circle indicates the number of people with scores in that month contributing to the average/median score plotted. Option to overlay weekly U.S. Covid-19 cases or deaths."),
                
                radioButtons(
                    inputId = "scores.by.date.type",
                    label = "What should be plotted?",
                    choices = c(
                        "average scores" = "avg.score",
                        "median scores" = "median.score"),
                    selected = "avg.score"),
                
                radioButtons(
                    inputId = "covid_cases_deaths",
                    label = "Add average weekly Covid-19 cases or deaths?",
                    choices = c(
                        "none" = "none", 
                        "cases" = "weekly.cases", 
                        "deaths" = "weekly.deaths"),
                    selected = "none"
                ),
                
                highchartOutput("hc_single_scale_date_linePlot", height = "500px") |>
                    helper(
                        icon = "circle-question",
                        colour = info.icon.color,
                        type = "markdown",
                        content = "single_scale_scores_by_date_info"),
                
                br(), br(),  
            ),
            
            
            
            #### Scores by date by time point ----
            conditionalPanel(
                condition = "input.single_scale_view == 'scores_by_date_by_time_point'",
                
                h4("Scores by date by time point"),
                h5("Line plot of scores by month by time point. Size of dot 
                   indicates the number of people with scores in that month 
                   contributing to the average/median score plotted. Color 
                   indicates the study time point."),
                
               radioButtons(
                   inputId = "scores.by.date.by.timepoint.type",
                   label = "What should be plotted?",
                   choices = c(
                       "average scores" = "avg.score",
                       "median scores" = "median.score"),
                   selected = "avg.score"),
               
               radioButtons(
                   inputId = "covid_cases_deaths_tmpt",
                   label = "Add average weekly Covid-19 cases or deaths?",
                   choices = c(
                       "none" = "none", 
                       "cases" = "weekly.cases", 
                       "deaths" = "weekly.deaths"),
                   selected = "none"
               ),
               
               highchartOutput("hc_single_scale_date_tmpt_linePlot", height = "500px") |>
                   helper(
                       icon = "circle-question",
                       colour = info.icon.color,
                       type = "markdown",
                       content = "single_scale_scores_by_date_by_timepoint_info"),
               
               br(), br()
            ),
            
            #### Missing all items ----
            conditionalPanel(
                condition = "input.single_scale_view == 'missing_all_items'",
                
                h4("Feature forthcoming")
                
            ),
            
            #### Missing individual items ----
            conditionalPanel(
                condition = "input.single_scale_view == 'missing_individual_items'",
                
                h4("Feature forthcoming")
                
            ),
            
            #### Missing some/all items ----
            conditionalPanel(
                condition = "input.single_scale_view == 'missing_some_all_items'",
                
                h4("Feature forthcoming")
                
            ),
        )# close main panel
    ), # close individual scales tab
    
    ## Correlation matrices ----
    
    tabPanel(
        HTML("Correlation<br/>matrices"),
        
        h4("Correlation matrices of selected variable measures 
                            at the 4 data collection time points."
        ) |> helper(
            icon = "circle-question",
            colour = info.icon.color,
            type = "markdown",
            content = "correlation_matrices_info"
        ),
        
        br(),
        
        ### Options ----
        fluidRow(
            column(
                4,
                radioButtons(
                    inputId = "corr.matrix.type",
                    label = "Select correlation matrix type",
                    choices = c("well-being scales", "meditation variables"),
                    selected = "well-being scales"
                ) |> helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "correlation_matrices_type"
                ),
            ),
            column(
                4,
                sliderInput(
                    inputId = "corr.matrix.sig.p.val",
                    label = "Select significance level",
                    min = 0,
                    max = 0.1,
                    value = 0.05
                ) |> helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "correlation_matrices_sig_level"
                ),
            )
        ),
        
        ### Visualizations ----
        fluidRow(column(
            6,
            h3(HTML("<b>Baseline</b>"),
               style = "text-align:center"),
            plotOutput("correlation.heatmap.t1")
        ),
        column(
            6,
            h3(HTML("<b>4-month follow-up</b>"),
               style = "text-align:center"),
            plotOutput("correlation.heatmap.t2")
        )),
        fluidRow(column(
            6,
            h3(HTML("<b>8-month follow-up</b>"),
               style = "text-align:center"),
            plotOutput("correlation.heatmap.t3")
        ),
        column(
            6,
            h3(HTML("<b>1-year follow-up</b>"),
               style = "text-align:center"),
            plotOutput("correlation.heatmap.t4")
        ))
    ), # close correlation matrices tab
                 
        
    
    ## Meditation & well-being tab ----
    
    tabPanel(
        HTML("Meditation &<br/>well-being"),
        
        h2(
            "Relationships between meditation experience and
                  measures of well-being"
        ),
        br(),
        
        #### Sidebar ----
        # Sidebar panel for input/controls
        sidebarPanel(
            h3("Options:"),
            
            # Input: well-being scale
            selectInput(
                inputId = "scale.type.var",
                label = "Well-being variable",
                choice = scale.label,
                selected = "pss"
            ) |>
                shinyhelper::helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_scale_var_y"
                ),
            
            materialSwitch(
                inputId = "hide.show.scale.descriptions",
                label = "Show/hide scale description",
                value = FALSE,
                status = "success"
            ),
            
            # Input: meditation variable
            selectInput(
                inputId = "meditation.var",
                label = "Meditation variable",
                choices = c(
                    "years of meditation" = "med.years",
                    "days/week practice" = "days.week.prac",
                    "minutes/sit" = "total.mins.sit",
                    "minutes/week practice" = "mins.week.prac"
                ),
                selected = "med.years"
            ) |>
                shinyhelper::helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_meditation_var_x"
                ),
            
            materialSwitch(
                inputId = "hide.show.meditation.descriptions",
                label = "Show/hide meditation description",
                value = FALSE,
                status = "success"
            ),
            
            # Input: time point
            radioButtons(
                inputId = "timepoint.var",
                label = "Time point",
                choices = timepoint.label,
                selected = "T1"
            ) |>
                shinyhelper::helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_timepoint"
                ),
            
            # Input: demographic category
            radioButtons(
                inputId = "demographic.var",
                label = "Demographic characteristic",
                choices = c(
                    "None" = "no.demo",
                    "Age" = "age.bin",
                    "Gender" = "gender",
                    "Orientation" = "orientation",
                    "Ethnicity" = "ethnicity",
                    "Income" = "income",
                    "Education" = "education"
                ),
                selected = "no.demo"
            ) |>
                shinyhelper::helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_demographic_option"
                ),
            
            # Input: all or complete participants
            radioButtons(
                inputId = "scale.complete",
                label = "Participants",
                choices = c("All", "Complete"),
                selected = "All"
            ) |>
                helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_participants_complete_all"
                ),
            
            # Input: x & y marginal plot types
            
            radioButtons(
                inputId = "marginal.x.plot.type",
                label = "X-axis marginal plot type",
                choices = c("violin", "box", "histogram"),
                selected = "box"
            ) |> helper(
                icon = "circle-question",
                colour = info.icon.color,
                type = "markdown",
                content = "med_wellbeing_scatterplot_marginal_x_plot_type"
            ),
            
            radioButtons(
                inputId = "marginal.y.plot.type",
                label = "Y-axis marginal plot type",
                choices = c("violin", "box", "histogram"),
                selected = "box"
            ) |> helper(
                icon = "circle-question",
                colour = info.icon.color,
                type = "markdown",
                content = "med_wellbeing_scatterplot_marginal_y_plot_type"
            ),
            
            # Input: regression line or no
            checkboxInput(
                inputId = "fit",
                "Add best fit (regression) line to scatterplot",
                value = FALSE
            ) |>
                helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_regression_line"
                ),
            
            # width of sidebar
            width = 3
        ),
        # close sidebar panel input options
        
        #### Main panel ----
        # Main panel with scatter plots
        mainPanel(
            # heading and scale description text
            h3(textOutput("caption")),
            br(),
            
            # main scatter plot
            plotlyOutput("scatPlot") |>
                helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_scatterplot_main_plot_info"
                ),
            br(),
            
            # show description of scale - toggle
            uiOutput("lay.scale.description"),
            
            # show description of meditation - toggle
            uiOutput("meditation.var.description"),
            
            hr(),
            h4("Number of participants"),
            # counts of all and complete participants
            h5(textOutput("counts.scatterplots.all")),
            h5(textOutput("counts.scatterplots.complete")),
            hr(),
            
            # correlation table text
            h4(textOutput("correlation.table.txt")),
            
            # correlation table: scale vs meditation var
            DT::dataTableOutput("dt.correlation.table") |>
                helper(
                    icon = "circle-question",
                    colour = info.icon.color,
                    type = "markdown",
                    content = "med_wellbeing_correlations_scale_meditation_table_info"
                ),
            br(),
            
            # heading for faceted scatter plots
            h4(textOutput("caption.facets")),
            
            # faceted scatter plots
            plotOutput("scatfacetPlot")
        ) # close well-being & meditation main
    ), # close well-being & meditation tab
    
    
        ## Credits tab ----
        
        tabPanel("Credits",
                 
                 # Credits page text from md file
                 includeMarkdown("./text_mds/credits_page.md")
        ) # close Credits tab
        
    ) # close main nav bar tabs
) # close fluid page
) # close ui
