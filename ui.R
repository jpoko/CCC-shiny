
# UI ----


shinyUI(fluidPage(
    # add google analytics/tracking to shiny app
    tags$head(includeHTML(("google-analytics.html"))),
    
    theme = shinytheme("flatly"),

    # to add css styling, include next line and update css file
    #theme = "style.css",
    
    navbarPage(
        # Application title
        title = div(
            span("Contemplative Coping during Covid-19",
                 style = "position: relative; top: 50%; transform: translateY(-50%);")
        ),
        # close application title
        
        
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
                )
            )
        ),
        # close About tab
        
        
        
        
        ## Demographics tab ----
        
        # Demographics tab with subtabs
        tabPanel(
            "Demographics",
            
            tabsetPanel(
                type = "tabs",
                
                
                ### Interactive map subtab ----
                tabPanel(
                    "Map",
                    
                    br(),
                    leafletOutput("zipcodeMap") |>
                        shinyhelper::helper(
                            icon = "circle-question",
                            colour = info.icon.color,
                            type = "markdown",
                            content = "map_info"
                        ),
                    br()
                ),
                # close Map subtab
                
                
                
                ### General demographics subtab ----
                tabPanel(
                    "General demographics",
                    
                    h3("General demographic information about participants"),
                    br(),
                    
                    # Demographic options to visualize
                    sidebarPanel(
                        radioButtons(
                            inputId = "demo.treemaps.var",
                            label = "Select a demographic characteristic",
                            choices = names(demographic.treemaps)
                        )
                    ),
                    
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
                )  # close General Demographics subtab
            ) # close all Demographics subtabs
        ), # close Demographics tab
        
        
        
        ## Well-being tab ----
        
        # Well-being tab with subtabs
        tabPanel("Well-being",
                 
                 # separate tabs
                 tabsetPanel(
                     type = "tabs",
                     
                     
                     ### Mental health & meditation subtab ----
                     tabPanel(
                         "Meditation & well-being",
                         
                         h2("Relationships between meditation experience and 
                            measures of well-being"),
                         br(),
                         
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
                                     content = "scatterplot_yvar"
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
                                     content = "scatterplot_xvar"
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
                                     content = "scatterplot_timepoint"
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
                                     content = "scatterplot_color_demographic"
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
                                     content = "scatterplot_participants_complete_all"
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
                                 content = "scatterplot_marginal_x_plot_type"
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
                                 content = "scatterplot_marginal_y_plot_type"
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
                                     content = "scatterplot_regression_line"
                                 ),
                             
                             # width of sidebar
                             width = 3
                         ), # close sidebar panel input options
                         
                         # Main panel with scatter plots
                         mainPanel(
                             
                             # heading and scale description text
                             h3(textOutput("caption")),
                             htmlOutput("scale.description"),
                             br(),
                             
                             # main scatter plot
                             plotlyOutput("scatPlot") |>
                                 helper(
                                     icon = "circle-question",
                                     colour = info.icon.color,
                                     type = "markdown",
                                     content = "scatterplot_main_plot_info"
                                 ),
                             
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
                                     content = "correlations_scale_meditation_table_info"
                                 ),
                             
                             br(),br(),br(),
                             # heading for faceted scatter plots
                             h4(textOutput("caption.facets")),
                             
                             # faceted scatter plots
                             plotOutput("scatfacetPlot")
                         ) # close well-being & meditation main
                     ), # close well-being & meditation subtab
                     
                     
                     
                     ### Correlation matrices ----
                     
                     tabPanel(
                         "Correlation matrices",
                         
                         # h2("Correlation matrices"),
                         
                         h4("Correlation matrices of selected variable measures 
                            at the 4 data collection time points."
                         ) |> helper(
                             icon = "circle-question",
                             colour = info.icon.color,
                             type = "markdown",
                             content = "correlation_matrices_info"
                         ),
                         
                         br(),
                         
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
                                     content = "correlation_matrix_type"
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
                                     content = "correlation_sig_level"
                                 ),
                             )
                         ),
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
                     ) # close correlation matrices subtab
                 ) # close all well-being subtabs
        ), # close well-being tab
        
        
        ## Credits tab ----
        
        tabPanel("Credits",
                 
                 # Credits page text from md file
                 includeMarkdown("./text_mds/credits_page.md")
        ) # close Credits tab
        
    ) # close main nav bar tabs
) # close fluid page
) # close ui
