
# SERVER ----

shinyServer(function(input, output) {
    
    # location of md files for helper pop-ups
    observe_helpers(help_dir = "helpfiles_mds")
    
    ## Demographic info ----
    
    ### Tree maps ----
    output$demographic.treemap.plots <- renderPlotly({
        plotly::subplot(
            demographic.treemaps[input$demo.treemaps.var],
            nrows = 1,
            shareX = TRUE,
            shareY = TRUE
        )
    })
    
    
    ### Table of demographic counts & percentages ----
    
    output$demographic.table <- DT::renderDataTable({
        dat.mini |>
            dplyr::group_by_at(input$demo.treemaps.var) |>
            dplyr::summarize(Count = n()) |>
            dplyr::mutate(Percent = Count/sum(Count),
                          Percent = round(Percent * 100, 2))
    }, options = list(paging = FALSE, dom = "t"), rownames = FALSE)
    
    
    
    ### Zip code map ----
    
    output$zipcodeMap <- renderLeaflet({
        leaflet(zipcode.dat) |>
            addTiles() |>
            setView(-93.65, 42.0285, zoom = 3) |>
            addMarkers(clusterOptions = markerClusterOptions())
    })
    
    
    
    ### Explain demographics ----
    
    #### Demographic category descriptions----
    demographic_description_file <- reactive({
        switch(input$demo_category_explain,
               "age" = "./text_mds/demographics_descriptions/age_description.md",
               "gender" = "./text_mds/demographics_descriptions/gender_description.md",
               "ethnicity" = "./text_mds/demographics_descriptions/ethnicity_description.md",
               "orientation" = "./text_mds/demographics_descriptions/orientation_description.md",
               "education" = "./text_mds/demographics_descriptions/education_description.md",
               "income" = "./text_mds/demographics_descriptions/income_description.md")
    })
    
    output$demographic.category.description <- renderUI({
        includeMarkdown(demographic_description_file())
    })
    
    
    
    #### Show sankey diagram tab selectively ----
    
    # show 'visualization' tab (that has sankey diagram) if select category
    # has a sankey diagram (i.e., gender, ethnicity, orientation)
    
    observe({
        req(input$demo_category_explain)
        
        if (input$demo_category_explain %in% c("gender", "ethnicity", "orientation")) {
            showTab(inputId = "explain.demo.tabs", target = "sankeyTab")
        } else {
            hideTab(inputId = "explain.demo.tabs", target = "sankeyTab")
        }
    })
    
    
    
    #### Sankey plots ----
   
    output$sankey.diagram.stage <- renderUI({

        if (input$demo_category_explain %in% c("ethnicity", "gender", "orientation")) {
            radioButtons(
                inputId = "sankey.diagram.stage",
                label = "Stage",
                choices = c("applied", "screened",
                            "admitted", "consented",
                            "started"),
                selected = "applied")
        } else {
            br()
        }
    })
    
    
    
    
    output$sankey.diagram.demographic.category <- networkD3::renderSankeyNetwork({
      
        req(input$demo_category_explain)
        
        if (input$demo_category_explain == "ethnicity") {
            get_sankey_data <- nested_ethnicity_levels
        } else if (input$demo_category_explain == "gender") {
            get_sankey_data <- nested_gender_levels
        } else if (input$demo_category_explain == "orientation") {
            get_sankey_data <- nested_orientation_levels
        } 
            
        
        # get index of which stage to examine
        index_data <- which(sapply(get_sankey_data,
                                   function(x) x$name == input$sankey.diagram.stage))
        
        # get that data (selected demo category and selected stage)
        sankey_data <- get_sankey_data[[index_data]][["data"]]
        
        # rename columns
        colnames(sankey_data) <- c("source", "target", "value")
        
        nodes <- data.frame(
            name = c(
                as.character(sankey_data$source), 
                as.character(sankey_data$target)) %>% 
                unique())
        
        sankey_data$IDsource = match(sankey_data$source, nodes$name) - 1
        
        sankey_data$IDtarget = match(sankey_data$target, nodes$name) - 1
        
        networkD3::sankeyNetwork(Links = sankey_data,
                                 Nodes = nodes,
                                 Source = "IDsource",
                                 Target = "IDtarget",
                                 Value = "value",
                                 NodeID = "name",
                                 units = "case(s)",
                                 fontSize = 14,
                                 nodeWidth = 30)
    })
    
    
 
    ## Well-being plots & info ----
    
    
    ### Update scatter plots' heading text ----
    
    # formula for the text - scatter plot heading
    txt.scatterplot.heading <- reactive({
        paste(
            "Relationship between ",
            names(scale.label)[scale.label == input$scale.type.var],
            " and ",
            names(meditation.label)[meditation.label == input$meditation.var],
            " at the ",
            names(timepoint.label)[timepoint.label == input$timepoint.var],
            " time point."
        )
    })
    
    # return formula text for printing as a caption
    output$caption <- renderText({
        txt.scatterplot.heading()
    })
    
    
    # scatter plot heading of faceted plots
    txt.scatterplot.faceted <- reactive({
        paste(
            "Relationship between ",
            names(scale.label)[scale.label == input$scale.type.var],
            " and ",
            names(meditation.label)[meditation.label == input$meditation.var],
            " at each time point"
        )
    })
    
    # return formula text for printing as a caption
    output$caption.facets <- renderText({
        txt.scatterplot.faceted()
    })
    
    
    
    ### Data for counts of n and correlations ----
    
    # Filter data for selected scale at selected time point, for all participants
    # (1), or complete participants (2), and only complete cases
    
    selected.dat.all.participants <- reactive({
        dat.long |>
            dplyr::filter(scale %in% input$scale.type.var) |>
            dplyr::filter(time.pt %in% input$timepoint.var) |>
            dplyr::select(score, input$meditation.var, age, scale, time.pt) %>%
            dplyr::filter(complete.cases(.))
    })
    
    selected.dat.complete.participants <- reactive({
        dat.all <- dat.long |>
            dplyr::filter(scale %in% input$scale.type.var) |>
            dplyr::filter(time.pt %in% input$timepoint.var)
        
        name.match <-
            paste0("^", input$scale.type.var, ".complete", sep = "")
        name.match.col <- grep(name.match, colnames(dat.all))
        
        dat.all |>
            dplyr::filter(dat.all[name.match.col] == 1) |>
            dplyr::select(score, input$meditation.var, age, scale, time.pt) %>%
            dplyr::filter(complete.cases(.))
    })
    
    
    
    
    
    
    ### Counts for n in scatter plots ----
    
    # Extract count of all participants of selected survey at selected time point
    count.all.n <- reactive({
        count.all.n <- nrow(selected.dat.all.participants())
        paste0("All participants: n = ", count.all.n)
    })
    
    output$counts.scatterplots.all <- renderText({
        count.all.n()
    })
    
    
    # Extract count of participants who completed selected survey at all time points
    count.complete.n <- reactive({
        dat.all.complete <- nrow(selected.dat.complete.participants())
        paste0("Complete participants: n = ", dat.all.complete)
    })
    
    output$counts.scatterplots.complete <- renderText({
        count.complete.n()
    })
    
    
    
    ### Data for scatter plots ----
    
    # Get all data for selected scale and 'complete' or 'all' participants 
    # (selected), data from all time points
    scatter.data.all.tmpts <- reactive({
        selected.scatter.dat <- dat.long |>
            dplyr::filter(scale %in% input$scale.type.var)
        
        if (input$scale.complete == "Complete") {
            name.match <-
                paste0("^", input$scale.type.var, ".complete", sep = "")
            name.match.col <-
                grep(name.match, colnames(selected.scatter.dat))
            selected.scatter.dat <- selected.scatter.dat |>
                dplyr::filter(selected.scatter.dat[name.match.col] == 1)
        } else {
            name.match <-
                paste0("^", input$scale.type.var, ".complete", sep = "")
            name.match.col <-
                grep(name.match, colnames(selected.scatter.dat))
            selected.scatter.dat <- selected.scatter.dat |>
                dplyr::filter(selected.scatter.dat[name.match.col] == 0 |
                                  selected.scatter.dat[name.match.col] == 1)
        }
    })
    
    # Get data from single selected time point for selected scale and 'complete' 
    # or 'all' participants (selected)
    scatter.data.one.tmpt <- reactive({
        selected.scatter.dat <- dat.long |>
            dplyr::filter(scale %in% input$scale.type.var) |>
            dplyr::filter(time.pt %in% input$timepoint.var)
        
        if (input$scale.complete == "Complete") {
            name.match <-
                paste0("^", input$scale.type.var, ".complete", sep = "")
            name.match.col <-
                grep(name.match, colnames(selected.scatter.dat))
            selected.scatter.dat <- selected.scatter.dat |>
                dplyr::filter(selected.scatter.dat[name.match.col] == 1)
        } else {
            name.match <-
                paste0("^", input$scale.type.var, ".complete", sep = "")
            name.match.col <-
                grep(name.match, colnames(selected.scatter.dat))
            selected.scatter.dat <- selected.scatter.dat |>
                dplyr::filter(selected.scatter.dat[name.match.col] == 0 |
                                  selected.scatter.dat[name.match.col] == 1)
        }
    })
    
    

    
 
    ### Main scatter plot & marginal distribution plots ----
    
    # Filter data for scatter plot so only includes complete cases
    scatter.data <- reactive({
        scatter.data.one.tmpt() |>
        dplyr::select(ccc.id:age.bin, input$meditation.var, scale, time.pt, score) %>%
        dplyr::filter(complete.cases(.))
    })
    
    
    # Set up x-axis marginal plot - violin, box, histogram
    # NOTE: violin plot requires additional spanmode setting, so is separate
    marginal.x.plot <- reactive({
       
        if (input$marginal.x.plot.type == "violin") {
            plot_ly(data = scatter.data(), 
                    x = ~get(input$meditation.var), 
                    type = 'violin', 
                    alpha = .5, 
                    color = ~get(input$demographic.var), 
                    colors = "viridis", 
                    showlegend = FALSE,
                    spanmode = "hard")
        } else {
            plot_ly(data = scatter.data(), 
                    x = ~get(input$meditation.var), 
                    type = input$marginal.x.plot.type, 
                    alpha = .5, 
                    color = ~get(input$demographic.var), 
                    colors = "viridis", 
                    showlegend = FALSE)
        }
    })
    
    # Set up y-axis marginal plot - violin, box, histogram
    # NOTE: violin plot requires additional spanmode setting, so is separate
    marginal.y.plot <- reactive({ 
        if (input$marginal.y.plot.type == "violin") {
            
            plot_ly(data = scatter.data(), 
                    y = ~get(score.option), 
                    type = 'violin', 
                    alpha = .5, 
                    color = ~get(input$demographic.var), 
                    colors = "viridis", 
                    showlegend = FALSE,
                    spanmode = "hard")
        } else {
            plot_ly(data = scatter.data(), 
                    y = ~get(score.option), 
                    type = input$marginal.y.plot.type, 
                    alpha = .5, 
                    color = ~get(input$demographic.var), 
                    colors = "viridis", 
                    showlegend = FALSE)
        }
    })
    
    # Set up main scatter plot
    marginal.scatter.plot <- reactive({
        
        # Add regression line if option is selected
        if (input$fit) {

            # Resources:
            # https://rpubs.com/cfong32/Plotly-Tutorial (search: regression)
            # https://stackoverflow.com/questions/53836414/r-plotly-plotting-multiple-regression-lines
            # how to pass variables: https://stackoverflow.com/questions/18762962/passing-variable-names-to-model-in-shiny-app
            
            # Meditation variable as predictor
            model.predictors <- paste(input$meditation.var, collapse = "+")

            # Determine model formula depending on whether demographic category
            # is selected or not (if demographic category is selected, separate
            # regression lines are calculated for each demographic category group)
            # Formula: response ~ predictor / response ~ predictor * predictor 2
            # score ~ meditation variable / score ~ meditation variable * demographic
            if (input$demographic.var == "no.demo") {
                model.formula <- as.formula(sprintf('%s ~ %s', 
                                                    score.option, 
                                                    model.predictors))
            } else {
                model.category <- paste(input$demographic.var, collapse = "+")
                model.formula <- as.formula(sprintf('%s ~ %s*%s', 
                                                    score.option, 
                                                    model.predictors, 
                                                    model.category))
            }

            # Run regression
            model.fit <- lm(model.formula, data = scatter.data())
            
            # Scatter plot of selected meditation variable (x-axis) and 
            # score on selected well-being scale (y-axis), with regression
            # line(s)
            plot_ly(data = scatter.data(), 
                    x = ~get(input$meditation.var), 
                    y = ~get(score.option), 
                    type = 'scatter', 
                    color = ~get(input$demographic.var), 
                    colors = "viridis", 
                    mode = "markers") %>%
                add_trace(
                    data = scatter.data(),
                    x = ~get(input$meditation.var),
                    y = model.fit$fitted.values,
                    mode = "lines",
                    showlegend = FALSE)
        } else {
            # Scatter plot of selected meditation variable (x-axis) and 
            # score on selected well-being scale (y-axis)
            plot_ly(data = scatter.data(), 
                    x = ~get(input$meditation.var), 
                    y = ~get(score.option), 
                    type = 'scatter', 
                    color = ~get(input$demographic.var), 
                    colors = "viridis", 
                    mode = "markers")
        }
    })
    
    
    # Layout main scatter plot with marginal distribution plots
    output$scatPlot <- renderPlotly({
        marg_plot <- plotly::subplot(marginal.x.plot(), 
                             plotly_empty(), 
                             marginal.scatter.plot(), 
                             marginal.y.plot(),
                             nrows = 2, 
                             heights = c(.2, .8), widths = c(.8,.2), 
                             margin = 0,
                             shareX = TRUE, shareY = TRUE, 
                             titleY = TRUE)
        
        plotly::layout(marg_plot, 
                       barmode = 'overlay', 
                       yaxis2 = list(title = as.character(get_label(
                           scatter.data.one.tmpt()[score.option]))), 
                       xaxis = list(title = as.character(get_label(
                           scatter.data.one.tmpt()[input$meditation.var]))))
    })
    
    
    
    
    
    ### Faceted scatter plots ----
    
    output$scatfacetPlot <- renderPlot({
        facet.plot <- scatter.data.all.tmpts() |>
            
            ggplot(
                aes_string(
                    x = input$meditation.var,
                    y = score.option,
                    color = input$demographic.var)) +
            
            geom_jitter(size = 2) +
            
            # FORMATTING
            theme_bw() +
            scale_color_viridis_d() +
            
            theme(
                legend.position = "none",
                strip.background = element_rect(
                    fill = "gray20",
                    color = "black",
                    linewidth = 1
                ),
                strip.text = element_text(
                    size = 14, color = "white", face = "bold")
            ) +
            
            # Add labels to plot
            easy_labs() +
            
            # Facet by time point
            facet_wrap( ~ time.pt,
                        nrow = 1)
        
        # Add regression line(s) if option is selected 
        if (input$fit) {
            facet.plot <- facet.plot + 
                geom_smooth(
                    alpha = 0.3,
                    method = "lm",
                    se = FALSE,
                    fill = NA
                )
            }
        facet.plot
    })
    
    
    
    ### Correlations ----
    
    # Calculate correlations and make table
    calculate.correlations <- reactive({
        
        # Partial correlation all participants
        partial.corr.all <- pcor.test(selected.dat.all.participants()[, 1],
                                      selected.dat.all.participants()[, 2],
                                      selected.dat.all.participants()[, 3],
                                      method = "spearman")
        
        # Correlation all participants
        orig.corr.all <-
            corr.test(selected.dat.all.participants()[, 1], 
                      selected.dat.all.participants()[, 2], 
                      method = "spearman")
        
        # Partial correlation complete participants
        partial.corr.complete <- pcor.test(selected.dat.complete.participants()[, 1],
                                           selected.dat.complete.participants()[, 2],
                                           selected.dat.complete.participants()[, 3],
                                           method = "spearman")
        
        # Correlation of complete participants
        orig.corr.complete <-
            corr.test(selected.dat.complete.participants()[, 1], 
                      selected.dat.complete.participants()[, 2], 
                      method = "spearman")

        # Make table of correlation results (type, estimate, p, n)
        correlation.table <- data.frame(
            Correlation = c(
                "All participants",
                "All participants controlling for age",
                "Complete participants",
                "Complete participants controlling for age"
            ),
            Estimate = c(
                orig.corr.all$r,
                partial.corr.all$estimate,
                orig.corr.complete$r,
                partial.corr.complete$estimate
            ),
            p = c(
                orig.corr.all$p,
                partial.corr.all$p.value,
                orig.corr.complete$p,
                partial.corr.complete$p.value
            ),
            n = c(
                orig.corr.all$n,
                partial.corr.all$n,
                orig.corr.complete$n,
                partial.corr.complete$n
            )
        )
        
        # Round estimate and p values
        correlation.table <- correlation.table |>
            mutate(Estimate = round(Estimate, 3),
                   p = round(p, 3))
        
        # Return correlation table
        correlation.table
    })
    
    
    # Correlation table  
    output$dt.correlation.table <- DT::renderDataTable(
        calculate.correlations(),
        options = list(paging = FALSE, dom = "t"),
        rownames = FALSE
    )
    
    # Correlation table heading
    correlation.table.heading <- reactive({
        paste(
            "Spearman correlations between ",
            names(scale.label)[scale.label == input$scale.type.var],
            " and ",
            names(meditation.label)[meditation.label == input$meditation.var],
            " at the ",
            names(timepoint.label)[timepoint.label == input$timepoint.var],
            " time point."
        )
    })
    
    output$correlation.table.txt <- renderText({
        correlation.table.heading()
    })
    
    
    
    
    
    ### Scale descriptions ----
    
    ### Short scale descriptions----
    scale_lay_description_file <- reactive({
        switch(input$scale.type.var,
               "pss" = "./text_mds/scales_lay_descriptions/pss_lay.md",
               "stai" = "./text_mds/scales_lay_descriptions/stai_lay.md",
               "cesd" = "./text_mds/scales_lay_descriptions/cesd_lay.md",
               "pcl" = "./text_mds/scales_lay_descriptions/pcl_lay.md",
               "uls" = "./text_mds/scales_lay_descriptions/uls_lay.md",
               "sc" = "./text_mds/scales_lay_descriptions/sc_lay.md",
               "risc" = "./text_mds/scales_lay_descriptions/risc_lay.md",
               "ptgi" = "./text_mds/scales_lay_descriptions/ptgi_lay.md",
               "mhc" = "./text_mds/scales_lay_descriptions/mhc_lay.md")
    })
    
    output$lay.scale.description <- renderUI({
        includeMarkdown(scale_lay_description_file())
    })
  
     observeEvent(input$hide.show.scale.descriptions, {
         toggle("lay.scale.description")
     })
    
     ### Meditation duration descriptions ----
     
     ### Short meditation variable descriptions----
     meditation_vars_description_file <- reactive({
         switch(input$meditation.var,
                "med.years" = "./text_mds/meditation_vars_descriptions/years_meditation_description.md",
                "days.week.prac" = "./text_mds/meditation_vars_descriptions/days_week_practice_description.md",
                "total.mins.sit" = "./text_mds/meditation_vars_descriptions/total_mins_practice_description.md",
                "mins.week.prac" = "./text_mds/meditation_vars_descriptions/mins_week_practice_description.md")
     })
     
     output$meditation.var.description <- renderUI({
         includeMarkdown(meditation_vars_description_file())
     })
     
     observeEvent(input$hide.show.meditation.descriptions, {
         toggle("meditation.var.description")
     })
     
    
    ## Correlation matrices ----
    
    # get significance level input
    significant.p.value <- reactive({
        as.numeric(input$corr.matrix.sig.p.val)
    })
    
    input_corr <- reactive({
        input$corr.matrix.type
    })
    
    output$correlation.heatmap.t1 <- renderPlot({
        tmpt <- "t1"
        input_name <- paste(input_corr(), tmpt)
        idx_corr <- which(sapply(nested_corr, function(x) x$name == input_name))
        idx_p <- which(sapply(nested_p, function(x) x$name == input_name))
        
        corr <- nested_corr[[idx_corr]][["data"]]
        p.mat <- nested_p[[idx_p]][["data"]]
        
        correlation.heatmap(corr, p.mat, significant.p.value())
    })
    
    output$correlation.heatmap.t2 <- renderPlot({
        tmpt <- "t2"
        input_name <- paste(input_corr(), tmpt)
        idx_corr <- which(sapply(nested_corr, function(x) x$name == input_name))
        idx_p <- which(sapply(nested_p, function(x) x$name == input_name))
        
        corr <- nested_corr[[idx_corr]][["data"]]
        p.mat <- nested_p[[idx_p]][["data"]]
        
        correlation.heatmap(corr, p.mat, significant.p.value())
    })
    
    output$correlation.heatmap.t3 <- renderPlot({
        tmpt <- "t3"
        input_name <- paste(input_corr(), tmpt)
        idx_corr <- which(sapply(nested_corr, function(x) x$name == input_name))
        idx_p <- which(sapply(nested_p, function(x) x$name == input_name))
        
        corr <- nested_corr[[idx_corr]][["data"]]
        p.mat <- nested_p[[idx_p]][["data"]]
        
        correlation.heatmap(corr, p.mat, significant.p.value())
    })
    
    output$correlation.heatmap.t4 <- renderPlot({
        tmpt <- "t4"
        input_name <- paste(input_corr(), tmpt)
        idx_corr <- which(sapply(nested_corr, function(x) x$name == input_name))
        idx_p <- which(sapply(nested_p, function(x) x$name == input_name))
        
        corr <- nested_corr[[idx_corr]][["data"]]
        p.mat <- nested_p[[idx_p]][["data"]]
        
        correlation.heatmap(corr, p.mat, significant.p.value())
    })
    
 
    
    ## Individual scales ----
    
    ### General ----
    
    # get the index of the total columns for the scale selected
    index_individual_scale_selected_explore <- reactive({
        col.interest <- paste("^", input$scale.type.var.single, ".total.", sep = "")
        total.cols.interest <- grep(col.interest, colnames(dat.mini))
        total.cols.interest
    })
    
    data_individual_scale_selected_explore <- reactive({
        selected.dat <- dat.long |>
            dplyr::filter(scale %in% input$scale.type.var.single) 
        selected.dat
    })
    
    # get abbreviation of scale to use in plots
    abbreviation_individual_scale_selected_explore <- reactive({
        abbreviation <- paste(input$scale.type.var.single) |> str_to_upper()
    })
    
    
    
    #### Detailed scale descriptions----
    scale_detailed_description_file <- reactive({
        switch(input$scale.type.var.single,
               "pss" = "./text_mds/scales_detailed_descriptions/pss_detailed.md",
               "stai" = "./text_mds/scales_detailed_descriptions/stai_detailed.md",
               "cesd" = "./text_mds/scales_detailed_descriptions/cesd_detailed.md",
               "pcl" = "./text_mds/scales_detailed_descriptions/pcl_detailed.md",
               "uls" = "./text_mds/scales_detailed_descriptions/uls_detailed.md",
               "sc" = "./text_mds/scales_detailed_descriptions/sc_detailed.md",
               "risc" = "./text_mds/scales_detailed_descriptions/risc_detailed.md",
               "ptgi" = "./text_mds/scales_detailed_descriptions/ptgi_detailed.md",
               "mhc" = "./text_mds/scales_detailed_descriptions/mhc_detailed.md")
    })
    
    output$long.scale.description <- renderUI({
        includeMarkdown(scale_detailed_description_file())
    })
    
    
    #### Descriptive table ----
    
    descriptives_scale_selected <- reactive({ 
       
        # num NAs for total scores of interest at each time point
        freq.nas <- dat.mini %>% 
            dplyr::select(all_of(index_individual_scale_selected_explore())) %>% 
            dplyr::summarize_all(funs(sum(is.na(.)))) %>% 
            t(.) %>% 
            as.data.frame()
    
        # summary stats
        total.summary <- as.data.frame(psych::describe(dat.mini[index_individual_scale_selected_explore()]))
    
        # add num NAs and round numbers
        total.summary <- total.summary %>% 
            mutate(NAs = freq.nas[,1]) %>% 
            mutate(across(where(is.numeric), round, 2))
    
        # add time point
        total.summary <- total.summary %>% 
            rownames_to_column(var = "timept") %>% 
            mutate(timept = timept %>% str_extract(".{2}$") %>% str_to_upper()) %>% 
            #mutate(timept = timept %>% str_replace_all("\\.", " ") %>% str_to_upper()) %>% 
            remove_rownames() %>% 
            dplyr::select(-c(vars, trimmed, mad))
    
        total.summary 
    })
    
    output$descriptives.table <- DT::renderDataTable({
        descriptives_scale_selected()
    }, options = list(paging = FALSE, dom = "t"), rownames = FALSE)
    
    
    
    #### Item response distributions ----
    
    item_response_distribution_data_selected <- reactive({
        
        # TO DO: read in item-level response data to do this
    })
    
    output$plot.item.response.distribution.selected <- renderPlot({

    })
 
    ### Distributions ----
    
    #### Raincloud plots ----
    
    output$single.scale.raincloudPlot <- renderPlotly({
    
        raincloud.data <- data_individual_scale_selected_explore() |>
            dplyr::mutate(
                time.pt = factor(time.pt,
                                 levels = c("T4", "T3", "T2", "T1"))) 
        
            raincloud.plot <- chronicle::make_raincloud(
                dt = raincloud.data,
                value = 'score',
                groups = 'time.pt',
                adjust = 0.5, # kernal width of bins
                include_boxplot = TRUE,
                include_mean = TRUE,
                include_median = TRUE,
                force_all_jitter_obs = TRUE,
                ggtheme = 'bw',
                plot_palette_generator = "viridis",
                x_axis_label = "Scale score")
            
            raincloud.plot 
            
            })
                                      
    #### Histograms ----           
    
    output$single.scale.histogramPlot <- renderPlotly({
        
        p <- data_individual_scale_selected_explore() |>
            
            ggplot(aes(score)) +
            
            # add histogram
            geom_histogram(binwidth = input$histo.bin.size,
                           color = 'white',
                           fill = 'black') +
            
            # x- and y-axis labels
            labs(
                #title = "Frequency of scores",
                x = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores"), 
                y = "Frequency") +
            
            # apply theme
            theme_bw() +
            
            theme(
                # remove legend
                legend.position = "none",
                
                # color of facet strip and facet text
                strip.background = element_rect(
                    color = "black",
                    fill = "#526ea3"),
                strip.text = element_text(
                    color = "white",
                    face = "bold",
                    size = 12)) +

            # facet according to time point
            facet_wrap( ~ time.pt)
        
        gp <- ggplotly(p)
        gp
    })                       
                
    
    #### Estimated density plot ----
    
    output$single.scale.estimateddensityPlot <- renderPlotly({    
    
        density.plot <- data_individual_scale_selected_explore() |>

        ggplot(aes(score)) +
       
        geom_density(
            aes(color = time.pt,
                fill = time.pt),
            alpha = .3,
            linewidth = .5) +
        
        theme_bw() +
        
        # apply color
        scale_fill_manual(
            name = "Time point", 
            values = color.palette.tmpt) +
        
        scale_color_manual(
            name = "Time point", 
            values = color.palette.tmpt) +
        
        labs(
            title = str_glue("Estimated densities of {abbreviation_individual_scale_selected_explore()} scores"),
            x = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores"),
            y = "Estimated density"
        )
    
        # option to view by time point
        if (input$estimated.density.single.faceted == "Multiple") {
            density.plot <- density.plot + facet_wrap(~ time.pt)
        }

    gp <- ggplotly(density.plot)
    gp
})
  
    
    #### Box plot ----
    
    output$single.scale.boxPlot <- renderPlotly({    
        
        box.plot <- data_individual_scale_selected_explore() |>
            
            ggplot(aes(
                x = time.pt, 
                y = score, 
                fill = time.pt)) + 
            
            # Add individual points 
            geom_jitter(
                # if want color of points to be same color as box plots
                #aes(color = time.pt),
                color = "black",
                size = 0.5, 
                alpha = 0,
                width = .1) + 
            
            scale_fill_manual(values = color.palette.tmpt) +
        
            geom_boxplot() + 
            
            geom_jitter(
                # if want color of points to be same color as box plots
                #aes(color = time.pt),
                color = "black",
                size = 0.5, 
                alpha = 0.75,
                width = .1) + 
            
            labs(
                title = str_glue("Box plots of total {abbreviation_individual_scale_selected_explore()} scores at each time point"),
                x = "Time point", 
                y = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores")
            ) +
            
            # apply theme
            theme_bw() +
            
            # remove legend
            theme(legend.position = "none") 
            
        ggplotly(box.plot) 
        
    })
    
    
    #### Violin plot ----
    
    output$single.scale.violinPlot <- renderPlotly({    
        
        p <- data_individual_scale_selected_explore() |>
         
            ggplot(aes(x = time.pt, y = score, fill = time.pt)) + 
            
            geom_sina(
                # size of point
                size = 1,
                # make semi-transparent
                alpha = 0,
                # control range/width of points
                maxwidth = .7) +
            
            # color palette to use
            scale_fill_manual(values = c(
                "T1" = "#fde725", # yellow
                "T2" = "#35b779", # green
                "T3" = "#316883", # blue
                "T4" = "#472c7a" # purple
                    )) +
            
            # make violin plot
            geom_violin(
                # scale violin area proportionate to number of observations
                scale = "count",
                # bandwidth adjustment - SD of smoothing kernel
                bw = .7) + 
            
            # add individual points
            geom_sina(
                # size of point
                size = 1,
                # make semi-transparent
                alpha = .75,
                # control range/width of points
                maxwidth = .7) +
            
            # apply theme
            theme_bw() +
            
            # remove legend
            theme(legend.position = "none") +
            
            # add axis labels
            labs(
                title = str_glue("Violin plots of total {abbreviation_individual_scale_selected_explore()} scores at each time point"),
                x = "Time point", 
                y = str_glue("Total {abbreviation_individual_scale_selected_explore()} scores")
            ) 
        
        # calculate the mean and SD to add to the plot  
        violin.plot <- p + stat_summary(
            fun.data = mean_sdl,
            fun.args = list(mult = 1),
            geom = "pointrange", 
            color = '#ff9966',
            size = 2)
        
        ggplotly(violin.plot)
    })
    
      
    
    ### Scores over time ----
    
    #### Corset plots ----
    
    # If SC scale, time point compare option only T1 vs T4, otherwise
    # options are: T1 vs T2, T2 vs T3, T3 vs T4, T1 vs T4
    
    output$corset.tmpt.compare.options <- renderUI({
        
        if (input$scale.type.var.single == "sc") {
            radioButtons(
                inputId = "corset.plot.tmpts",
                label = "Time points to compare",
                choices = c("T1 vs T4"),
                selected = "T1 vs T4",
                inline = T)
        } else {
            radioButtons(
                inputId = "corset.plot.tmpts",
                label = "Time points to compare",
                choices = c("T1 vs T2", "T2 vs T3", 
                            "T3 vs T4", "T1 vs T4"),
                selected = "T1 vs T2",
                inline = T)
        }
    })
    
    # Extract time point A
    change_timepoint_a <- reactive({
        split_string <- paste(input$corset.plot.tmpts) |> strsplit(" ")
        split_string[[1]][1]
    })
    
    # Extract time point B
    change_timepoint_b <- reactive({
        split_string <- paste(input$corset.plot.tmpts) |> strsplit(" ")
        split_string[[1]][3]
    })
    
    # Extract difference score and direction of change between 2 time points 
    data_individual_scale_change_timepoints <- reactive({
        
        time.a <- tolower(change_timepoint_a())
        time.b <- tolower(change_timepoint_b())

        # get the columns of interest (score at the 2 time points, change, 
        # and direction values)
        col.interest.a <- paste(input$scale.type.var.single, ".total.", time.a, sep = "")
        col.interest.b <- paste(input$scale.type.var.single, ".total.", time.b, sep = "")
        col.change <- paste(input$scale.type.var.single, ".change.", time.a, ".", time.b, sep = "")
        col.direction <- paste(input$scale.type.var.single, ".direction.", time.a, ".", time.b, sep = "")
        
        vars <- c(col.interest.a, col.interest.b, 
                  col.change, col.direction)
        
        corset.data <- dat.mini %>% 
            dplyr::select(all_of(vars),
                          ccc.id:mins.week.prac.t4)
        
        colnames(corset.data)[3:4] <- c("change", "direction")
        
        corset.data
    })
    
    
    # Get data for corset plot - count by each direction type
    select_corset_data <- reactive({
        
        dat.corset.select <- data_individual_scale_change_timepoints() |>
            dplyr::filter(!is.na(change)) |>
            dplyr::group_by(direction) |> 
            mutate(Count = n()) |> 
            dplyr::ungroup() |> 
            mutate(direction_updated = paste0(direction, ": n=", Count))
        
        dat.corset.select
    })
    
    # Create corset plot for data of interest
    output$single.scale.corsetPlot <- renderPlot({
        
        corset.plot <- select_corset_data() |>
            gg_corset(
                y_var1 = 1,
                y_var2 = 2,
                c_var = "direction_updated",
                group = "ccc.id",
                eyelets = T,
                faceted = T,
                e_type = "SE",
                line_size = 1
            ) +
            
            scale_color_manual(name = "Direction of change:", 
                               values = c("#46327e", "#4ac16d", "#fde725")) +
            
            theme(
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 11, face = "bold", hjust = 0),
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                strip.text.x = element_text(
                    size = 12, face = "bold.italic"
                ),
                strip.text.y = element_text(
                    size = 11, 
                    face = "bold.italic"),
                axis.title.y.left = element_text(
                    hjust = .5,
                    size = 11)
            ) +
            
            labs(
                title = str_glue("Change in {abbreviation_individual_scale_selected_explore()} scores - {change_timepoint_a()} vs {change_timepoint_b()}"),
                x = "",
                y = ""
            ) +
            
            scale_y_continuous(
                name = str_glue("{abbreviation_individual_scale_selected_explore()} Scores")) +
            
            scale_x_discrete(
                labels = c(change_timepoint_a(), change_timepoint_b())) 
        
    corset.plot
    
    })    
    
    
    # Get counts and avg by direction change type 
    data_corset_change_counts <- reactive({
        
        count.change <- select_corset_data() |>
            dplyr::group_by(direction) |>
            dplyr::summarize(Count = n(),
                      Avg = round(mean(change, na.rm = TRUE), 3))
        count.change
    })
    
    
    # Create histogram of change scores 
    output$single.scale.corset.change.histoPlot <- renderPlotly({
        
        corset.histo.plot <- select_corset_data() |> 
            
            ggplot(aes(change)) +
            
            geom_histogram(
                binwidth = 1,
                color = 'black',
                fill = histo.color
            ) + 
            
            theme_bw() +
            
            # add vertical lines for mean of decrease and increase
            geom_vline(
                xintercept = data_corset_change_counts()$Avg[data_corset_change_counts()$direction == 'Decrease'][1],
                size = 1,
                color = mean.color,
                linetype = 'dashed'
            ) +
            
            geom_vline(
                xintercept = data_corset_change_counts()$Avg[data_corset_change_counts()$direction == 'Increase'][1],
                size = 1,
                color = mean.color,
                linetype = 'dashed'
            ) +
            
            # add vertical line for 0
            geom_vline(
                xintercept = 0,
                size = 1,
                linetype = 'dashed'
            ) +
            labs(
                title = str_glue("Distribution of change in {abbreviation_individual_scale_selected_explore()} scores {change_timepoint_a()} vs {change_timepoint_b()}"),
                x = str_glue("Change in {abbreviation_individual_scale_selected_explore()} scores"),
                y = "Frequency") 

        ggplotly(corset.histo.plot)
        
    })
    
    
    
    #### Scores by date ---- 
    
    # Prepare scores by month data:
    # Select scale of interest
    # Round date to 1st of month & group by month
    # Calculate count, mean, and median 
    
    scores_by_date_data <- reactive({
        scores.by.month.dat <- dat.long |>
            dplyr::filter(scale %in% input$scale.type.var.single) |>
            mutate(
                month.date = lubridate::floor_date(recordeddate, "month")) |>
            dplyr::group_by(month.date) |>
            
            dplyr::summarize(
                avg.score = round(mean(score, na.rm = TRUE), 2),
                median.score = median(score, na.rm = TRUE),
                n = n()) |>
            dplyr::filter(month.date < '2022-01-31') |>
            dplyr::ungroup()
    })
    
    
  
    # Text for plot title and caption

    score_by_date_title <- reactive({
        
        if (input$scores.by.date.type == "avg.score") {
            score_type_text <- "average"
        } else if (input$scores.by.date.type == "median.score") {
            score_type_text <- "median"
        }
        
        score_type_title_text <- str_glue("Line plot of {score_type_text} {abbreviation_individual_scale_selected_explore()} scores by month")
       
        return(score_type_title_text)
        
    })
    
    score_by_date_caption <- reactive({
        
        if (input$covid_cases_deaths == "weekly.cases") {
            covid_type_text <- "cases"
        } else if (input$covid_cases_deaths == "weekly.deaths") {
            covid_type_text <- "deaths"
        }
        
        covid_type_caption_text <- str_glue("Weekly number of U.S. Covid-19 {covid_type_text} overlaid in red. Value is reflected on the right y-axis.")
        
        return(covid_type_caption_text)
    })
        
    
    # Line plot (highcharter)
    
    output$hc_single_scale_date_linePlot <- renderHighchart({
        
       hc <- highchart(type = "chart") |> 
           
           # set up axes
           hc_yAxis_multiples(
               list(title = list(text = input$scores.by.date.type)),
               list(title = list(text = input$covid_cases_deaths),
                    labels = list(style = list(color = "red")),
                    showLastLabel = FALSE, 
                    opposite = TRUE)) %>% 
           
            hc_xAxis(type = "datetime") %>% 
           
           # add line for scores
           hc_add_series(name = "Score",
                         data = scores_by_date_data(),
                         color = "black",
                         id = "score",
                         'line',
                         hcaes(x = month.date,
                               y = !!input$scores.by.date.type),
                         tooltip = list(
                             pointFormat = "Score={point.y}, n={point.n}")) %>% 
           
           # add circles, sized for number of people
           hc_add_series(name = input$scores.by.date.type,
                         data = scores_by_date_data(),
                         'scatter',
                         linkedTo = "score",
                         color = 'black',
                         hcaes(x = month.date,
                               y = !!input$scores.by.date.type,
                               size = n),
                          tooltip = list(
                              pointFormat = "{point.x:%b %Y}<br>Score={point.y}<br>n={point.n}")) %>%
           
           hc_title(text = score_by_date_title()) %>% 
           
           # add tooltip formatting
           hc_tooltip(crosshairs = TRUE)
        
        
       
       # add weekly covid deaths/cases if selected
        if (input$covid_cases_deaths != "none") {
            
            hc <- hc %>%
                hc_add_series(name = input$covid_cases_deaths,
                              data = covid.cases.deaths.dat, 
                              "spline", 
                              yAxis = 1,
                              color = "red",
                              hcaes(x = date, y = !!input$covid_cases_deaths)) %>% 
                hc_caption(text = score_by_date_caption()) %>% 
                
                hc_credits(enabled = TRUE,
                           text = "Covid-19 data downloaded from CDC COVID Data Tracker", 
                           href = "https://covid.cdc.gov/covid-data-tracker/#trends_weeklycases_select_00")
            
        }
        
        hc
    
    })
    
    
    
    
    #### Scores by date by time point---- 
    
    # Prepare scores by month data:
    # Select scale of interest
    # Round date to 1st of month & group by time point & month
    # Calculate count, mean, and median 
    
    scores_by_date_by_timepoint_data <- reactive({
        scores.by.month.dat <- dat.long |>
            dplyr::filter(scale %in% input$scale.type.var.single) |>
            mutate(
                month.date = lubridate::floor_date(recordeddate, "month")) |>
            dplyr::group_by(time.pt, month.date) |>
            
            dplyr::summarize(
                avg.score = round(mean(score, na.rm = TRUE), 2),
                median.score = median(score, na.rm = TRUE),
                n = n()) |>
            
            # dplyr::filter(!is.na(avg.score) | !is.na(median.score))
            
            dplyr::filter(month.date < '2022-01-31') |>
            
            mutate(color_group = case_when(
                time.pt == "T1" ~ "#fde725",
                time.pt == "T2" ~ "#35b779",
                time.pt == "T3" ~ "#316883",
                time.pt == "T4" ~ "#472c7a"
                    ),
                tmpt = time.pt) |>
            
            dplyr::ungroup()
    })

    
    # Text for plot title and caption
    
    score_by_date_by_timepoint_title <- reactive({
        
        if (input$scores.by.date.by.timepoint.type == "avg.score") {
            score_type_text <- "average"
        } else if (input$scores.by.date.by.timepoint.type == "median.score") {
            score_type_text <- "median"
        }
        
        score_type_title_text <- str_glue("Line plot of {score_type_text} {abbreviation_individual_scale_selected_explore()} scores by month by time point")
        
        return(score_type_title_text)
        
    })
    
    score_by_date_by_timepoint_caption <- reactive({
        
        if (input$covid_cases_deaths_tmpt == "weekly.cases") {
            covid_type_text <- "cases"
        } else if (input$covid_cases_deaths_tmpt == "weekly.deaths") {
            covid_type_text <- "deaths"
        }
        
        covid_type_caption_text <- str_glue("Weekly number of U.S. Covid-19 {covid_type_text} overlaid in red. Value is reflected on the right y-axis.")
        
        return(covid_type_caption_text)
    })
    
    
    # Line plot (highcharter)
    
    output$hc_single_scale_date_tmpt_linePlot <- renderHighchart({
        
        hc <- highchart(type = "chart") |> 
            
            # set up axes
            hc_yAxis_multiples(
                list(title = list(text = input$scores.by.date.by.timepoint.type)),
                list(title = list(text = input$covid_cases_deaths_tmpt),
                     labels = list(style = list(color = "red")),
                     showLastLabel = FALSE, 
                     opposite = TRUE)) %>% 
            
            hc_xAxis(type = "datetime") %>% 
            
            # add line for scores
            hc_add_series(
                          data = scores_by_date_by_timepoint_data(),
                          color = color.palette.tmpt,
                          id = "score",
                          'line',
                          hcaes(x = month.date,
                                y = !!input$scores.by.date.by.timepoint.type,
                                group = time.pt),
                          tooltip = list(
                              pointFormat = "Score={point.y}, n={point.n}<br>Time point: {point.tmpt}")) %>% 
            
            # add circles, sized for number of people
            hc_add_series(name = input$scores.by.date.by.timepoint.type,
                          data = scores_by_date_by_timepoint_data(),
                          'scatter',
                          linkedTo = "score",
                          hcaes(x = month.date,
                                y = !!input$scores.by.date.by.timepoint.type,
                                color = color_group,
                                size = n),
                          tooltip = list(
                              pointFormat = "{point.y}, n={point.n}<br>{point.x:%b %Y}<br>Time point: {point.tmpt}")) %>%
            
            hc_title(text = score_by_date_by_timepoint_title()) %>% 
            
            # add tooltip formatting
            hc_tooltip(crosshairs = TRUE)
        
        
        
        # add weekly covid deaths/cases if selected
        if (input$covid_cases_deaths_tmpt != "none") {
            
            hc <- hc %>%
                hc_add_series(name = input$covid_cases_deaths_tmpt,
                              data = covid.cases.deaths.dat, 
                              "spline", 
                              yAxis = 1,
                              color = "red",
                              hcaes(x = date, y = !!input$covid_cases_deaths_tmpt)) %>% 
                hc_caption(text = score_by_date_by_timepoint_caption()) %>% 
                
                hc_credits(enabled = TRUE,
                    text = "Covid-19 data downloaded from CDC COVID Data Tracker", 
                           href = "https://covid.cdc.gov/covid-data-tracker/#trends_weeklycases_select_00")
        }
        
        hc

    })
    
    
    
 
})   
    

    