
# SERVER ----

shinyServer(function(input, output) {
    
    # location of md files for helper pop-ups
    observe_helpers(help_dir = "helpfiles_mds")
    
    ## Demographic info ----
    
    ### Tree maps ----
    output$demographic.treemap.plots <- renderPlotly({
        subplot(
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
        marg_plot <- subplot(marginal.x.plot(), 
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
     
    
    ### Correlation matrices ----
    
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
    
    
    
    ### Detailed scale descriptions----
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
    
    
    ### Descriptive table ----
    
    descriptives_scale_selected <- reactive({ 
       
        # num NAs for total scores of interest at each time point
        freq.nas <- dat.mini %>% 
            dplyr::select(all_of(index_individual_scale_selected_explore())) %>% 
            dplyr::summarize_all(funs(sum(is.na(.)))) %>% 
            t(.) %>% 
            as.data.frame()
    
        # summary stats
        total.summary <- as.data.frame(describe(dat.mini[index_individual_scale_selected_explore()]))
    
        # add num NAs and round numbers
        total.summary <- total.summary %>% 
            mutate(NAs = freq.nas[,1]) %>% 
            mutate(across(where(is.numeric), round, 2))
    
        # add time point
        total.summary <- total.summary %>% 
            rownames_to_column(var = "timept") %>% 
            remove_rownames() %>% 
            dplyr::select(-c(vars, trimmed, mad))
    
        total.summary 
    })
    
    output$descriptives.table <- DT::renderDataTable({
        descriptives_scale_selected()
    }, options = list(paging = FALSE, dom = "t"), rownames = FALSE)
    
    
    
    ### Item response distributions ----
    
    item_response_distribution_data_selected <- reactive({
        
        # TO DO: read in item-level response data to do this
    })
    
    output$plot.item.response.distribution.selected <- renderPlot({

    })
 
    
    ### Raincloud plots ----
    
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
                                      
    ### Histograms ----           
    
    output$single.scale.histogramPlot <- renderPlotly({
        
        p <- data_individual_scale_selected_explore() |>
            
            ggplot(aes(score)) +
            
            # add histogram
            geom_histogram(binwidth = input$histo.bin.size,
                           color = 'white',
                           fill = 'black') +
            
            # # add mean/median lines
            # add.mean.line.histo +
            # add.median.line.histo +
            # 
            # # add min/max lines
            # my_add.min.max.lines.vert(
            #     min.score = min.score,
            #     max.score = max.score
            # ) +
            
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
                
    
    ### Estimated density plot ----
    
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
    
    gp <- ggplotly(density.plot)
    gp
})
  
    
    ### Box plot ----
    
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
    
    
    ### Violin plot ----
    
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
    
      
})    
    
    