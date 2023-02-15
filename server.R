
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
    
    
    ### Table of demographic counts ----
    
    output$demographic.table <- DT::renderDataTable({
        dat.mini |>
            dplyr::group_by_at(input$demo.treemaps.var) |>
            dplyr::summarize(Count = n())
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
                    alpha =.5, 
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
    
    output$scale.description <- renderText({
        paste0(scale.descriptions$scale.description[scale.descriptions$scale == input$scale.type.var])
    })
    
    
    
    
    ### Well-being correlation matrices ----
    
    # View correlation matrices for each time point
    output$scale.correlation.heatmap.t1 <- renderPlot({
        correlation.heatmap(corr.t1, p.mat.t1)
    })
    
    output$scale.correlation.heatmap.t2 <- renderPlot({
        correlation.heatmap(corr.t2, p.mat.t2)
    })
    
    output$scale.correlation.heatmap.t3 <- renderPlot({
        correlation.heatmap(corr.t3, p.mat.t3)
    })
    
    output$scale.correlation.heatmap.t4 <- renderPlot({
        correlation.heatmap(corr.t4, p.mat.t4)
    })
}
)