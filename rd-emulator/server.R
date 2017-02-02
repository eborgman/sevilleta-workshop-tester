library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)


function(input, output) {

  data <- reactive({
    dist <- switch(input$dist,
                   norm=rnorm,
                   lnorm=rlnorm,
                   pois=rpois,
                   # binom=rbinom,
                   rnorm)
    # dist(input$n)  # use design_framework object
  })

  observeEvent(input$addInput, {
    add <- input$addInput
    inputId <- paste0('input', add)
    panelInputId <- paste0('panelInput', add)
    removeInputId <- paste0('removeInput', add)
    insertUI(
      selector = '#placeholderInput',
      ui=tags$div(id=inputId,
                    panelInput(panelInputId),
                    bsButton(removeInputId, label='',
                             icon=icon('remove', lib='glyphicon'),
                             size='extra-small', style='danger'))
    )
    observeEvent(input[[removeInputId]], {
      removeUI(selector = paste0('#', inputId))
    })
  })

  core_panel_data <- reactive({
    period_range <- input$survey_period
    ui_panel_inputs <- str_extract(names(input), "^panelInput[[:digit:]]+") %>%
      na.omit %>%
      unique
    bind_inputs <- function(x) {
      panel_input <- callModule(panel, x)
      data.frame(panel_input=x, first_period=min(period_range),
                 last_period=max(period_range),
                 panel_input(), stringsAsFactors=FALSE)
    }
    dropped_panel_input = removeInputObserver(input)
    return(list(df=lapply(ui_panel_inputs, bind_inputs) %>%
                  bind_rows %>%
                  filter(!panel_input %in% dropped_panel_input)))
  })

  output$test_this <- renderUI({
    if(input$dist=='norm') {
      eq <- withMathJax("$$\\begin{align}
                        Y_{ij} &\\sim N(\\beta_{0 j}+\\beta_1X_{1ij}, \\sigma^2) \\\\
                        \\beta_{0j}&=\\beta_{0}+u_{0j} \\\\
                        u_{0j}&=N(0, \\sigma^2_{u_0}) \\\\
                        \\end{align}$$")
    } else if (input$dist=='lnorm') {
      eq <- withMathJax("$$\\begin{align}
                        Y_{ij} &\\sim lognormal(\\beta_{0 j}+\\beta_1X_{1ij}, \\sigma^2) \\\\
                        \\beta_{0j}&=\\beta_{0}+u_{0j} \\\\
                        u_{0j}&=N(0, \\sigma^2_{u_0}) \\\\
                        \\end{align}$$")
    } else if (input$dist=='pois') {
      eq <- withMathJax("$$\\begin{align}
                        Y_{ij} &\\sim Poisson(\\lambda) \\\\
                        log(\\lambda)&=\\beta_{0 j}+\\beta_1X_{1ij} \\\\
                        \\beta_{0j}&=\\beta_{0}+u_{0j} \\\\
                        u_{0j}&=N(0, \\sigma^2_{u_0}) \\\\
                        \\end{align}$$")
    }
    tagList(
      eq,
      hyperparameterInput(input, 'hyperparamInput')
    )

  })

  output$data <- renderUI({
    sim_df_filtered <- simulated_data()$sim_df_filtered %>%
      select(panel, subpanel, unit, period, y, -is_sampled)
    fluidRow(
      column(5,
             tagList(
               tags$div(
                 style='margin-left: 10px; margin-top: 10px; margin-bottom: 5px;',
                 'Showing the first 10 entries of the data...'),
               # renderDataTable({sim_df_filtered}),
               renderTable({
                 sim_df_filtered %>% slice(1:10)
                 }, 'include.rownames' = FALSE, 'include.colnames' = TRUE),
               tags$div(style='margin-left: 10px;',
                        downloadLink("downloadData", "Download"))
             )
      ),
      column(7,
             tags$div(style='margin-left: 10px; margin-top: 10px;',
                      renderPlot({
               p1 <- ggplot(sim_df_filtered) +
                 geom_histogram(aes(x=y), color='black', fill='white') +
                 labs(x=expression(italic(Y)[ij]), y='Frequency')
               p2 <- ggplot(sim_df_filtered) +
                 geom_point(aes(x=period, y=y), alpha=0.5) +
                 labs(x='Period', y=expression(italic(Y)[ij]))
               gridExtra::grid.arrange(p1, p2, ncol=1)
             }))

      )
    )
  })

  output$analysis <- renderUI({
    rep_analysis <- function(n, data) {
      sim_data_using_hyperparams(input, data)
    }
    dat <- simulated_data()$sim_df_full %>%
      mutate(group=interaction(panel, subpanel, unit)) %>%
      mutate(period=period-min(period))
    m <- glmer(y ~ period + (1|group), data=dat, family='gaussian')
    # browser()
    tmp <- lapply(seq_len(50), rep_analysis,  #
           data=core_panel_data()$df) #
    tmp_out <- lapply(tmp, function(x) {
      x <- x %>%
        mutate(group=interaction(panel, subpanel, unit)) %>%
        mutate(period=period-min(period))
      m1 <- update(m, data=x)
      betas <- summary(m1)$coefficients %>% as.data.frame %>% .[['Estimate']]
      sigmas <- VarCorr(m1) %>% as.data.frame %>% .[['sdcor']]
      data.frame(variable=c('beta_0', 'beta_1', 'sigma_u0', 'sigma'),
                 value=c(betas, sigmas))
    }) %>% bind_rows(., .id='test') %>% as.data.frame
    print(input$analysis_reps)
    print(str(input$analysis_reps))
    fluidRow(
      column(6,
        renderPrint({
          summary(m)
        })
      ),
      column(6,
             tagList(
               numericInput("analysis_reps", label = h4("Analysis replicates"), value = 1),
               h4("Recovery"),
               renderPlot({
                 ggplot(tmp_out) +
                   geom_histogram(aes(x=value), color='black', fill='white') +
                   facet_grid(.~variable, scales='free_x')
               })
             )
      )
    )
  })

  output$downloadData <- downloadHandler(
    filename=function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content=function(file) {
      # get_design_framework(core_panel_data()$df)
      write.csv(simulated_data()$sim_df_full, file, row.names=FALSE)
    }
  )

  simulated_data <- reactive({
    sim_df <- sim_data_using_hyperparams(input, core_panel_data()$df)
    return(list(sim_df_full=sim_df,
                sim_df_filtered=sim_df %>% filter(is_sampled==1)))
  })

  output$layout <- renderPlot({
    df <- core_panel_data()$df
    design_notation <- get_design_notation(df)
    design_framework <- get_design_framework(df)
    design_framework_summary <- get_design_framework_summary(design_framework)
    u_units <- get_u_units(design_framework)
    n_units_per_period <- get_n_units_per_period(design_framework_summary)

    ggplot(design_framework_summary,
           aes(x=period, y=panel_subpanel, size=samples, color=factor(panel))) +
      geom_point(alpha=1) +
      # geom_text(data=n_units_per_period, aes(x=period, y=Inf, label = n)) +
      scale_y_discrete(limits=
                         rev(levels(design_framework_summary$panel_subpanel))) +
      scale_color_brewer('Panel', type='qual', palette='Set1') +
      scale_size('Units',
                 breaks=na.omit(unique(design_framework_summary$samples))) +
      labs(x='Period', y='Panel') +
      ggtitle(paste('Split panel design:', u_units, 'unique units'),
              subtitle=parse(text=design_notation)) +
      theme(plot.title=element_text(hjust=0.5, size=14, face='bold'),
            plot.subtitle=element_text(hjust=0.5, size=14),
            axis.title=element_text(size=14, face='italic'),
            axis.text=element_text(size=12),
            legend.title=element_text(size=12),
            legend.text=element_text(size=12))
  })

}