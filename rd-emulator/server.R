library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)


function(input, output) {

  data <- reactive({
    dist <- switch(input$dist,
                   norm=rnorm,
                   lnorm=rlnorm,
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

  output$table <- renderTable({
    if (is.null(core_panel_data())) {return()}
    # print(core_panel_data()$df)
    print(simulated_data()$sim_df %>% filter(is_sampled==1))
  }, 'include.rownames' = FALSE, 'include.colnames' = TRUE,
     'sanitize.text.function' = function(x) { x }
  )

  output$downloadData <- downloadHandler(
    filename=function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content=function(file) {
      # get_design_framework(core_panel_data()$df)
      write.csv(simulated_data()$sim_df, file, row.names=FALSE)
    }
  )

  simulated_data <- reactive({
    return(list(sim_df=sim_data_using_hyperparams(input, core_panel_data()$df)))
  })

  output$plot <- renderPlot({
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
      ggtitle(paste('Revisit design:', u_units, 'unique units'),
              subtitle=parse(text=design_notation)) +
      theme(plot.title=element_text(hjust=0.5, size=14, face='bold'),
            plot.subtitle=element_text(hjust=0.5, size=14),
            axis.title=element_text(size=14, face='italic'),
            axis.text=element_text(size=12),
            legend.title=element_text(size=12),
            legend.text=element_text(size=12))
  })

}