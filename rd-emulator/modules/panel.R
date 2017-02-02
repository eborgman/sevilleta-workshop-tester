panelInput <- function(id, label="Revisit design") {
  # A module UI function for numeric input related to panel design.
  ns <- NS(id)  # a namespace function using the provided id
  style <-
    'display:inline-block; vertical-align:center; width:20%;'
  tagList(
    tags$div(style=style,
             numericInput(ns('d_left'), label=h5('On'), value=1, min=1)),
    tags$div(style=style,
             numericInput(ns('d_right'), label=h5('Off'), value=0, min=0)),
    tags$div(style=style,
             numericInput(ns('n_units'), label=h5('Units'), value=1, min=1)),
    tags$div(style=style,
             numericInput(ns('n_panels'), label=h5('Panels'), value=1, min=1)),
    bsTooltip(ns('d_left'), 'The number of consecutive times a panel is sampled before it is rotated out of the sample for a period of time',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('d_right'), 'The number of consecutive times a panel is not sampled before it is rotated back into the sample',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('n_units'), 'The number of sample units',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('n_panels'), 'The number of panels with the specified revisit design',
              placement='bottom', trigger='hover', options=NULL)
  )
}

panel <- function(input, output, session) {
  # Server logic for the panel design module UI function.
  reactive({
    data.frame(d_left=input$d_left,
               d_right=input$d_right,
               n_units=input$n_units,
               n_panels=input$n_panels)
  })
}
