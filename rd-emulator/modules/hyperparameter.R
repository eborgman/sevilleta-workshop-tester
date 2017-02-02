hyperparameterInput <- function(id, label="Panel input") {
  # A module UI function for numeric input related to hyperparameters.
  ns <- NS(id)  # a namespace function using the provided id
  style <-
    'display:inline-block; vertical-align:center; width:20%;'
  tagList(
    tags$div(style=style,
             numericInput(ns('beta_0'), label=h4(withMathJax('$$\\beta_0$$')),
                          value=3)),
    tags$div(style=style,
             numericInput(ns('beta_1'), label=h4(withMathJax('$$\\beta_1$$')),
                          value=1.5)),
    tags$div(style=style,
             numericInput(ns('sigma_u0'), label=h4(withMathJax('$$\\sigma_{u_0}$$')),
                          value=1, min=0)),
    tags$div(style=style,
             numericInput(ns('sigma'), label=h4(withMathJax('$$\\sigma_{\\epsilon_0}$$')),
                          value=2, min=0)),
    bsTooltip(ns('beta_0'), 'Population intercept',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('beta_1'), 'Population slope',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('sigma_u0'), 'Random intercepts',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('sigma'), 'Individual-level error',
              placement='bottom', trigger='hover', options=NULL)
  )
}

hyperparameter <- function(input, output, session) {
  # Server logic for the hyperparameters module UI function.
  reactive({
    data.frame(beta_0=input$beta_0,
               beta_1=input$beta_1,
               sigma_u0=input$sigma_u0,
               sigma=input$sigma)
  })
}
