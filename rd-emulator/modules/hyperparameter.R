dist_init <- data.frame(
  dist=c('norm', 'lnorm', 'pois', 'binom'),
  beta_0=c(3, 0.5, 2, 0.25),
  beta_1=c(1.5, -1, -0.4, -0.5),
  sigma_u0=c(1, 0.2, 0.6, 0.1),
  sigma=c(2, 0.75, NA, NA)
)

hyperparameterInput <- function(input, id, label="Panel input",
                                dist_lookup=dist_init) {
  # A module UI function for numeric input related to hyperparameters.
  ns <- NS(id)  # a namespace function using the provided id
  style <-
    'display:inline-block; vertical-align:center; width:20%;'
  init_values <- dist_lookup %>% filter(dist==input$dist)
  tagList(
    tags$div(style=style,
             numericInput(ns('beta_0'), label=h5(withMathJax('$$\\beta_0$$')),
                          value=init_values$beta_0)),
    tags$div(style=style,
             numericInput(ns('beta_1'), label=h5(withMathJax('$$\\beta_1$$')),
                          value=init_values$beta_1)),
    tags$div(style=style,
             numericInput(ns('sigma_u0'), label=h5(withMathJax('$$\\sigma_{u_0}$$')),
                          value=init_values$sigma_u0, min=0)),
    tags$div(style=style,
             numericInput(ns('sigma'), label=h5(withMathJax('$$\\sigma$$')),
                          value=init_values$sigma, min=0)),
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
