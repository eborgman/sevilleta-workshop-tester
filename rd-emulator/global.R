panelInput <- function(id, label="Panel input") {
  # A module UI function for numeric input related to panel design.
  ns <- NS(id)  # a namespace function using the provided id
  style <-
    'display:inline-block; vertical-align:center; width:20%;'
  tagList(
    tags$div(style=style,
             numericInput(ns('d_left'), label='On', value=1, min=1)),
    tags$div(style=style,
             numericInput(ns('d_right'), label='Off', value=0, min=0)),
    tags$div(style=style,
             numericInput(ns('n_units'), label='Units', value=1, min=1)),
    tags$div(style=style,
             numericInput(ns('n_panels'), label='Panels', value=1, min=1)),
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

hyperparametersInput <- function(id, label="Panel input") {
  # A module UI function for numeric input related to hyperparameters.
  ns <- NS(id)  # a namespace function using the provided id
  style <-
    'display:inline-block; vertical-align:center; width:20%;'
  tagList(
    tags$div(style=style,
             numericInput(ns('sigma_u0'), label=h4(withMathJax('$$\\sigma_{u_0}$$')),
                          value=1, min=0)),
    tags$div(style=style,
             numericInput(ns('beta_0'), label=h4(withMathJax('$$\\beta_0$$')),
                          value=3)),
    tags$div(style=style,
             numericInput(ns('sigma'), label=h4(withMathJax('$$\\sigma_{\\epsilon_0}$$')),
                          value=2, min=0)),
    tags$div(style=style,
             numericInput(ns('beta_1'), label=h4(withMathJax('$$\\beta_1$$')),
                          value=1.5)),
    bsTooltip(ns('sigma_u0'), 'Random intercepts',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('beta_0'), 'Population intercept',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('sigma'), 'Individual-level error',
              placement='bottom', trigger='hover', options=NULL),
    bsTooltip(ns('beta_1'), 'Population slope',
              placement='bottom', trigger='hover', options=NULL)
  )
}

hyperparameters <- function(input, output, session) {
  # Server logic for the hyperparameters module UI function.
  reactive({
    data.frame(sigma_u0=input$sigma_u0,
               beta_0=input$beta_0,
               sigma=input$sigma,
               beta_1=input$beta_1)
  })
}

removeInputObserver <- function(input) {
  remove_input <- names(input)[grep('removeInput', names(input))]
  is_removed <- sapply(remove_input, function(x) input[[x]])
  gsub('removeInput', 'panelInput', names(is_removed))[is_removed==1]
}

design_params_to_data <- function(data) {
  periods <- (data$last_period - data$first_period + 1)
  reps <- ceiling(periods / (data$d_left + data$d_right))
  on_off_seq <- rep(c(rep(1, data$d_left), rep(0, data$d_right)), reps)
  panel_designs <- lapply(seq_len(data$n_panels), function(x) {
    last <- x - 1
    data.frame(
      subpanel=x,
      period=seq(data$first_period, data$last_period),
      is_sampled=c(tail(on_off_seq, n=last), on_off_seq[1:(periods-last)]))
  })
  rep_subpanel_design <- function(x, reps) {
    replicate(reps, x, simplify=FALSE) %>%
      bind_rows %>%
      mutate(unit=rep(seq(1, reps), each=periods))
  }
  panel_designs %>%
    bind_rows %>%
    group_by(subpanel) %>%
    do(rep_subpanel_design(., data$n_units)) %>%
    ungroup %>%
    as.data.frame
}

get_design_framework <- function(data) {
  data %>%
    group_by(panel_input) %>%
    do(design_params_to_data(.)) %>%
    ungroup %>%
    mutate(panel=as.numeric(factor(panel_input))) %>%
    select(-panel_input)
}

get_design_framework_summary <- function(data) {
  data %>%
    group_by(panel, subpanel, period) %>%
    summarise(samples=sum(is_sampled)) %>%
    mutate(samples=ifelse(samples==0, NA, samples),
           panel_subpanel=factor(paste(panel, subpanel, sep='_'))) %>%
    ungroup
}

get_n_units_per_period <- function(data) {
  data %>% group_by(period) %>% summarise(n=sum(samples, na.rm=TRUE))
}

get_u_units <- function(data) {
  data %>% group_by(panel, subpanel) %>% summarise(u=n_distinct(unit)) %>%
    .[['u']] %>% sum
}

get_design_notation <- function(data) {
  design_vec <- data %>%
    mutate(panel_design_notation=
             paste0('(', paste(d_left, d_right, sep='~`-`~'), ')^', n_panels)) %>%
    .[['panel_design_notation']]
  paste0('`[`~', paste(design_vec, collapse='~`,`~'), '~`]`')
}

sim_data_using_hyperparams <- function(input, data) {
  design_framework <- get_design_framework(data)
  u_units <- get_u_units(design_framework)
  unit_reps_summary <- design_framework %>%
    group_by(panel, subpanel, unit) %>%
    mutate(psu=paste(panel, subpanel, unit, sep='_')) %>%
    summarise(psu_verbose=psu[1], n=n()) %>%
    ungroup %>%
    mutate(psu=as.numeric(factor(psu_verbose)))

  # Set seed for replicability.
  set.seed(123)
  # Random intercepts.
  sigma_u0 <- input[['hyperparamsInput-sigma_u0']]
  u_0j     <- rnorm(n = u_units, mean = 0, sd = sigma_u0)
  # Population intercept.
  beta_0   <- input[['hyperparamsInput-beta_0']]
  # Varying intercepts.
  beta_0j  <- rep(beta_0 + u_0j, unit_reps_summary %>% .[['n']])
  # Cluster indicator.
  cluster  <- rep(seq(1, u_units), unit_reps_summary %>% .[['n']])
  # Individual-level error.
  sigma    <- input[['hyperparamsInput-sigma']]
  e_0ij    <- rnorm(n = nrow(design_framework), mean = 0, sd = sigma)
  # Covariate (based on period).
  X_1ij    <- design_framework$period - min(design_framework$period)
  # X_2ij    <- runif(n = nrow(design_framework), min = 0, max = 20)
  # Population slope.
  beta_1   <- input[['hyperparamsInput-beta_1']]
  # Outcome.
  Y_ij     <- beta_0j + beta_1*X_1ij + e_0ij

  design_framework %>% mutate(y=Y_ij)
}
