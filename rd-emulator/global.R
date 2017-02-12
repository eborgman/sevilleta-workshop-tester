source('modules/panel.R')
source('modules/hyperparameter.R')


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
    summarise(n=n()) %>%
    ungroup

  n <- nrow(design_framework)
  unit_reps <- unit_reps_summary %>% .[['n']]

  # Set seed for replicability.
  # set.seed(123)
  # Random intercepts.
  sigma_u0 <- input[['hyperparamInput-sigma_u0']]
  u_0j     <- rnorm(n = u_units, mean = 0, sd = sigma_u0)
  # Population intercept.
  beta_0   <- input[['hyperparamInput-beta_0']]
  # Varying intercepts.
  beta_0j  <- rep(beta_0 + u_0j, unit_reps)
  # Individual-level error.
  sigma    <- input[['hyperparamInput-sigma']]
  # Covariate (based on period).
  X_1ij    <- design_framework$period - min(design_framework$period)
  # X_2ij    <- runif(n = nrow(design_framework), min = 0, max = 20)
  # Population slope.
  beta_1   <- input[['hyperparamInput-beta_1']]
  # Outcome.
  if (input$dist=='norm') {
    y_ij     <- rnorm(n, beta_0j + beta_1*X_1ij, sigma)
  } else if (input$dist=='lnorm') {
    y_ij     <- rlnorm(n, exp(beta_0j + beta_1*X_1ij), sigma)
  } else if (input$dist=='pois') {
    y_ij     <- rpois(n, exp(beta_0j + beta_1*X_1ij))
  } else if (input$dist=='binom') {
    y_ij     <- rbinom(n, 100, inv_logit(beta_0j + beta_1*X_1ij))
  }

  design_framework %>% mutate(y=y_ij)
}

logit <- function(p) {
  log(p / (1 - p))
}

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

