library(dplyr)
library(ggplot2)


data_dir <- file.path(PROJ_ROOT, 'data', 'SCPN')
# fig_dir <- file.path(PROJ_ROOT, 'data', 'SCPN', 'figs')
functional_grp <- read.csv (file.path(data_dir, 'FctGrp_Plot.csv')) %>%
  tbl_df

ecosite_glmer <- function(x, group='TotalLiveFoliarCoverClass_10m') {
  df <- x %>%
    filter(FunctionalGroup==group) %>%
    mutate(RelYear=EventYear-min(EventYear),
           y=CoverClassMidpoint_Plot_mn/100,
           cases=1000)

  m <- tryCatch({
    lme4::glmer(y ~ RelYear + (1|Plot), data=df, family=binomial, weights=cases)
  }, error = function(e) {
    NA
  })

  if (df$EcoSite[1]=='WUPA_S') browser()
  m_coefs <- summary(m)$coefficients[, c('Estimate', 'Std. Error')]
  data.frame(b0=m_coefs['(Intercept)', 'Estimate'],
             b0_sigma=m_coefs['(Intercept)', 'Std. Error'],
             b1=m_coefs['RelYear', 'Estimate'],
             b1_sigma=m_coefs['RelYear', 'Std. Error'])

}

logit <- function(p) {
  log(p / (1 - p))
}

inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

results <- functional_grp %>%
  group_by(EcoSite) %>%
  mutate(n_years=diff(range(EventYear)),
         n_samples=n(),
         n_plots=n_distinct(Plot)) %>%
  ungroup %>%
  filter(n_years>=5, n_plots>30) %>%
  group_by(EcoSite) %>%
  do(ecosite_glmer(.))

results %>%
  mutate(b0_z_value=b0/b0_sigma,
         b1_z_value=b1/b1_sigma)

# inv_logit(-1.742762+-0.132064094*c(0,5))  # for BAND_P
# inv_logit(-2.670291+0.121143313*c(0,5))  # for WUPA_S
