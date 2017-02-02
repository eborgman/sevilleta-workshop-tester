library(shiny)
library(shinyBS)
library(shinythemes)


fluidPage(
  theme=shinytheme('flatly'),
  titlePanel('Survey design'),
  fluidRow(
    tabsetPanel(type='tabs',
                tabPanel('Layout', plotOutput('layout')),
                tabPanel('Data', uiOutput('data'))
    ),
    hr(),
    column(5,
           h3('Panels'),
           tags$hr(style="border-color: #95a5a6;"),
           tags$div(id='input0',
                    sliderInput("survey_period", label=h4("Survey period"), min=2005,
                                max=2017, value=c(2008, 2016), step=1, sep=''),
                    h4('Revisit design'),
                    panelInput('panelInput0'),
                    bsButton('addInput', label='', icon=icon('plus', lib='glyphicon'),
                             size='extra-small', style='success')),
           tags$div(id='placeholderInput')
    ),
    column(6, offset=1,
           h3('Simulation'),
           tags$hr(style="border-color: #95a5a6;"),
           radioButtons('dist', h4('Probability distribution of the outcome'),
                        c('Normal'='norm',
                          'Lognormal'='lnorm',
                          'Poisson'='pois',
                          'Binomial'='binom'), inline=TRUE),
           h4('Hyperparameters'),
           'For the linear mixed-effects model',
           withMathJax("$$\\begin{align}
                       Y_{ij}&=\\beta_{0 j}+\\beta_1X_{1ij}+\\epsilon_{0ij}\\text{, where}\\\\
                       \\beta_{0j}&=\\beta_{0}+u_{0j} \\\\
                       u_{0j}&=N(0, \\sigma^2_{u_0}) \\\\
                       \\epsilon_{0ij}&=N(0, \\sigma^2_{\\epsilon_0})
                       \\end{align}$$"),
           hyperparameterInput('hyperparamInput')
           )
    )
  )