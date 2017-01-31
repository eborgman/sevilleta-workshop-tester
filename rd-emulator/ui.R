library(shiny)
library(shinyBS)
library(shinythemes)


fluidPage(
  theme=shinytheme('flatly'),
  titlePanel('Survey design'),
  sidebarLayout(
    # Sidebar panel.
    sidebarPanel(width=5,
      h4('PANELS'),
      tags$div(id='input0',
               sliderInput("survey_period", label = "Survey period", min=2005,
                           max=2017, value=c(2008, 2016), step=1, sep=''),
               panelInput('panelInput0'),
               bsButton('addInput', label='', icon=icon('plus', lib='glyphicon'),
                        size='extra-small', style='success')
      ),
      tags$div(id = 'placeholderInput'),
      h4('OUTCOME'),
      radioButtons('dist', 'Probability distribution',
                   c('Normal'='norm',
                     'Log-normal'='lnorm')),
      h4('HYPERPARAMETERS'),
      'For the linear mixed-effects model',
      withMathJax("$$\\begin{align}
                  Y_{ij}&=\\beta_{0 j}+\\beta_1X_{1ij}+\\epsilon_{0ij}\\text{, where}\\\\
                  \\beta_{0j}&=\\beta_{0}+u_{0j} \\\\
                  u_{0j}&=N(0, \\sigma^2_{u_0}) \\\\
                  \\epsilon_{0ij}&=N(0, \\sigma^2_{\\epsilon_0})
                  \\end{align}$$"),
      hyperparametersInput('hyperparamsInput'),
      # hr(),
      downloadLink("downloadData", "Download")
    ),
    # Main panel.
    mainPanel(width=7,
      tabsetPanel(type='tabs',
                  tabPanel('Plot', plotOutput('plot')),
                  tabPanel('Table', uiOutput('table'))
      )
    )
  )
)