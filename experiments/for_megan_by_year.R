library(dplyr)
library(ggplot2)


data_dir <- file.path(PROJ_ROOT, 'data', 'SCPN')
fig_dir <- file.path(PROJ_ROOT, 'data', 'SCPN', 'figs')
scpn_data <- read.csv (file.path(data_dir, 'FctGrp_Plot.csv'))

make_plot_by_year <- function(x, functional_grp, figs_root=fig_dir) {
  # browser()
  fig_name <- file.path(figs_root, paste0(x$EcoSite[1], '_by_year.png'))
  ggplot(x %>% filter(FunctionalGroup==functional_grp) ,
         aes(x=EventYear, y=CoverClassMidpoint_Plot_mn, color=factor(Plot),
             group=factor(Plot))) +
    theme(legend.position='none') +
    geom_line() +
    geom_point(alpha=1, size=3)
  ggsave(fig_name, width=6, height=4, units = "in")
  data.frame(success=TRUE)
}
# make_plot(x=scpn_data %>% filter(EcoSite=='CHCU_S'))  # the 'long way'

scpn_data %>%
  group_by(EcoSite) %>%
  do(make_plot_by_year(., 'TotalLiveFoliarCoverClass_10m'))

# my_sum <- function(danas_arg) {
#   # browser()
#   danas_arg + 1
#   }
# my_sum(danas_arg=5)  # remember: Q quits a browser()
