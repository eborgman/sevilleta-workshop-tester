library(dplyr)


data_dir <- file.path(PROJ_ROOT, 'data', 'SCPN')
fig_dir <- file.path(PROJ_ROOT, 'data', 'SCPN', 'figs')
diversity <- read.csv (file.path(data_dir, 'Diversity.csv'))

make_plot <- function(x, figs_root=fig_dir) {
  # if(x$EcoSite[1]=='BAND_M') browser()
  fig_name <- file.path(figs_root, paste0(x$EcoSite[1], '.png'))
  ggplot(x, aes(x=EventYear, y=factor(Plot), color=factor(Plot))) +
    theme(legend.position='none') +
    geom_point(alpha=1, size=3)
  ggsave(fig_name, width=6, height=4, units = "in")
  data.frame(success=TRUE)
}
# make_plot(x=diversity %>% filter(EcoSite=='CHCU_S'))  # the 'long way'

diversity %>%
  group_by(EcoSite) %>%
  do(make_plot(.))

# my_sum <- function(danas_arg) {
#   # browser()
#   danas_arg + 1
#   }
# my_sum(danas_arg=5)  # remember: Q quits a browser()
