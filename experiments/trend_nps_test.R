library(dplyr)
library(ggplot2)


dat <-
  read.csv(file.path(PROJ_ROOT, 'data', 'ROMN',
                     'GRKO_AdjWgts_TrendNPS_SelectMetrics_WithPanels_r.csv')) %>%
  select(Site=SiteName, Year, WYear=Wyear, EvalStatus, panel, wgt, Mgmt,
         adjwt_Wyear, adjwt_panel, NativeRich)

# Year specific means
means_df <- dat %>%
  group_by(Year, WYear) %>%
  summarise(simple_mean=mean(NativeRich),
            ht_mean=sum(NativeRich*adjwt_Wyear)/sum(adjwt_Wyear))

new_native_rich <- dat %>%
  group_by(panel, Site) %>%
  summarise(NewNativeRich=mean(NativeRich)) %>%
  as.data.frame
new_native_rich %>%
  left_join(dat %>% select(panel, Site , adjwt_panel) %>% distinct) %>%
  group_by(panel) %>%
  summarise(simple_mean=mean(NewNativeRich),
            ht_mean=sum(NewNativeRich*adjwt_panel)/sum(adjwt_panel))

m1 <- lme4::glmer(NativeRich~WYear+(1|Site), data=dat, family=poisson)
means_df %>% left_join(
  data.frame(WYear=unique(dat$WYear), Site='A') %>%
    mutate(glmer_mean=predict(m1, newdata=.,
                              allow.new.levels=TRUE, type='response', re.form=NA)) %>%
    arrange(WYear)
)

summary(lme4::glmer(NativeRich~WYear+(1|Site), data=dat, family=poisson))
m2 <- lme4::glmer(NativeRich~WYear+factor(Mgmt)+(1|Site), data=dat, family=poisson)
tmp <- expand.grid(WYear=unique(dat$WYear), Mgmt=unique(dat$Mgmt), Site=NA) %>%
  mutate(glmer_mean=predict(m2, newdata=.,
                            allow.new.levels=TRUE, type='response', re.form=NA))

ggplot(tmp, aes(x=WYear, y=glmer_mean, group=Mgmt, color=factor(Mgmt))) +
  geom_line() + geom_point()

# lme4::bootMer(m2, nsim=1)
# (b0_ci <- boot::boot.ci(m2, index=1, type=c("norm", "basic", "perc")))# beta

# dat %>% arrange(Site) %>% tbl_df
ggplot(dat,
       aes(x=Year, y=factor(Site), color=factor(Site))) +
  geom_point(alpha=1, size=3)
ggplot(dat,
       aes(x=Year, y=NativeRich, group=Site)) +
  geom_line(aes(color=factor(Mgmt)))

tmp <- lapply(unique(dat$Year), function(YYYY) {
  dat %>%
    filter(Year==YYYY) %>%
    mutate(adjwgt=TrendNPS::AdjWgt(., popn='Target'))
})
tmp %>% bind_rows



# my_fun(dat, popn='Target')
