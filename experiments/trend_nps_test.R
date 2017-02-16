library(dplyr)
library(ggplot2)

# read in data (set your working directory first)
# this data set includes SiteName, Year, WYear (relative year, first sampled year is 0),
# chunk (sampling period over which all sites are visited), wgt (initial weight from GRTS draw), 
# Mgmt (management action where 1=irrigated, 2=non-irrigated, 3=other), 
# adjwt_Wyear (weight adjusted using TrendNPS for each year), adjwt_chunk (adjusted weight for each chunk),
# NativeRich (native species richness - the number of native plant species found)
dat <-
  read.csv('GRKO_AdjWgts_TrendNPS_SelectMetrics_WithPanels_r.csv') %>%
  select(Site=SiteName, Year, WYear=Wyear, EvalStatus, chunk=panel, wgt, Mgmt,
         adjwt_Wyear, adjwt_chunk=adjwt_panel, NativeRich)

# create new dataframe (means_df) with year specific means, unweighted (simple_mean) and Horvitz-Thompson weight-adjusted mean (ht_mean) 
means_df <- dat %>%
  group_by(Year, WYear) %>%
  summarise(simple_mean=mean(NativeRich),
            ht_mean=sum(NativeRich*adjwt_Wyear)/sum(adjwt_Wyear))

# create new dataframe (new_native_rich) with mean of NativeRich for each site
# (get mean for sites revisted over the sampling period over which all sites are visited (chunk))
new_native_rich <- dat %>%
  group_by(chunk, Site) %>%
  summarise(NewNativeRich=mean(NativeRich)) %>%
  as.data.frame

# use new_native_rich to compute chunk simple_mean and ht_mean for time period 0 (first 4 years of sampling) and time period 1 (second 4 years)
new_native_rich %>%
  left_join(dat %>% select(chunk, Site , adjwt_chunk) %>% distinct) %>%
  group_by(chunk) %>%
  summarise(simple_mean=mean(NewNativeRich),
            ht_mean=sum(NewNativeRich*adjwt_chunk)/sum(adjwt_chunk))

# model with response NativeRich, fixed effect relative year, covariate management group, random effect site
m1 <- lme4::glmer(NativeRich~WYear+factor(Mgmt)+(1|Site), data=dat, family=poisson)
# create new dataframe (tmp) to look at all combinations of unique year and unique managment actions and the modeled predicted mean for each
tmp <- expand.grid(WYear=unique(dat$WYear), Mgmt=unique(dat$Mgmt)) %>%
  mutate(glmer_mean=predict(m1, newdata=.,
                            allow.new.levels=TRUE, type='response', re.form=NA))

# plot modeled mean for each year and each management group
ggplot(tmp, aes(x=WYear, y=glmer_mean, group=Mgmt, color=factor(Mgmt))) +
  geom_line() + geom_point()

# visualize panel design
ggplot(dat,
       aes(x=Year, y=factor(Site), color=factor(Site))) +
  geom_point(alpha=1, size=3)

# visualize raw data by each and mgmt group
ggplot(dat,
       aes(x=Year, y=NativeRich, group=Site)) +
  geom_line(aes(color=factor(Mgmt)))