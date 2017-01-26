# The membership design specifies which population units ('samples') are members
# of which panels. Panels are groups of samples visited on the same occassion.

# panel members be chosen using independent simple random sampling

# library(dplyr)


PANELS <- # groups of population units sampled the same occasion
# N_SAMPLES <- 76
# N_YEARS <- 4
# START_YEAR <- 2008
# D1 <- 1
# D2 <- 0
#
# panel
#
# df <- expand.grid(sample_id=seq(1, N_SAMPLES),
#                   year=seq(START_YEAR, START_YEAR+N_YEARS))

REVISITATION_INTERVAL <- 2
seq(0, REVISITATION_INTERVAL)
seq(0, 10) %% 3
