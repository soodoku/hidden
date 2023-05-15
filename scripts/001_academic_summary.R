## Summarize Academic Polls

setwd(basedir)
setwd("hidden/")

# Load libs
library(readxl)

anes_2012 <- read_excel("data/academic_polls/anes_2012/anes_2012_clean.xls")

# Rename cols.

# Average number of options
# LC placement can't be coded as having 7 options
mean(as.numeric(anes_2012$`Number of options (if closed)`), na.rm = T)

# Proportion with explicit DK
mean(as.numeric(anes_2012$`Explicit DK option?`), na.rm = T)

# DK possibility
mean(as.numeric(anes_2012$`DK possibility`), na.rm = T)

# Matter of opinion
mean(as.numeric(anes_2012$`Disguised as matter of opinion or not`), na.rm = T)
