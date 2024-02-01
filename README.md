# VT_proportional_MA

#install packages
install.packages("meta")
library(meta)

# Conducting proportion subgrouped MA with Random effects model and subgrouping by species 
MA_data_results_MA <- metaprop(MA_data$`N_pairs_shared species`, MA_data$'N_pairs_shared species', MA_data$Reference, sm="PFT", data=MA_data, method="Inverse", method.tau="DL", random = TRUE, fixed =FALSE)

summary(MA_data_results_MA)

# Summary effect measure obtained from random effect model only (visualization)
forest(MA_data_results_MA, layout="RevMan5", xlab="Proportion", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)
