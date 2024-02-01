#16_11_2023
install.packages("meta")
library(meta)
#subgrouped random effect model meta-analysis by species

MA_data <- Book_1

# Conducting proportion MA with Random effects model and subgrouping by Bifidum 
MA_data_results_MA <- metaprop(MA_data$`N_pairs_shared species`, MA_data$'N_pairs_shared species', MA_data$Reference, sm="PFT", data=MA_data, method="Inverse", method.tau="DL", random = TRUE, fixed =FALSE)

summary(MA_data_results_MA)

# Summary effect measure obtained from random effect model only
forest(MA_data_results_MA, layout="RevMan5", xlab="Proportion", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)

----------------------------------------------------------------------------------------
#for ach one of the species
subgroup_results <- metaprop(MA_data$B.spp_TE, MA_data$Total_B._TE, studlab = MA_data$Reference, sm = "PFT", data = MA_data, method = "Inverse", method.tau = "DL", random = TRUE, fixed = FALSE)
# Print or view the subgroup results per taxonomies of proportions of bifidobacterium per total
print(subgroup_results)

forest.meta(subgroup_results, layout="RevMan5", xlab="Proportion B. bifidum TE - total Bibidobacterium TE", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)


