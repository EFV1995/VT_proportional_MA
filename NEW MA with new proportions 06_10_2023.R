#16_11_2023
library(meta)
#x6 meta-analysis for total and each of the species separately

MA_Bibidobacterium_16_11_2023
MA_pseudo_16_11_2023
MA_adoles_16_11_2023
MA_longum_16_11_2023
MA_breve_16_11_2023
MA_bifis_16_11_2023

MA_data <- MA_pseudo_16_11_2023
MA_data
#Data cleaning
#Look at data loaded as “MA_data”
MA_data
glimpse(MA_data)

#Replace the variable names in the dataset

names(MA_data) <- c("ID", "StudyName", "n[mother-infantpairs]", "B.spp_TE_Total_TE", "B.spp_TE_Total B. TE", "B.spp_TE_Sample size", "Persistance","TaxonomicProfiling", "MaternalOrigin", "Taxonomy")

glimpse(MA_data)

str(MA_data)

summary(MA_data)

# Conducting proportional MA without subgroup


MA_data_results_MA <- metaprop(MA_data$B.spp_TE, MA_data$Total_Bacteria_TE, studlab=MA_data$Reference, sm="PFT", data=MA_data, method="Inverse", method.tau="DL", random = TRUE, fixed =FALSE)

summary(MA_data_results_MA)


# Conducting proportion MA with Random effects model and subgrouping by Bifidum 
MA_data_results_MA <- metaprop(MA_data$B.spp_TE, MA_data$Total_Bacteria_TE, studlab=MA_data$Reference, sm="PFT", data=MA_data, method="Inverse", method.tau="DL", random = TRUE, fixed =FALSE)

summary(MA_data_results_MA)

# Summary effect measure obtained from random effect model only
forest.meta(MA_data_results_MA, layout="RevMan5", xlab="Proportion", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)



------------------------------------------------------------------------------------------
  # Check for non-positive values in MA_data$Total_B._TE
  negative_values <- MA_data$Total_B._TE <= 0
str(negative_values)
# If there are negative values, you can choose to remove those rows or handle them as needed
if (any(negative_values)) {
  # Option 1: Remove rows with non-positive values
  MA_data <- MA_data[!negative_values, ]
  
  # Option 2: Replace non-positive values with NA or a specific value
  # MA_data$Total_B._TE[negative_values] <- NA
}

# Now, you can run the metaprop function with the cleaned data
MA_data_results_MA <- metaprop(MA_data$B.spp_TE, MA_data$Total_B._TE, studlab = MA_data$Reference, sm = "PFT", data = MA_data, method = "Inverse", method.tau = "DL", random = TRUE, fixed = FALSE)
forest.meta(MA_data_results_MA, layout="RevMan5", xlab="Proportion B.spp TE - total Bacterial TE", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)


# Conduct subgroup meta-analysis by "taxonomy"
subgroup_results <- metaprop(MA_data$B.spp_TE, MA_data$Total_B._TE, studlab = MA_data$Reference, sm = "PFT", data = MA_data, method = "Inverse", method.tau = "DL", random = TRUE, fixed = FALSE, byvar = MA_data$Taxonomy)

# Print or view the subgroup results per taxonomies of proportions of bifidobacterium per total
print(subgroup_results)


forest.meta(subgroup_results, layout="RevMan5", xlab="Proportion B.spp TE - total Bibidobacterium TE", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)



----------------------------------------------------------------------------------------
#for ach one of the species
subgroup_results <- metaprop(MA_data$B.spp_TE, MA_data$Total_B._TE, studlab = MA_data$Reference, sm = "PFT", data = MA_data, method = "Inverse", method.tau = "DL", random = TRUE, fixed = FALSE)
# Print or view the subgroup results per taxonomies of proportions of bifidobacterium per total
print(subgroup_results)

forest.meta(subgroup_results, layout="RevMan5", xlab="Proportion B. bifidum TE - total Bibidobacterium TE", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)


