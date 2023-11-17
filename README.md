# VT_proportional_MA
VT_proportional_MA_and_world_map 
#This script can be used to perfrom a proportional meta-analysis in R studio. 
#IT was used to calculate the proportion of strain sharing events derived from bifidobacteria in realtion to the total in mother-infant dyads.

install.packages(meta)
library(meta)

#In this case I needed x6 different meta-analysis to figure out the contribution of each one of the species. 
#x6 meta-analysis for total and each of the species separately

MA_Bibidobacterium_16_11_2023
MA_pseudo_16_11_2023
MA_adoles_16_11_2023
MA_longum_16_11_2023
MA_breve_16_11_2023
MA_bifis_16_11_2023

#Here change MA_pseudo_16_11_2023 for the file you want to work with

MA_data <- MA_pseudo_16_11_2023
MA_data

#Look at data loaded as “MA_data”

str(MA_data)

# Option 1 is an MA without sugroupong, option2 will subgroup by taxonomy

# Conducting proportional MA without subgrouping (option 1)

MA_data_results_MA <- metaprop(MA_data$B.spp_TE, MA_data$Total_Bacteria_TE, studlab=MA_data$Reference, sm="PFT", data=MA_data, method="Inverse", method.tau="DL", random = TRUE, fixed =FALSE)

summary(MA_data_results_MA)


# Conducting proportion MA with Random effects model and subgrouping by Bifidum (option 2)
subgroup_results <- metaprop(MA_data$B.spp_TE, MA_data$Total_B._TE, studlab = MA_data$Reference, sm = "PFT", data = MA_data, method = "Inverse", method.tau = "DL", random = TRUE, fixed = FALSE, byvar = MA_data$Taxonomy)

# Summary effect measure obtained from random effect model only
summary(MA_data_results_MA) #or
print(subgroup_results)

# View the forestplot the subgroup results per taxonomies of proportions of bifidobacterium per total**
forest.meta(subgroup_results, layout="RevMan5", xlab="Proportion B.spp TE - total Bibidobacterium TE", comb.fixed=FALSE, comb.random=TRUE, xlim = c(0,1), fontsize=10, digits=3)


------------------------------------------------------------------------------------------
# Sometimes you might have non-positive values in your continous variable, if that is the case, perform this code and then code back to either option 1 or option 2
  # Check for non-positive values in MA_data$Total_B._TE
  negative_values <- MA_data$Total_B._TE <= 0
str(negative_values)
# If there are negative values, you can choose to remove those rows or handle them as needed
if (any(negative_values)) {
# Option 1: Remove rows with non-positive values
MA_data <- MA_data[!negative_values, ]
  
# Option 2: Replace non-positive values with NA or a specific value
MA_data$Total_B._TE[negative_values] <- NA
}
