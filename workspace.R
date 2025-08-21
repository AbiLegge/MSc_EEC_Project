# -----------------------------------------------------------------------------#
# Ecosystem health 2025
#Abigail Legge
#Msc EEC
# -----------------------------------------------------------------------------#

#set working directory
setwd("/Users/abilegge/Documents/MSC_EEC/Dissertation/r_clean")
getwd()

#load packages
packages <- c("dplyr", "ggplot2", "tidyr", "FD", "BIEN", "vegan", "stringr", 
              "tibble", "lme4", "lmtest", "car", "rstatix",
              "emmeans", "MuMIn", "glmmTMB", "lmerTest", "performance")

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------------------------------------------------------#
#prepping data for analyis
# -----------------------------------------------------------------------------#

###############################################################################
#1. read in data
boothby2022 <- read.csv("surveys/boothby2022.csv", header = TRUE)
boothby2023 <- read.csv("surveys/boothby2023.csv")
boothby2024 <- read.csv("surveys/boothby2024.csv")
ennerdale2025 <- read.csv("surveys/ennerdale2025.csv")
HW20 <- read.csv("surveys/HW20.csv")
HW25 <- read.csv("surveys/HW25.csv")
HWE0 <- read.csv("surveys/HWEAST0.csv")
HWW0 <- read.csv("surveys/HWWEST0.csv")
HWN10 <- read.csv("surveys/HWNORTH10.csv")
HWN4 <- read.csv("surveys/HWNORTH4.csv")
HWS10 <- read.csv("surveys/HWSOUTH10.csv")
HWS4 <- read.csv("surveys/HWSOUTH4.csv")
#merge HW
hwater2024 <- bind_rows(HW25, HW20, HWN10, HWS10, HWN4,HWS4, HWE0, HWW0)
write.csv(hwater2024, "HWater_merged.csv") #saved as csv for easier loading if needed next time

knepp2022 <- read.csv("surveys/knepp2022.csv")
knepp2023 <- read.csv("surveys/knepp2023.csv")
knepp2024 <- read.csv("surveys/knepp2024.csv")
lowther2025 <- read.csv("surveys/lowther2025.csv")

############################################
#fix format, add _ to species and remove 0 cover
# List of dataset names
dataset_names <- c("boothby2022", "boothby2023", "boothby2024", 
                   "ennerdale2025", "hwater2024", "knepp2022", 
                   "knepp2023", "knepp2024", "lowther2025")


for (name in dataset_names) {
  
  dataset <- get(name)
  
  
  dataset$Species <- stringi::stri_trans_general(dataset$Species, "latin-ascii")
  dataset$Species <- gsub(" ", "_", dataset$Species, fixed = TRUE)
  dataset[is.na(dataset)] <- 0
  dataset <- dataset %>% 
    filter(Cover > 0) %>%
    filter(!grepl('\\*', Plot))
  
  
  assign(name, dataset)
}


###############################################################################
#check spellings
bo22spell <- read.csv("names/bo22_names.csv")
bo23spell <- read.csv("names/bo23_names.csv")
bo24spell <- read.csv("names/bo24_names.csv")
en25spell <- read.csv("names/en25_names.csv")
hw24spell <- read.csv("names/hw24_names.csv")
kn22spell <- read.csv("names/kn22_names.csv")
kn23spell <- read.csv("names/kn23_names.csv")
kn24spell <- read.csv("names/kn24_names.csv")
lo25spell <- read.csv("names/lo25_names.csv")

spell_objects <- c("bo22spell", "bo23spell", "bo24spell", "en25spell",
                   "hw24spell", "kn22spell", "kn23spell", "kn24spell", "lo25spell")

# spell check for each dataset
for(i in 1:length(dataset_names)) {
  current_data <- get(dataset_names[i])
  current_spell <- get(spell_objects[i])
  
  
  corrected_data <- current_data %>%
    left_join(current_spell, by = "Species") %>%
    mutate(Species = coalesce(Replace, Species)) %>%
    dplyr::select(-Replace)
  
  
  assign(dataset_names[i], corrected_data)
}



###############################################################################
#2. add native / introduced using WCVP
#will remove non plants too

#world checklist of vascular plants distribution data
wcvp <- read.table("data/wcvp_distribution.csv", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8")
head(wcvp_names)
#only species found in Great Britain
GB_wcvp <- wcvp %>% filter(area_code_l3 == "GRB")
#world checklist of vascular plants name data
wcvp_names <- read.table("data/wcvp_names.csv", sep="|", header=TRUE, quote = "", fill=TRUE, encoding = "UTF-8") 
#merge with distribution data to add species names using the plant id code
merged_wcvp <- merge(GB_wcvp, wcvp_names[, c("plant_name_id", "taxon_name")], by = "plant_name_id", all.x = TRUE)
#make sure it is in the right format
merged_wcvp$taxon_name <- stringi::stri_trans_general(merged_wcvp$taxon_name, "latin-ascii")
merged_wcvp$taxon_name <-gsub(" ", "_", merged_wcvp$taxon_name, fixed=TRUE)
#change the merged wcvp dataframe column to match species in all the observed data
names(merged_wcvp)[names(merged_wcvp) == "taxon_name"] <- "Species"
#write out
write.csv(merged_wcvp, "data/merged_wcvp.csv")

#merge with observation data
for (site in dataset_names) {
  dataset <- get(site)
  dataset <- dataset %>% 
    left_join(merged_wcvp %>% dplyr::select("Species", "plant_name_id", "introduced"), 
              by = "Species")
  assign(site, dataset)
}

#check NAs
boothby2022_na <- filter(boothby2022, if_any(everything(), is.na))
unique(boothby2022_na$Species)
boothby2023_na <- filter(boothby2023, if_any(everything(), is.na))
unique(boothby2023_na$Species)
boothby2024_na <- filter(boothby2024, if_any(everything(), is.na))
unique(boothby2024_na$Species) #only ones left are outside UK, 19 to be removed
hwater2024_na <- filter(hwater2024, if_any(everything(), is.na))
unique(hwater2024_na$Species)
knepp2022_na <- filter(knepp2022, if_any(everything(), is.na))
unique(knepp2022_na$Species)
knepp2023_na <- filter(knepp2023, if_any(everything(), is.na))
unique(knepp2023_na$Species)
knepp2024_na <- filter(knepp2024, if_any(everything(), is.na))
unique(knepp2024_na$Species)
lowther2025_na <- filter(lowther2025, if_any(everything(), is.na))
unique(lowther2025_na$Species)
ennerdale2025_na <- filter(ennerdale2025, if_any(everything(), is.na))
unique(ennerdale2025_na$Species)

#manually edit those with different naming systems
boothby2024$introduced[boothby2024$Species == "Brassica_nigra"] <- 0
boothby2024$location_doubtful[boothby2024$Species == "Brassica_nigra"] <- 0
boothby2024$plant_name_id[boothby2024$Species == "Brassica_nigra"] <- 2359243  

knepp2024$introduced[knepp2024$Species == "Brassica_nigra"] <- 0
knepp2024$location_doubtful[knepp2024$Species == "Brassica_nigra"] <- 0
knepp2024$plant_name_id[knepp2024$Species == "Brassica_nigra"] <- 2359243  

knepp2024$introduced[knepp2024$Species == "Potentilla_anserina"] <- 0
knepp2024$location_doubtful[knepp2024$Species == "Potentilla_anserina"] <- 0
knepp2024$plant_name_id[knepp2024$Species == "Potentilla_anserina"] <- 2941910  

knepp2023$introduced[knepp2023$Species == "Potentilla_anserina"] <- 0
knepp2023$location_doubtful[knepp2023$Species == "Potentilla_anserina"] <- 0
knepp2023$plant_name_id[knepp2023$Species == "Potentilla_anserina"] <- 2941910  

knepp2023$introduced[knepp2023$Species == "Stellaria_holostea"] <- 0
knepp2023$location_doubtful[knepp2023$Species == "Stellaria_holostea"] <- 0
knepp2023$plant_name_id[knepp2023$Species == "Stellaria_holostea"] <- 3246437  

knepp2023$introduced[knepp2023$Species == "Asperula_cynanchica"] <- 0
knepp2023$location_doubtful[knepp2023$Species == "Asperula_cynanchica"] <- 0
knepp2023$plant_name_id[knepp2023$Species == "Asperula_cynanchica"] <- 546926 

#remove species not found in the UK, non species entries and species only at genus level
bo_clean_22 <- boothby2022 %>%
  filter(!is.na(introduced))

bo_clean_23 <- boothby2023 %>%
  filter(!is.na(introduced))

bo_clean_24 <- boothby2024 %>%
  filter(!is.na(introduced))

en_clean_25 <- ennerdale2025 %>%
  filter(!is.na(introduced))

hw_clean_24 <- hwater2024 %>%
  filter(!is.na(introduced))

kn_clean_22 <- knepp2022 %>%
  filter(!is.na(introduced))

kn_clean_23 <- knepp2023 %>%
  filter(!is.na(introduced))

kn_clean_24 <- knepp2024 %>%
  filter(!is.na(introduced))

lo_clean_25 <- lowther2025 %>%
  filter(!is.na(introduced))

#save new files
write.csv(bo_clean_22, "clean/boothby22_clean.csv")
write.csv(bo_clean_23, "clean/boothby23_clean.csv")
write.csv(bo_clean_24, "clean/boothby24_clean.csv")
write.csv(hw_clean_24, "clean/haweswater24_clean.csv")
write.csv(kn_clean_22, "clean/knepp22_clean.csv")
write.csv(kn_clean_23, "clean/knepp23_clean.csv")
write.csv(kn_clean_24, "clean/knepp24_clean.csv")
write.csv(lo_clean_25, "clean/lowther25_clean.csv")
write.csv(en_clean_25, "clean/ennerdale25_clean.csv")

###############################################################################
#tidying up and making the same format to merge

#edit dataframes so they have the same column names and number
#edit boothby so columns match
bo_clean_22 <- rename(bo_clean_22, Fractal = Block)
bo_clean_23 <- rename(bo_clean_23, Fractal = Block)
bo_clean_24 <- rename(bo_clean_24, Fractal = Block)

#edit Haweswater so columns match
hw_clean_24 <- rename(hw_clean_24, Block = Years)


#merging columns in Knepp to create new fractal names using the group column
kn_clean_22 <- kn_clean_22 %>%
  mutate(Fractal = if_else(Group == "0", Block, paste0(Block, Group))
  )
kn_clean_23 <- kn_clean_23 %>%
  mutate(Fractal = if_else(Group == "0", Block, paste0(Block, Group))
  )
kn_clean_24 <- kn_clean_24 %>%
  mutate(Fractal = if_else(Group == "0", Block, paste0(Block, Group))
  )

#convert notes column to character
bo_clean_23$Notes <- as.character(bo_clean_23$Notes)
kn_clean_23$Notes <- as.character(kn_clean_23$Notes)

hw_clean_24$Block <- as.character(hw_clean_24$Block)

kn_clean_22$Plot <- as.integer(kn_clean_22$Plot)
bo_clean_22$Plot <- as.integer(bo_clean_22$Plot)

str(lo_clean_25)
str(en_clean_25)
str(bo_clean_22)

#make one large dataset
all_data <- bind_rows(bo_clean_22, bo_clean_23, bo_clean_24, hw_clean_24, 
                      kn_clean_22, kn_clean_23, kn_clean_24, lo_clean_25, en_clean_25)
#clean up
all_data <- all_data %>% dplyr::select(-c(Species..Nell., 
                                          what3words, location_doubtful, 
                                          survey, Group, Lat, Long, Notes,
                                          plant_name_id))

all_data$Date <- as.Date(all_data$Date, format = "%d/%m/%Y")
all_data <- all_data %>%
  mutate(year = format(Date, "%Y"))

#make groups of recorders
unique(all_data$Recorder)
#team effects
#haweswater teams - separate recording
all_data$Recorder[all_data$Recorder == "Saskia"] <- "Saskia Pearce"
all_data$Recorder[all_data$Recorder == "Saskia "] <- "Saskia Pearce"
#ennerdale / lowther - team recording
all_data$Recorder[all_data$Recorder == "Clara Barton, Abi Legge"] <- "AL_CB"
all_data$Recorder[all_data$Recorder == "Abi Legge, Clara Barton"] <- "AL_CB"
all_data$Recorder[all_data$Recorder == "Clara Barton"] <- "AL_CB"
all_data$Recorder[all_data$Recorder == "Abi Legge"] <- "AL_CB"

#2023 megan and rona - team effort
all_data$Recorder[all_data$Recorder == "Megan Sherlock, Rona Learmonth"] <- "RL_MS"
all_data$Recorder[all_data$Recorder == "Rona Learmonth, Megan Sherlock"] <- "RL_MS"
all_data$Recorder[all_data$Recorder == "Megan Sherlock"] <- "RL_MS"
all_data$Recorder[all_data$Recorder == "Rona Learmonth"] <- "RL_MS"

#knepp 2024 = el and bb
all_data$Recorder[all_data$Recorder == "Eimear Loughlin, Ben Bennett"] <- "BB_EL"
all_data$Recorder[all_data$Recorder == "Ben Bennett, Eimear Loughlin"] <- "BB_EL"
all_data$Recorder[all_data$Recorder == "Ben Bennett"] <- "BB_EL"
all_data$Recorder[all_data$Recorder == "Eimear Loughlin"] <- "BB_EL"


#aggregate quadrats into plots
all_data_plots <- all_data %>%
  group_by(year, Site, Block, Fractal, Recorder, introduced, Plot, Species) %>%
  summarise(
    total_cover = sum(Cover, na.rm = TRUE),
    introduced = first(introduced),
    .groups = "drop"
  ) %>%
  filter(total_cover > 0) 

all_data_plots <- all_data_plots %>%
  unite("plot_id", year, Site, Block, Fractal, Plot, sep = "_", na.rm = TRUE, remove = FALSE)

all_data_plots <- all_data_plots %>% 
  unite("fractal_id", year, Site, Block, Fractal, sep = "_", na.rm = TRUE, remove = FALSE)

#add management group
groups <- read.csv("data/groups.csv")

all_data_plots <- all_data_plots %>% left_join(groups, by = "fractal_id")

#save out
write.csv(all_data_plots, "Output_Data/all_data_plotlevel.csv")

# -----------------------------------------------------------------------------#
#2. Analysis
# -----------------------------------------------------------------------------#

###############################################################################
#basic metrics

# 1 Shannons diversity
shannon_diversity <- all_data_plots %>%
  group_by(year, Site, Block, Fractal, Plot, group, fractal_id, plot_id, Recorder) %>%
  summarise(shannon = diversity(total_cover), .groups = "drop")

write.csv(shannon_diversity, "Output_Data/shannons.csv")

#site level
shannon_diversity_site <- all_data_plots %>%
  group_by(year, Site) %>%
  summarise(shannon = diversity(total_cover), .groups = "drop")


shannon_diversity_site <- shannon_diversity_site %>%
  unite("site_id", year, Site, sep = "_", na.rm = TRUE, remove = FALSE)

#2 Proportion of native species

native_prop <- all_data_plots %>% 
  group_by(year, Site, Block, Fractal, Plot, group, fractal_id, plot_id, Recorder) %>%
  summarise(
    all_cover = sum(total_cover, na.rm = TRUE),
    native_cover = sum(total_cover[introduced == 0], na.rm = TRUE)
  ) %>%
  mutate(proportion_native = native_cover / all_cover)


write.csv(native_prop, "Output_Data/native_prop.csv")


#3 Species Richness
richness <- all_data_plots %>% 
  group_by(year, Site, Block, Fractal, Plot, group, fractal_id, plot_id, Recorder) %>%
  summarise(
    richness = n_distinct(Species)
  )


write.csv(richness, "Output_Data/richness.csv")

richness_site <- all_data_plots %>% 
  group_by(year, Site) %>%
  summarise(
    richness = n_distinct(Species)
  )

richness_site <- richness_site %>%
  unite("site_year", year, Site, sep = "_", na.rm = TRUE, remove = FALSE)


#merge data together

biodiversity_metrics <- shannon_diversity %>%
  left_join(native_prop, by = "plot_id") %>%
  left_join(richness, by = "plot_id")  %>%
  dplyr::select(Recorder, plot_id, year, Site, Block, Fractal, Plot, group, fractal_id, shannon, proportion_native, richness)

write.csv(biodiversity_metrics, "Output_Data/diversity_metrics.csv")


###############################################################################
# functional diversity

#1. get trait data
vignette("BIEN")

species_list <- all_data_plots %>% dplyr::select(Species)
species_list$Species <-gsub("_", " ", species_list$Species, fixed=TRUE)
species_list <- unique(species_list$Species)
traits_list <- c("leaf area", "leaf area per leaf dry mass", "maximum whole plant height", "seed mass")

#species level
bientraits_species <- BIEN_trait_traitbyspecies(species = species_list, trait = traits_list, 
                                        all.taxonomy = TRUE,
                                        political.boundaries = FALSE,
                                        source.citation = TRUE,)
#family and genus level
family_genus_SLA <- BIEN_trait_mean(species_list, "leaf area per leaf dry mass")
family_genus_leaf <- BIEN_trait_mean(species_list, "leaf area")
family_genus_height <- BIEN_trait_mean(species_list, "maximum whole plant height")
family_genus_seed <- BIEN_trait_mean(species_list, "seed mass")

SLA <-  family_genus_SLA %>% 
  dplyr::select(species, mean_value)
names(SLA)[2] <- "SLA"

leaf <- family_genus_leaf %>% 
  dplyr::select(species, mean_value)
names(leaf)[2] <- "leaf_area"

height <- family_genus_height %>% 
  dplyr::select(species, mean_value)
names(height)[2] <- "height"

seed <- family_genus_seed %>% 
  dplyr::select(species, mean_value)
names(seed)[2] <- "seed_mass"

#make one dataframe with all traits
all_traits <- SLA %>%
  full_join(leaf, by = "species") %>%
  full_join(height, by = "species") %>%
  full_join(seed, by = "species")


all_traits <- all_traits %>%
  mutate(across(c(SLA, height, leaf_area, seed_mass), as.numeric))

#check for nas, make sure all species have at least 2 traits
na_counts <- rowSums(is.na(all_traits))
species_with_2plus_nas <- names(na_counts[na_counts > 2])
species_with_2plus_nas

write.csv(all_traits, "Output_Data/all_traits.csv")


#2. get diversity metrics
abundance <- all_data_plots %>% 
  dplyr::select(plot_id, Species, total_cover) %>%
  tidyr::pivot_wider(
    names_from = Species,
    values_from = total_cover
  ) %>%
  tibble::column_to_rownames("plot_id") %>%
  replace(is.na(.), 0) %>%  # replace NAs with 0 after pivot_wider
  as.matrix()

traits <- data.frame(Species = colnames(abundance))
#add in traits from BIEN
names(all_traits)[1] <- "Species"
all_traits$Species <-gsub(" ", "_", all_traits$Species, fixed=TRUE)
traits <- traits %>% left_join(all_traits, by = "Species")
rownames(traits) <- traits$Species
traits <- traits %>% dplyr::select(!Species)

traits$seed_mass <- as.numeric(traits$seed_mass)

#check distributions
hist(traits$height)   # example trait
hist(traits$seed_mass)
hist(traits$leaf_area)
hist(traits$SLA)
#log transform skewed data 
traits$height <- log10(traits$height + 1)     
traits$seed_mass <- log10(traits$seed_mass + 1)
traits$leaf_area <- log10(traits$leaf_area + 1)

result <- dbFD(traits, abundance, calc.CWM = TRUE, corr = "cailliez")

names(result)
head(result$CWM)  # Community-weighted means
summary(result$FRic)  # Functional richness

functional_results <- data.frame(
  community = names(result$FRic),
  nbsp = result$nbsp,
  sing_sp = result$sing.sp,
  FRic = result$FRic,
  qual_FRic = result$qual.FRic,
  FEve = result$FEve,
  FDiv = result$FDiv,
  FDis = result$FDis,
  RaoQ = result$RaoQ
)

# Add CWM data
functional_results <- cbind(functional_results, result$CWM)

head(functional_results)
dim(functional_results)

write.csv(functional_results, "Output_Data/functional_results.csv")


#merge with other metrics
names(functional_results)[1] <- "plot_id"

all_metrics <- biodiversity_metrics %>%
  left_join(functional_results, by = "plot_id")

#change names to add cwm
all_metrics <- all_metrics %>% 
  rename(SLA_CWM = SLA) %>%
  rename(height_CWM = height) %>%
  rename(leaf_area_CWM = leaf_area) %>%
  rename(seed_mass_CWM = seed_mass)


write.csv(all_metrics, "Output_Data/all_metrics.csv")

# -----------------------------------------------------------------------------#
#3. descriptive stats and visualisation
# -----------------------------------------------------------------------------#

group_summary_stats <- all_metrics %>%
  group_by(year, group) %>%
  summarise(
    n_plots = n(),
    
    shannon_mean = round(mean(shannon, na.rm = TRUE), 3),
    shannon_sd = round(sd(shannon, na.rm = TRUE), 3),
    shannon_median = round(median(shannon, na.rm = TRUE), 3),
    shannon_iqr = round(IQR(shannon, na.rm = TRUE), 3),
    shannon_range = paste0(round(min(shannon, na.rm = TRUE), 3), " - ", round(max(shannon, na.rm = TRUE), 3)),
    
    richness_mean = round(mean(richness, na.rm = TRUE), 3),
    richness_sd = round(sd(richness, na.rm = TRUE), 3),
    richness_median = round(median(richness, na.rm = TRUE), 3),
    richness_iqr = round(IQR(richness, na.rm = TRUE), 3),
    richness_range = paste0(round(min(richness, na.rm = TRUE), 3), " - ", round(max(richness, na.rm = TRUE), 3)),
    
    FEve_mean = round(mean(FEve, na.rm = TRUE), 3),
    FEve_sd = round(sd(FEve, na.rm = TRUE), 3),
    FEve_median = round(median(FEve, na.rm = TRUE), 3),
    FEve_iqr = round(IQR(FEve, na.rm = TRUE), 3),
    FEve_range = paste0(round(min(FEve, na.rm = TRUE), 3), " - ", round(max(FEve, na.rm = TRUE), 3)),
    
    FDis_mean = round(mean(FDiv, na.rm = TRUE), 3),
    FDis_sd = round(sd(FDiv, na.rm = TRUE), 3),
    FDis_median = round(median(FDiv, na.rm = TRUE), 3),
    FDis_iqr = round(IQR(FDiv, na.rm = TRUE), 3),
    FDis_range = paste0(round(min(FDiv, na.rm = TRUE), 3), " - ", round(max(FDiv, na.rm = TRUE), 3)),
    
    Native_mean = round(mean(proportion_native, na.rm = TRUE), 3),
    Native_sd = round(sd(proportion_native, na.rm = TRUE), 3),
    Native_median = round(median(proportion_native, na.rm = TRUE), 3),
    Native_iqr = round(IQR(proportion_native, na.rm = TRUE), 3),
    Native_range = paste0(round(min(proportion_native, na.rm = TRUE), 3), " - ", round(max(proportion_native, na.rm = TRUE), 3)),
    
    .groups = 'drop'
  )


plot_summary_stats <- all_metrics %>%
  group_by(Site, year) %>%
  summarise(
    n_plots = n(),
    
    shannon_mean = round(mean(shannon, na.rm = TRUE), 3),
    shannon_sd = round(sd(shannon, na.rm = TRUE), 3),
    shannon_median = round(median(shannon, na.rm = TRUE), 3),
    shannon_iqr = round(IQR(shannon, na.rm = TRUE), 3),
    shannon_range = paste0(round(min(shannon, na.rm = TRUE), 3), " - ", round(max(shannon, na.rm = TRUE), 3)),
    
    richness_mean = round(mean(richness, na.rm = TRUE), 3),
    richness_sd = round(sd(richness, na.rm = TRUE), 3),
    richness_median = round(median(richness, na.rm = TRUE), 3),
    richness_iqr = round(IQR(richness, na.rm = TRUE), 3),
    richness_range = paste0(round(min(richness, na.rm = TRUE), 3), " - ", round(max(richness, na.rm = TRUE), 3)),
    
    FEve_mean = round(mean(FEve, na.rm = TRUE), 3),
    FEve_sd = round(sd(FEve, na.rm = TRUE), 3),
    FEve_median = round(median(FEve, na.rm = TRUE), 3),
    FEve_iqr = round(IQR(FEve, na.rm = TRUE), 3),
    FEve_range = paste0(round(min(FEve, na.rm = TRUE), 3), " - ", round(max(FEve, na.rm = TRUE), 3)),
    
    FDis_mean = round(mean(FDiv, na.rm = TRUE), 3),
    FDis_sd = round(sd(FDiv, na.rm = TRUE), 3),
    FDis_median = round(median(FDiv, na.rm = TRUE), 3),
    FDis_iqr = round(IQR(FDiv, na.rm = TRUE), 3),
    FDis_range = paste0(round(min(FDiv, na.rm = TRUE), 3), " - ", round(max(FDiv, na.rm = TRUE), 3)),
    
    Native_mean = round(mean(proportion_native, na.rm = TRUE), 3),
    Native_sd = round(sd(proportion_native, na.rm = TRUE), 3),
    Native_median = round(median(proportion_native, na.rm = TRUE), 3),
    Native_iqr = round(IQR(proportion_native, na.rm = TRUE), 3),
    Native_range = paste0(round(min(proportion_native, na.rm = TRUE), 3), " - ", round(max(proportion_native, na.rm = TRUE), 3)),
    
    .groups = 'drop'
  )

write.csv(group_summary_stats, "Output_Data/group_level_stats.csv")
write.csv(plot_summary_stats, "Output_Data/site_level_stats.csv")


###############################################################################
#boxplot


plot_level_long <- all_metrics %>%
  pivot_longer(cols = c(richness, shannon, proportion_native, FEve, FDis),
               names_to = "metric",
               values_to = "value") %>%
  mutate(year = factor(year))

plot_level_long$metric <- dplyr::recode(plot_level_long$metric,
                                        richness = "Species richness",
                                        shannon = "Shannon Diversity Index",
                                        proportion_native = "Proportion native species",
                                        FDis = "Functional dispersion",
                                        FEve = "Functional evenness",
)

plot_level_long <- plot_level_long %>%
  filter(!year == "2022") %>%
  filter(!year == "2023")

overview_p1 <- ggplot(plot_level_long, aes(x = Site, y = value, fill = Site)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  labs(x = "Site", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(overview_p1)

overview_groups <- ggplot(plot_level_long, aes(x = group, y = value, fill = Site)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  labs(x = "Site", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

print(overview_groups)

unique(all_metrics$Recorder)

# -----------------------------------------------------------------------------#
#4. statistical models
# -----------------------------------------------------------------------------#

#set Knepp Middle as reference
all_metrics$group <- factor(all_metrics$group)
all_metrics$group <- relevel(all_metrics$group, ref = "Knepp_M")

###############################################################################
#model fitting

#richness
lmm_richness <- lmer(richness ~ group + year + (1 | Recorder), data = all_metrics, REML = FALSE)
lm_richness <- lm(richness ~ group + year, data = all_metrics)
lmm_richness2 <- lmer(richness ~ group + (1|year) + (1 | Recorder), data = all_metrics, REML = FALSE)

anova(lmm_richness, lm_richness, lmm_richness2)

#shannon
lmm_shannon <- lmer(shannon ~ group + year + (1 | Recorder), data = all_metrics, REML = FALSE)
lm_shannon <- lm(shannon ~ group + year, data = all_metrics)
lmm_shannon2 <- lmer(shannon ~ group + (1|year) + (1 | Recorder), data = all_metrics, REML = FALSE)

anova(lmm_shannon, lm_shannon, lmm_shannon2)

#native
all_metrics$proportion_native_adj <- pmax(pmin(all_metrics$proportion_native, 0.999), 0.001)

m1_native <- glmmTMB(proportion_native_adj ~ group + year + (1|Recorder),
                     family = beta_family(link="logit"),
                     data = all_metrics)

m2_native <- glmmTMB(proportion_native_adj ~ group + year,
                     family = beta_family(link="logit"),
                     data = all_metrics)

m3_native <- glmmTMB(proportion_native_adj ~ group + (1|Recorder) + (1|year),
                     family = beta_family(link="logit"),
                     data = all_metrics)

anova(m1_native, m2_native, m3_native)

#fdis

lmm_FDis <- lmer(FDis ~ group + year + (1 | Recorder), data = all_metrics, REML = FALSE)
lm_FDis <- lm(FDis ~ group + year, data = all_metrics)
lmm_FDis2 <- lmer(FDis ~ group + (1|year) + (1 | Recorder), data = all_metrics, REML = FALSE)

anova(lmm_FDis, lm_FDis, lmm_FDis2)


#feve
m1_feve <- glmmTMB(FEve ~ group + year + (1|Recorder),
                     family = beta_family(link="logit"),
                     data = all_metrics)
m2_feve <- glmmTMB(FEve ~ group + year,
                   family = beta_family(link="logit"),
                   data = all_metrics)
m3_feve <- glmmTMB(FEve ~ group + (1|year) + (1| Recorder),
                   family = beta_family(link="logit"),
                   data = all_metrics)

anova(m1_feve, m2_feve, m3_feve)


###############################################################################
#final models
lm_richness <- lm(richness ~ group + year, data = all_metrics)

lmm_shannon <- lmer(shannon ~ group + year + (1 | Recorder), data = all_metrics, REML = FALSE)

m2_native <- glmmTMB(proportion_native_adj ~ group + year,
                     family = beta_family(link="logit"),
                     data = all_metrics)

lmm_FDis <- lmer(FDis ~ group + year + (1 | Recorder), data = all_metrics, REML = FALSE)

m2_feve <- glmmTMB(FEve ~ group + year,
                   family = beta_family(link="logit"),
                   data = all_metrics)

summary(lm_richness)
summary(lmm_shannon)
summary(lmm_FDis)
summary(m2_feve)
summary(m2_native)

###############################################################################
#model assumptions

#richness
par(mfrow = c(2, 2))
plot(lm_richness)

#shannon
par(mfrow = c(2, 2))
#Residuals vs Fitted
res <- resid(lmm_shannon)
fit <- fitted(lmm_shannon)

plot(fit, res,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# Q-Q
qqnorm(res)
qqline(res, col = "red")

#Scale-Location
plot(fit, sqrt(abs(res)),
     main = "Scale-Location",
     xlab = "Fitted values",
     ylab = "Sqrt(|Residuals|)")
abline(h = 0, col = "red")

#native
par(mfrow = c(2, 2))
sim_residuals <- simulateResiduals(m2_native)
#qq plot
plot(sim_residuals)  

# overdispersion
testDispersion(sim_residuals)

#residuals by group
plotResiduals(sim_residuals, form = all_metrics$group)


#fdis
#Residuals vs Fitted
par(mfrow = c(2, 2))
res <- resid(lmm_FDis)
fit <- fitted(lmm_FDis)

plot(fit, res,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# 2. Normal Q-Q
qqnorm(res)
qqline(res, col = "red")

# 3. Scale-Location
plot(fit, sqrt(abs(res)),
     main = "Scale-Location",
     xlab = "Fitted values",
     ylab = "Sqrt(|Residuals|)")
abline(h = 0, col = "red")

#feve
# 1. Residuals vs Fitted
par(mfrow = c(2, 2))
sim_residuals <- simulateResiduals(m2_feve)
#qq plot
plot(sim_residuals)  

# overdispersion
testDispersion(sim_residuals)

#residuals by group
plotResiduals(sim_residuals, form = all_metrics$group)



###############################################################################
#testing between metrics
#site effects
anova(lmm_shannon)
summary(lm_richness)    
anova(lmm_FDis)    
Anova(m2_native, type = "II") 
Anova(m2_feve, type = "II") 

#marginal r2
r2(m2_native)
r2(m2_feve)
r2(lmm_shannon)
r2(lmm_richness)
r2(lmm_FDis)



