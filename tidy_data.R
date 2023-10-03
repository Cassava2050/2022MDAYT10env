rm(list = ls())

source("https://raw.githubusercontent.com/Cassava2050/PPD/main/utilities_tidy.R")

## Load the files to check

local_file <- "yes" #

if (local_file == "yes") {
  folder <- here::here("data//")  
  file <- "phenotype.csv"
  skip_col <- 3 # double check the number of col skipped
  trial_interest = "MDAYT"
  year_interest <- 2022
}

# 1) load the data
sel_data <- read_cassavabase(phenotypeFile = paste0(folder, file))


# ---- Change columns into standar names ----
sel_data_kp <- change_colname(sel_data, NA)


## change the column class

obs_col <- c(
  names(sel_data_kp)[str_detect(names(sel_data_kp), "obs_")],
  "use_rep_number", "blockNumber",
  "use_plot_number", "use_plot_width",
  "use_plot_length"
)
sel_data_kp %<>%
  mutate(across(all_of(obs_col), as.numeric))


# remove - , replace by _
names(sel_data_kp) = gsub("-", "_", names(sel_data_kp))

## Duplications in row and cols
duplicated_plot <- row_col_dup(sel_data_kp)

## Plot trial layout
trial_layout(sel_data_kp)


## Check the clone name
cloneName_new_old <- check_clone_name(
  clone_list = sel_data_kp$use_accession_name,
  new_names = NA,
  add_check = NULL
)

trial_standard <- sel_data_kp %>%
  left_join(cloneName_new_old,
            by = c("use_accession_name" = "accession_name_ori")
  ) %>%
  select(-use_accession_name) %>%
  rename(use_accession_name = use_accession_name.y)

## Add GIS data
trial_standard <- add_GIS(trial_standard)

# Get world map data for reference
world <- map_data("world")

# Create a data frame with your locations
locations <- data.frame(
  Location = c("Phu Yen", "Son La", "Quang Ngai", "Tay Ninh", "Dong Nai", "Daklak"),
  Latitude = c(13.0892, 21.3255, 15.1219, 11.3009, 11.1432, 12.6667),
  Longitude = c(109.1193, 103.7633, 108.8029, 106.1107, 107.2742, 108.0500)
)

# Create a ggplot object for the map
static_map <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), 
               fill = "lightgray", col = "black", linewidth = 0.03) +
  geom_point(data = locations, aes(x = Longitude, y = Latitude), size = 2, color = "red") +
  geom_text_repel(data = locations, aes(x = Longitude, y = Latitude, label = Location), 
            vjust = -0.5, size = 3) +
  geom_text(aes(x = 109, y = 9, label = "Author: Luis Fdo. Delgado"), 
            size = 2, fontface = "bold", color = "black") +
  geom_text(data = NULL, aes(x = 105, y = 22, label = "Vietnam"), size = 3, 
            fontface = "bold.italic") +
  geom_text(data = NULL, aes(x = 102, y = 20, label = "Laos"), size = 3, 
            fontface = "bold.italic") +
  geom_text(data = NULL, aes(x = 105, y = 14, label = "Cambodia"), size = 3, 
            fontface = "bold.italic") +
  geom_text(data = NULL, aes(x = 102, y = 15, label = "Thailand"), size = 3, 
            fontface = "bold.italic") +
  labs(title = "Locations in Vietnam", fontface = "bold.italic") +
  theme(plot.title = element_text(face = "bold.italic")) +
  #theme_void() +
  coord_cartesian(xlim = c(90, 120), ylim = c(8, 25))
  
# Display the map
print(static_map)

ggsave(paste("images\\map", trial_interest, ".png", sep = "_"),
       plot = static_map, units = "in", dpi = 300, width = 6, height = 5
)
# extract checks


accession_rep_ct <- trial_standard %>%
  count(use_trial_name, use_accession_name, use_rep_number)  %>%
  arrange(use_trial_name) %>%
  filter(n>1)
accession_rep_ct 


conducted_trials <- 
  trial_standard %>% group_by(use_trial_name, use_plant_date,use_harvest_date, use_location) %>% 
  summarise(n_gen = n_distinct(use_accession_name)) %>% 
  mutate(harvesting_time = 
           interval(ymd(use_plant_date), ymd(use_harvest_date)) %>% as.period,
         harvesting_time = paste0(harvesting_time@month, "month ", harvesting_time@day, "day")) %>% 
  ungroup()

conducted_trials

conducted_trials %>% relocate(harvesting_time, .after = use_harvest_date) %>% 
  write.table("clipboard", sep="\t", col.names = T, row.names = F)

## plot plant number

plants_plot <- trial_standard %>%
  group_by(use_trial_name) %>%
  count(obs_planted_number_plot) 
plants_plot


## Frequency harvest plant number

plants_harvested <- trial_standard %>%
  group_by(use_trial_name) %>%
  count(obs_harvest_number) %>% arrange(desc(obs_harvest_number))

# planted and harvested
plants_plot %>% select(-n) %>% 
  left_join(plants_harvested %>% 
              summarise(harvested_plants = max(obs_harvest_number)), by = "use_trial_name") %>% 
  write.table("clipboard", sep="\t", col.names = T, row.names = F)


# I dont think I am able to calculate yield_V2

plants_to_harvest <- plants_harvested %>% 
  ggplot(aes(x = factor(obs_harvest_number), 
             y = n, fill = factor(obs_harvest_number))) +
  geom_col(col = 'black') +
  #scale_fill_jco() +
  theme_xiaofei() +
  theme(legend.position="top") +
  theme(
    axis.text.x = element_text(size = 5, vjust = 1, angle = 65))+
  labs(x = "Harvest_plant_number", y = "Freq", fill = "Harvest_plant_number") +
  facet_wrap(~ use_trial_name)

ggsave(paste("images\\bar", trial_interest, Sys.Date(), ".png", sep = "_"),
       plot = plants_to_harvest, units = "in", dpi = 300, width = 9, height = 6)

## Is numeric all traits?

is_numeric(trial_data = trial_standard)


## Get the tidy data

meta_info = names(trial_standard )[str_detect(names(trial_standard), "use_")]
meta_info = gsub("use_", "", meta_info)
meta_info
trial_tidy = trial_standard
names(trial_tidy)= gsub("use_", "", names(trial_standard))
# observations
trait_list = names(trial_tidy)[str_detect(names(trial_tidy), "obs_")]
trait_list = gsub("obs_", "", trait_list)
trait_list
names(trial_tidy)= gsub("obs_", "", names(trial_tidy))
trial_tidy = trial_tidy[c(meta_info, trait_list)]

# Boxplots


# remove columns with all NA
my_dat_noNA <- trial_tidy[, colSums(is.na(trial_tidy)) < nrow(trial_tidy)]
trait_wanted <- names(my_dat_noNA)[names(my_dat_noNA) %in% trait_list]
for (i in 1:length(trait_wanted)) {
  y_DATA <- my_dat_noNA[[trait_wanted[i]]] # data frame or vector?
  x_DATA <- my_dat_noNA$trial_name
  my_DATA <- my_dat_noNA
  y_LABEL <- trait_wanted[i]
  x_LABEL <- NULL
  TITLE <- NULL
  y_MAX <- max(y_DATA, na.rm = TRUE) * 1.2
  y_MIN <- 0
  plot_box <- ggplot(my_DATA, aes(x = x_DATA, y = y_DATA)) +
    geom_violin(trim = FALSE, fill = "gray") +
    geom_boxplot(width = 0.2) +
    coord_cartesian(ylim = c(y_MIN, y_MAX)) +
    theme_xiaofei() +
    labs(
      y = y_LABEL, x = x_LABEL,
      title = TITLE
    )
  plot(plot_box)
}

# coalesce CMD trait taken at different times (10, 12, harvest)
trial_tidy_1 <- trial_tidy %>%
  mutate(
    CMD_harvest =
      coalesce(CMD_harvest, CMD_10mon), 
    CMD_harvest=
      coalesce(CMD_harvest, CMD_9mon),
    CMD_harvest=
      coalesce(CMD_harvest, CMD_12mon)) 

# convert zero values into NA (yield, DM_gravity)
trial_tidy_1 <- trial_tidy_1 %>% 
  mutate(yield_ha = ifelse(yield_ha == 0, NA, yield_ha), 
         DM_gravity = ifelse(DM_gravity == 0, NA, DM_gravity)) 

# Calculate starch_yield_ha
trial_tidy_1 <- trial_tidy_1 %>% 
mutate(starch_yield_ha = starch_content * yield_ha / 100,
       DM_yield_ha = DM_gravity * yield_ha / 100)


## Grouping boxplot
trait_wanted <- c(trait_wanted, "starch_yield_ha", "DM_yield_ha")
plot_bxp <- trial_tidy_1 %>%
  pivot_longer(
    cols = all_of(trait_wanted),
    names_to = "var",
    values_to = "values"
  ) %>%
  filter(!var %in% c(
    "stake_plant", "planted_number_plot",
    "harvest_number", "root_weight_air",
    "root_weight_water", "harvest_number_plan",
    "root_rot_perc", "yield_ha_v2"
  )) %>%
  ggplot(aes(x = trial_name, y = values)) +
  facet_wrap(~var,
             ncol = 6, scales = "free_y"
  ) + 
  geom_violin(fill = "gray") +
  geom_boxplot(width = 0.2, trim = FALSE) +
  labs(x = NULL, y = NULL, title = "") +
  theme_xiaofei() +
  theme(
    axis.text.x = element_text(size = 8, vjust = 1, angle = 65),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(color = "black"),
    strip.text.x = element_text(
      size = 8, face = "bold.italic")
  ) 

plot_bxp

ggsave(paste0("images\\boxplot_", trial_interest, Sys.Date(), ".png"),
       plot = plot_bxp, units = "in", dpi = 300, width = 16, height = 12
)



## Save the tidy data for analysis

write.csv(trial_tidy_1, here::here("output", paste("01_", year_interest, trial_interest,
                                                 "_tidy_data4analysis_", Sys.Date(), ".csv", sep = "")), row.names = FALSE)


# Data analysis

## Load libraries
rm(list = ls())
library(asreml)
source("https://raw.githubusercontent.com/Cassava2050/PPD/main/utilities_tidy.R") # become into a 
trial_interest <- "MDAYT"
year_interest <- 2022


## master_data to save the results
master_data <- list()

## Load the tidy data


trial_set_number = 1
# all files in the folder
list_file = list.files(here::here("output"))
# tidy data of the trials interested
sel_file = list_file[str_detect(list_file, "_tidy_data4analysis_") &
                       str_detect(list_file,
                                  paste(year_interest, trial_interest, sep=""))]
# the data we will use
sel_file_use = sel_file[1]

sel_file_use
trial1_tidy = read.csv(here::here("output", sel_file_use), header=TRUE,
                       stringsAsFactors = FALSE,
                       as.is=T,
                       check.names = FALSE)
if(trial_set_number == 1){
  trial_tidy_all = trial1_tidy
}


## Obtain all the trait information using a cloud file (gitHub) -------
trait_all <-
  read.csv("https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/standar_col_names.csv") %>%
  select(analysis_col_name) %>%
  filter(str_detect(analysis_col_name, "obs_"))
trait_all_adj <- gsub("obs_", "", trait_all$analysis_col_name)
trait_all_adj = c(trait_all_adj,
                  "harvest_number_plan", "germination_perc",
                  "yield_ha_v2", "DM_yield_ha", "starch_content", "starch_yield_ha")
trait_all_adj <- gsub("-", "_", trait_all_adj)


## Meta info.
meta_all <-
  read.csv("https://raw.githubusercontent.com/lfdelgadom/standar_col_names_CB/main/standar_col_names.csv") %>%
  select(analysis_col_name) %>%
  filter(str_detect(analysis_col_name, "use_"))
meta_all_adj <- gsub("use_", "", meta_all$analysis_col_name)
meta_all_adj <- c(
  meta_all_adj,
  "check_released", "latitude", "longitude",
  "altitude", "department", "country",
  "ag_zone", "location_short"
)


## Select the observations for analysis
names(trial_tidy_all) <- gsub("-", "_", names(trial_tidy_all))
analysis_trait <- names(trial_tidy_all)[names(trial_tidy_all) %in% trait_all_adj]
print("All the traits investigated:")
print(analysis_trait)


## Select the meta information for analysis


meta_col <- names(trial_tidy_all)[names(trial_tidy_all) %in% meta_all_adj]
print("All the meta information:")
print(meta_col)


## Check the SD of each trait


trial_rm_sd <- remove_no_var_tidy(my_dat = trial_tidy_all,
                                  analysis_trait = analysis_trait,
                                  meta_info = meta_col)
master_data[["mean_of_sd"]] = sd_mean

## Trait ideal


no_traits_for_analysis <- c(
                            "stake_plant" , "planted_number_plot", 
                            "harvest_number", "root_weight_air", 
                            "root_weight_water", "harvest_number_plan",
                            "yield_ha_v2", "root_rot_perc", "harvest_index",
                            "germinated_number_plot"
)

no_variation_traits <- c() # "CAD_5mon", "CAD_7mon", "CAD_3mon", "lodging1_3_6mon"

no_traits_for_analysis <- c(no_variation_traits, no_traits_for_analysis)

trait_ideal <- analysis_trait[!analysis_trait %in% no_traits_for_analysis]
print("the trait ideal is:"); trait_ideal

trait_ideal %>% as.data.frame() %>% write.table("clipboard", sep = "\t", col.names = T, row.names = F)


# Genotypic correlation (Phenotypic values)
correlation <- gg_cor(
  colours = c("red", "white", "blue"),
  data = trial_rm_sd[, trait_ideal],
  label_size = 2
)

ggsave(paste("images\\pheno_corr", trial_interest, Sys.Date(), ".png", sep = "_"),
       plot = correlation, units = "in", dpi = 300, width = 12, height = 8
)

## Check design experimental

### Agriutilities library
# Rep_number == 3 was removed from 2021103MDAYT_quan, due to there were several missing values
my_dat_1 <- trial_rm_sd %>% filter(trial_name %in% c("2021103MDAYT_quan"), !rep_number == 3)

my_dat <- trial_rm_sd %>% 
  add_column(block = NA) %>% mutate(block = as.factor(block)) %>% 
  filter(!trial_name == "2021103MDAYT_quan") %>% 
  bind_rows(my_dat_1)

# Plot dry matter vs starch content. 
my_dat %>% select(DM_gravity, starch_content, trial_name) %>% 
  drop_na() %>% 
  ggplot(aes(x = DM_gravity, y = starch_content)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~trial_name) +
  theme_xiaofei()

ggsave(paste("images\\dm_starch_corr", trial_interest, Sys.Date(), ".png", sep = "_"),
       units = "in", dpi = 300, width = 12, height = 8
)
  
my_dat_1 <- my_dat %>% filter(trial_name %in% c("202088MDAYT_dona", 
                                                "202089MDAYT_tani",
                                                "2021100MDAYT_phuy",
                                                #"2021101MDAYT_sola",
                                                "2021102MDAYT_tani",
                                                "2021104MDAYT_dona",
                                                "2021105MDAYT_dakl",
                                                "2021103MDAYT_quan"
                                                #"2022118DMAYT_phuy",
                                                #"2021106MDAYT_tani"
))


# number of trials
length(unique(my_dat$trial_name)) 

results <- check_design_met(
  data = my_dat, #my_dat_1
  genotype = "accession_name",
  trial = "trial_name",
  traits = trait_ideal,
  rep = "rep_number",
  col = "col_number",
  row = "row_number",
  block = "block"
)



shared <- plot(results, type = "connectivity")

ggsave(paste('images\\shared_', trial_interest, Sys.Date(), ".png", sep = "_"),
       plot = shared, units = "in", dpi = 300, width = 8, height = 6)

summary <- results$summ_traits 

p1 <- summary %>% 
  ggplot(aes(x = traits , y = trial_name, label = round(miss_perc,2),  fill = miss_perc ))+
  geom_tile(color = "gray")+
  geom_text(color = "white")+
  theme_minimal(base_size = 13)+
  labs(title = "Percentage of missing values (exp/trait)", x = "", y = "") +
  theme(axis.text.x = element_text(hjust = 1 , angle = 75, size = 16),
        axis.text.y = element_text(size = 16))
p1
ggsave(paste("images\\missing_", trial_interest, Sys.Date(), ".png", sep = "_"),
       plot = p1, units = "in", dpi = 300, width = 15, height = 6
)
master_data[["summ_traits"]] <- summary


## Single trial analysis
obj <- single_trial_analysis(results = results,
                             progress = TRUE,
                             remove_outliers = FALSE)

#elements_to_remove <- c("202050DVPRG_ciat", "202136DVPRG_ciat")

trials <- unique(my_dat$trial_name)

header_sort = vector()
i = 1
for (i in 1:length(trials)) {
  
  cat("\n_______________")
  cat("\nTRIAL:", trials[i], "\n")
  cat("_______________\n")
  
  for (j in 1:length(trait_ideal)) {
    
    blue_blup <- obj$blues_blups %>% 
      filter(trial == trials[i]) %>% 
      select(-c(trial, seBLUEs, seBLUPs, wt)) %>% 
      pivot_wider(names_from = "trait", values_from = c("BLUEs", "BLUPs"))
    
    header_sort = c(header_sort,
                    grep(trait_ideal[j], sort(names(blue_blup)), value=TRUE))
    blue_blup <- blue_blup %>% dplyr::select(genotype, any_of(header_sort)) %>% 
      mutate(across(where(is.double), round, 1))
  }
  master_data[[paste0("BLUP_BLUE_", trials[i])]] <- blue_blup
}

plot(obj, type = "spatial") 

## Single heritability
single_h2 <- obj$resum_fitted_model[ ,1:3] %>% 
  group_by(trial) %>%
  spread(trait, value = heritability) #%>% print(width = Inf) 

single_h2 %>% print(width = Inf)

master_data[["single_h2"]] <- single_h2 

single_h2 %>% 
  write.table("clipboard", sep = "\t", col.names = T, row.names = F, na = "")

## Multi environmetal analysis

# It is necessary run again the single trial analysis removing
# - 2022118DMAYT_phuy
# - 2021106MDAYT_tani
# - 2021101MDAYT_sola
# because there was low conectivity among them.

traits_to_remove <- c("root_number_commercial",	"root_plant_number",	
                        "root_rot_number",	"root_skin_color1_3",
                        "germinated_number_plot", "harvest_index", 
                      "shoot_weight_plot", "root_number")
  
  met_results <- met_analysis(obj, 
                              filter_traits = trait_ideal[!trait_ideal %in% c(traits_to_remove)],
                              h2_filter = 0.09,
                              # remove_trials = trials[!trials %in% c("2021101MDAYT_sola",
                              #                                       "2021106MDAYT_tani",
                              #                                       "2022118DMAYT_phuy")],
                              progress = TRUE
                              )



# h2 gxe
master_data[["h2_gxe"]] <- 
  met_results$heritability %>% 
  arrange(desc(h2)) %>%
  mutate(across(where(is.numeric), round, 2))

master_data$h2_gxe %>%
  write.table("clipboard", col.names = T, row.names = F, sep = "\t")

# BLUPs gxe
BLUPs_table <- 
  met_results$overall_BLUPs %>% 
  select(-c(std.error, status)) %>% 
  group_by(genotype) %>% 
  spread(trait, value = predicted.value) %>% 
  rename("accession_name" = genotype) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  ungroup() 
#save the BLUPs data
master_data[[paste0("BLUPs_", "gxe")]] <- BLUPs_table

## Genotypic Correlation: Locations


# Yield
covcor_heat(matrix = met_results$VCOV$yield_ha$CORR, size = 4, legend =c(0.35, 0.8)) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))

ggsave(paste0("images\\yield_vcor", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

# starch
covcor_heat(matrix = met_results$VCOV$starch_content$CORR, size = 4, legend =c(0.35, 0.8)) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))

ggsave(paste0("images\\starch_content_vcor", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

# CMD_harvest
covcor_heat(matrix = met_results$VCOV$CMD_harvest$CORR, size = 4, legend =c(0.35, 0.8)) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))

ggsave(paste0("images\\CMD_harvest_vcor", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

# CMD_1month
covcor_heat(matrix = met_results$VCOV$CMD_1mon$CORR, size = 4, legend =c(0.35, 0.8)) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))

ggsave(paste0("images\\CMD_1mon", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

# CMD_3month
covcor_heat(matrix = met_results$VCOV$CMD_3mon$CORR, size = 4, legend =c(0.35, 0.8)) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))

ggsave(paste0("images\\CMD_3mon_vcor", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

# CMD_6mon
covcor_heat(matrix = met_results$VCOV$CMD_6mon$CORR, size = 4, legend = c(0.35, 0.8)) + theme(
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14))

ggsave(paste0("images\\CMD_6mon_vcor", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

# CMD_10mon
covcor_heat(matrix = met_results$VCOV$CMD_10mon$CORR, size = 4, legend = c(0.35, 0.8)) + theme(
  axis.text.y = element_text(size = 14),
  axis.text.x = element_text(size = 14))

ggsave(paste0("images\\CMD_10mon_vcor", trial_interest, Sys.Date(), ".png"),
       units = "in", dpi = 300, width = 8, height = 6)

## Save variance covariance correlation

as.data.frame(do.call(rbind, met_results$VCOV))$CORR

## Save the BLUEs or raw data across the trials
variables <- colnames(BLUPs_table)[!grepl("accession_name", colnames(BLUPs_table))]
for (var in variables) {
  
  cat("\n_______________")
  cat("\nTRIAL:", var, "\n")
  cat("_______________\n")
  
  blue_blup <-
    obj$blues_blups %>%
    select(trial, genotype, trait, BLUEs) %>%
    spread(trait, value = BLUEs) %>%
    select(trial, genotype, any_of(var)) %>%
    group_by(trial, genotype) %>%
    pivot_wider(names_from = trial, values_from = any_of(var)) %>%
    right_join(BLUPs_table %>%
                 select(accession_name, any_of(var)), by = c("genotype" = "accession_name")) %>%
    arrange(is.na(across(where(is.numeric))), across(where(is.numeric))) %>%
    mutate(across(where(is.numeric), round, 2))
  # remove all NA columns
  blue_blup <- blue_blup[, colSums(is.na(blue_blup)) < nrow(blue_blup)]
  
  master_data[[paste0("BLUP_BLUE_", var)]] <- blue_blup
}

## Stability analysis
for (var in variables) {
  
  cat("\n_______________")
  cat("\nTRIAL:", var, "\n")
  cat("_______________\n")
  
  stab <- met_results$stability %>% 
    filter(trait == var) %>% 
    arrange(superiority) %>% 
    pivot_wider(names_from = "trait", values_from = c('predicted.value')) 
  
  # Change colname
  colnames(stab)[5] <- paste('BLUPs', colnames(stab)[5], sep = '_') 
  colnames(stab)[c(2, 3, 4)] <- paste(colnames(stab)[c(2, 3, 4)], var, sep = '_') 
  
  master_data[[paste0("stability_", var)]] <- stab
}


ind <- grep("^stability_", names(master_data))

# select elements that satisfy the condition
stab_values <- master_data[ind] %>% 
  reduce(inner_join, by = "genotype") %>% 
  select(!starts_with("BLUPs_")) %>% 
  mutate(across(where(is.numeric), round, 2))

# remove multiple stability sheets
master_data[ind] <- NULL

## BLUE and BLUP data together
BLUEs_BLUPs <- 
  obj$blues_blups %>%
  select(trait, genotype, trial, BLUEs, seBLUEs) %>%
  filter(trait %in% variables) %>% 
  pivot_wider(names_from = "trait", values_from = c("BLUEs", "seBLUEs")) %>%
  pivot_wider(names_from = trial, values_from = c(
    paste("BLUEs", variables, sep = "_"),
    paste("seBLUEs", variables, sep = "_")
  )) %>%
  left_join(
    met_results$overall_BLUPs %>%
      select(!status) %>%
      rename(
        BLUPs = predicted.value,
        seBLUPs = std.error
      ) %>%
      pivot_wider(names_from = "trait", values_from = c("BLUPs", "seBLUPs")),
    by = "genotype"
  ) %>%
  arrange(desc(BLUPs_DM_gravity)) %>% 
  arrange(is.na(across(where(is.numeric))), across(where(is.numeric))) %>%
  mutate(across(where(is.numeric), round, 2))
# remove all NA columns
BLUEs_BLUPs <- BLUEs_BLUPs[, colSums(is.na(BLUEs_BLUPs)) < nrow(BLUEs_BLUPs)]


# put all together stab_values with blues_blups
BLUEs_BLUPs <- 
  BLUEs_BLUPs %>% left_join(stab_values, by = 'genotype')  


header_sort = vector()
for (i in 1:length(variables)) {
  
  header_sort = c(header_sort, 
                  grep(variables[i], sort(names(BLUEs_BLUPs)), value=TRUE) 
  )
  
}


BLUEs_BLUPs <- BLUEs_BLUPs %>%
  select(genotype, all_of(header_sort), -starts_with("se")) 
BLUEs_BLUPs <- BLUEs_BLUPs %>% 
  relocate(colnames(BLUEs_BLUPs)[str_detect(colnames(BLUEs_BLUPs), "starch_content")], .after = genotype)


master_data[["BLUEs_BLUPs_MET"]] = BLUEs_BLUPs


## Genotypic correlation
geno_cor <- gg_cor(
  colours = c("red", "white", "blue"),
  data = BLUPs_table, # remove carotenoids
  label_size = 2.5
) + 
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14))


ggsave(paste("images\\geno_corr", trial_interest, Sys.Date(), ".png", sep = "_"),
       units = "in", dpi = 300, width = 14, height = 8)



## Save the master data results
folder_output <- here::here("output//")
meta_file_name <- paste0(folder_output, paste("2022", trial_interest, "master_results", Sys.Date(), ".xlsx", sep = "_"))

write.xlsx(master_data, file = meta_file_name)


