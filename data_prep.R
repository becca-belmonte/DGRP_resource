library(tidyverse)

meta_data <- read.csv("Z:/Data/DGRP_resource/DGRP_resource/DGRP QTC literature.search_20220411.csv", na.strings = "") %>%
  filter(Status == "have" | Status == "don't") %>%
  select(-Status, -Status.rationale, -Notes, -Data.type, -Trait.s, -Guild.s, -Data.type.1, -Sample.Size..per.sex.if.applic.)
unscaled_data <- read.csv("Z:/Data/DGRP_resource/DGRP_resource/all.dgrp.phenos_unscaled_20220412.csv")
SNP_heritability_cleaned <- read.csv("Z:/Data/DGRP_resource/DGRP_resource/SNP_heritability_cleaned.csv")
SNP_heritability <- read.csv("Z:/Data/DGRP_resource/DGRP_resource/SNP_heritability.csv")
gwas_hits <- read.csv("Z:/Data/DGRP_resource/DGRP_resource/gwas_hits_for_shiny_app.csv")
filter_data <- unscaled_data %>% 
  filter(Reference %in% meta_data$Reference)

# making all_data file
all_data <- filter_data %>% 
  left_join(meta_data, by = c("Reference", "Sex")) %>% 
  left_join(SNP_heritability)


all_data <- all_data %>% 
  mutate(Trait_readable = case_when(Sex == "Female" | Sex == "Male" ~ (gsub('.{2}$', '', Trait)),
                                    Sex == "Both"~ Trait)) %>% 
  mutate(Trait_readable = gsub("201.", "", Trait_readable)) %>% 
  mutate(Trait_readable = gsub("202.", "", Trait_readable)) %>% 
  mutate(Trait_readable = gsub("\\.", " ", Trait_readable)) %>% 
  mutate(Trait_readable = gsub(" aeruginosa", ". aeruginosa", Trait_readable)) %>% 
  mutate(Trait_readable = gsub(" entomophila", ". entomophila", Trait_readable)) %>% 
  mutate(Trait_readable = gsub(" monocytogenes", ". monocytogenes", Trait_readable)) %>% 
  mutate(Trait_readable = gsub(" rettgeri", ". rettgeri", Trait_readable)) %>% 
  mutate(Trait_readable = gsub(" anisopliae", ". anisopliae", Trait_readable)) %>% 
  mutate(Trait_readable = gsub("0 2", "0.2", Trait_readable))
colnames(all_data) <- c("line", "Trait_old", "Sex", "trait_value", "Trait guild", "Description", 
                            "Reference", "Authors", "Title", "Year", "Full Text URL",
                            "No_lines_used", "Age", "Housing", "Diet", 
                            "Temperature", "Wolbachia adjusted", "Baseline reference", "V(G)", "V(E)", 
                            "V(P)", "V(G)/V(P)", "SE V(G)", "SE V(E)", "SE V(P)", "SE V(G)/V(P)", "Trait")


all_data <- all_data %>% 
  mutate(Sex = case_when(Trait_old == "change.methamphetamine.preferenceA.f" ~ "Female",
                         TRUE ~ Sex)) %>% 
  select(line, Trait, Sex, trait_value, `Trait guild`, Description, Reference, Authors, Title, Year, `Full Text URL`, No_lines_used, Age, Housing, Diet, Temperature, `Wolbachia adjusted`, `Baseline reference`, 
         `V(G)`, `V(E)`, `V(P)`, `V(G)/V(P)`, `SE V(G)`,`SE V(E)`,`SE V(P)`, `SE V(G)/V(P)`, Trait_old)

# making dt file
dt <- all_data  %>% 
  select(-line, -trait_value, -`V(G)`, -`V(E)`, -`V(P)`, -`V(G)/V(P)`, -`SE V(G)`, -`SE V(E)`, -`SE V(P)`, -`SE V(G)/V(P)`) %>% 
  distinct(Trait, Sex, Reference, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(`Trait guild`, Trait, Sex, Description, Reference, Title, `Full Text URL`)


# making corr_p file
corr_dt <- all_data %>% 
  dplyr::select(line, Trait_old, trait_value) %>% 
  pivot_wider(id_cols = line, names_from = Trait_old, values_from = trait_value, 
              values_fn = list(trait_value = mean))

corr_dt <- as.data.frame(corr_dt)

rownames(corr_dt) <- corr_dt$line
corr_dt <- corr_dt %>% 
  dplyr::select(-line) 


spearman_results <- corr.test(corr_dt, adjust = "holm", ci = FALSE, method = "spearman")
spearman_corr_p_values <- as.data.frame(spearman_results$p)
spearman_corr <- as.data.frame(spearman_results$r)

corr_gg <- spearman_corr %>% 
  mutate(sec_trait = rownames(spearman_corr)) %>% 
  pivot_longer(cols = colnames(spearman_corr), names_to = "trait", values_to = "Correlation") %>% 
  mutate(first_combo = paste(trait, sec_trait)) %>% 
  mutate(sec_combo = paste(sec_trait, trait)) %>% 
  pivot_longer(cols = first_combo:sec_combo, names_to = "combo", values_to = "traits_together") %>% 
  arrange(trait) %>% 
  distinct(traits_together, .keep_all = TRUE) %>% 
  filter(!trait == sec_trait) %>% 
  filter(combo != "sec_combo")

corr_p <- spearman_corr_p_values %>% 
  mutate(sec_trait = rownames(spearman_corr_p_values)) %>% 
  pivot_longer(cols = colnames(spearman_corr_p_values), names_to = "trait", values_to = "p_val") %>% 
  mutate(first_combo = paste(trait, sec_trait)) %>% 
  mutate(sec_combo = paste(sec_trait, trait)) %>% 
  pivot_longer(cols = first_combo:sec_combo, names_to = "combo", values_to = "traits_together") %>% 
  arrange(trait) %>% 
  distinct(traits_together, .keep_all = TRUE) %>% 
  filter(!trait == sec_trait) %>% 
  filter(combo != "sec_combo") %>% 
  left_join(corr_gg)
corr_p <- corr_p %>% 
  mutate(Correlation = round(Correlation, 3)) %>% 
  mutate(p_val = round(p_val, 3))

gwas_hits <- gwas_hits %>% 
  filter(Trait %in% all_data$Trait_old)
