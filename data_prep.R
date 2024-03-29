# To-do list

# Fix the correlation table to work with output of main project (Luke and Tom to-do)
# Make completed version of lit_search_file, with necessary columns for pub_info and meta_info (to be discussed at our meeting - maybe unnecessary work?)
# Update the heritability data with Tom's new methods (Tom to-do)
# Update/check the text in readmes, especially the 'info about calculations' one
# Fix or remove the correlation heat map viewer
# Standardise terminology "trait guild"?
# Fix non-unique new trait names, dt %>% group_by(Trait, Sex) %>% summarise(n=n()) %>% filter(n>1) %>% print(n=10000)


library(tidyverse)
library(stringr)

# Load data from Github repo and local literature search file (need to update that file...)
lit_search_file <- read_csv("DGRP QTC literature.search_20220411.csv")
meta_data <- read_csv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/data/derived/meta_data_for_all_traits.csv")
unscaled_line_means <- read_csv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/data/derived/all.dgrp.phenos_unscaled.csv") 
# Fix this line once Tom fixes the file on Github...
# SNP_heritability <- read_csv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/data/derived/SNP_heritability_cleaned.csv") %>% 
#   select(Trait, Sex, Reference, starts_with("V"), starts_with("SE_"))
heritability <- read_csv("SNP_heritability.csv") 
gwas_hits <- read_tsv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/gwas_data/derived/all_traits_significant_SNPs.tsv")

###############################################################
# Run some preliminary checks: all should return TRUE if the input data are as expected.
test1 <- all(meta_data$Trait %in% unscaled_line_means$Trait)
test2 <- all(unscaled_line_means$Trait %in% meta_data$Trait)
test3 <- all(gwas_hits$Trait %in% meta_data$Trait)

test4 <- all(heritability$Trait %in% meta_data$Trait)
heritability$Trait[!heritability$Trait %in% meta_data$Trait] # Currently Tom's heritability file uses some out of date trait names

# Check that all combinations of trait ID, reference and sex are unique (i.e. no trait IDs accidentally re-used), for 'meta_data'
test5 <- all((meta_data %>% group_by(Trait, Reference, Sex) %>% 
  summarise(n=n(), .groups = "drop") %>% pull(n)) == 1)

# Check that all combinations of trait ID, reference and sex are unique (i.e. no trait IDs accidentally re-used), for 'unscaled_line_means'
test6 <- all((unscaled_line_means %>% distinct(Trait, Reference) %>%  group_by(Trait, Reference) %>% 
       summarise(n=n(), .groups = "drop") %>% pull(n)) == 1)

data.frame(test1,test2,test3,test4,test5,test6)


###############################################################
# First make 'dt' for the app: this is a table with one row per trait

dt <- meta_data %>% 
  mutate(Trait_ID = Trait) %>% 
  distinct(Trait_ID, Sex, Reference, .keep_all = TRUE) %>% 
  arrange(`Trait guild`, Trait_ID, Sex) %>% 
  mutate(Trait = case_when(Sex == "Female" | Sex == "Male" ~ gsub('.{2}$', '', Trait_ID),
                                    Sex == "Both" ~ Trait_ID,
                                    Sex == "Pooled" ~ Trait_ID)) %>% 
  mutate(Trait = gsub("201.", "", Trait)) %>% 
  mutate(Trait = gsub("202.", "", Trait)) %>% 
  mutate(Trait = gsub("\\.", " ", Trait)) %>% 
  mutate(Trait = gsub(" aeruginosa", ". aeruginosa", Trait)) %>% 
  mutate(Trait = gsub(" entomophila", ". entomophila", Trait)) %>% 
  mutate(Trait = gsub(" monocytogenes", ". monocytogenes", Trait)) %>% 
  mutate(Trait = gsub(" rettgeri", ". rettgeri", Trait)) %>% 
  mutate(Trait = gsub(" anisopliae", ". anisopliae", Trait)) %>% 
  mutate(Trait = gsub("0 2", "0.2", Trait)) %>% 
  mutate(Trait = gsub("0 5", "0.5", Trait)) 
substr(dt$Trait,1,1) <- toupper(substr(dt$Trait,1,1))

dt <- dt %>% 
  left_join(read_csv("DGRP QTC literature.search_20220411.csv"), 
                 by = c("Sex", "Reference")) %>% 
  select(`Trait guild`, Trait, Trait_ID, Sex, Description = `Trait description`, Reference,
         `# lines measured`, Age, Housing, Diet, Temperature, `Wolbachia adjusted`, 
         `Baseline reference` = `Baseline reference (eg competitor genptype)`) 

###############################################################
# Making the line_means file - contains an estimate of the line means for every trait
line_means <- unscaled_line_means %>% 
  select(Trait_ID = Trait, line, trait_value)

###############################################################
# Making the herit_info file - contains an estimate of heritability etc for each trait

heritability <- heritability %>% 
  select(-`Trait guild`, -`Trait description`, -Sex, -Reference) %>% 
  rename(`V(E)` = `V(e)`, 
         `V(P)` = Vp, 
         `V(G)/V(P)` = `V(G)/Vp`,
         `SE V(G)` = `SE_V(G)`,
         `SE V(E)` = `SE_V(e)`,
         `SE V(P)` = SE_Vp,
         `SE V(G)/V(P)` = `SE_V(G)/Vp`) %>% 
  rename(Trait_ID = Trait)


###############################################################
# Making the meta_info file - contains details for experimental conditions
meta_info <- dt %>% 
  distinct(Trait_ID, Trait, Reference) %>% 
  left_join(lit_search_file %>% 
              filter(!is.na(Reference)) %>% 
              select(Reference, `No. lines used`, Sex, Age, `Sample Size (per sex if applic)`, Housing, Diet, Temperature, `Wolbachia adjusted`, `Baseline reference (eg competitor genptype)`) %>% 
              distinct(Reference, .keep_all = TRUE),
            by = "Reference")


###############################################################
# Make pub_info (for the table with full publication information):
pub_info <- dt %>% 
  distinct(Reference) %>% 
  left_join(lit_search_file %>% 
              filter(!is.na(Reference)) %>% 
              select(Reference, Authors, Title, Year, `Full Text URL`= FullTextURL) %>% 
              distinct(Reference, .keep_all = TRUE), 
            by = "Reference")


###############################################################
# Make gwas_hits (for the table of significant variants from GWAS):
# And gwas_summary (counts of significant snps on each chromosome)

gwas_hits <- gwas_hits %>%
  mutate(log10_P = format(round(-1 * log10(P), 2), nsmall = 2)) %>% 
  select(Trait_ID, Variant, 
         FBID, `Gene name` = gene_name, `Site class` = site_class, 
         `Distance to gene` = distance_to_gene, 
         `Minor allele frequency` = MAF, 
         `Minor allele` = minor_allele, `Major allele` = major_allele, 
         BETA, SE, P, `-log10(P)` = log10_P) 

gwas_summary <- gwas_hits %>% 
  mutate(Chromosome = map_chr(str_split(Variant, "_"), ~ .x[1])) %>% 
  group_by(Trait_ID, Chromosome) %>% 
  summarise(`Number of variants with p < 1e-5` = n(), 
            `Number of variants with p < 1e-8` = sum(P < 1e-8), 
            .groups = "drop") 

###############################################################
# making corr_p file (should be reworked to use the data from the main paper scripts, this works fine for now)
corr_dt <- all_data %>% 
  select(line, Trait_ID, trait_value) %>% 
  pivot_wider(id_cols = line, names_from = Trait_ID, values_from = trait_value, 
              values_fn = list(trait_value = mean)) %>% 
  as.data.frame()

rownames(corr_dt) <- corr_dt$line
corr_dt <- corr_dt %>% 
  dplyr::select(-line) 

library(psych)

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

corr_p <- cbind(corr_p, dt[match(corr_p$trait, dt$Trait_ID), "Sex"])
colnames(corr_p)[ncol(corr_p)] = "Sex_1"

corr_p <- cbind(corr_p, dt[match(corr_p$sec_trait, dt$Trait_ID), "Sex"])
colnames(corr_p)[ncol(corr_p)] = "Sex_2"

corr_p <- corr_p %>% 
  mutate(trait_ID_1 = trait) %>% 
  mutate(trait_ID_2 = sec_trait)%>% 
  mutate(trait = case_when(Sex_1 == "Female" | Sex_1 == "Male" ~ gsub('.{2}$', '', trait_ID_1),
                           Sex_1 == "Both" ~ trait_ID_1,
                           Sex_1 == "Pooled" ~ trait_ID_1)) %>% 
  mutate(trait = gsub("201.", "", trait)) %>% 
  mutate(trait = gsub("202.", "", trait)) %>% 
  mutate(trait = gsub("\\.", " ", trait)) %>% 
  mutate(trait = gsub(" aeruginosa", ". aeruginosa", trait)) %>% 
  mutate(trait = gsub(" entomophila", ". entomophila", trait)) %>% 
  mutate(trait = gsub(" monocytogenes", ". monocytogenes", trait)) %>% 
  mutate(trait = gsub(" rettgeri", ". rettgeri", trait)) %>% 
  mutate(trait = gsub(" anisopliae", ". anisopliae", trait)) %>% 
  mutate(trait = gsub("0 2", "0.2", trait)) %>% 
  mutate(trait = gsub("0 5", "0.5", trait)) %>% 
  mutate(trait = str_to_sentence(trait)) %>% 
  mutate(trait = paste(trait, " (", Sex_1, ")", sep = "")) %>% 
  mutate(sec_trait = case_when(Sex_2 == "Female" | Sex_2 == "Male" ~ gsub('.{2}$', '', trait_ID_2),
                               Sex_2 == "Both" ~ trait_ID_2,
                               Sex_2 == "Pooled" ~ trait_ID_2)) %>% 
  mutate(sec_trait = gsub("201.", "", sec_trait)) %>% 
  mutate(sec_trait = gsub("202.", "", sec_trait)) %>% 
  mutate(sec_trait = gsub("\\.", " ", sec_trait)) %>% 
  mutate(sec_trait = gsub(" aeruginosa", ". aeruginosa", sec_trait)) %>% 
  mutate(sec_trait = gsub(" entomophila", ". entomophila", sec_trait)) %>% 
  mutate(sec_trait = gsub(" monocytogenes", ". monocytogenes", sec_trait)) %>% 
  mutate(sec_trait = gsub(" rettgeri", ". rettgeri", sec_trait)) %>% 
  mutate(sec_trait = gsub(" anisopliae", ". anisopliae", sec_trait)) %>% 
  mutate(sec_trait = gsub("0 2", "0.2", sec_trait)) %>% 
  mutate(sec_trait = gsub("0 5", "0.5", sec_trait)) %>% 
  mutate(sec_trait = str_to_sentence(sec_trait)) %>% 
  mutate(sec_trait = paste(sec_trait, " (", Sex_2, ")", sep = "")) 

###############################################################
# Combine meta data and line means for all data
all_data <- line_means %>% 
  left_join(dt)



saveRDS(dt, "app_data/dt.RDS") 
saveRDS(line_means, "app_data/line_means.rds")
saveRDS(heritability, "app_data/heritability.rds")
saveRDS(meta_info, "app_data/meta_info.rds")
saveRDS(pub_info, "app_data/pub_info.rds")
saveRDS(gwas_hits, "app_data/gwas_hits.rds")
saveRDS(gwas_summary, "app_data/gwas_summary.rds")
saveRDS(all_data, "app_data/all_data.rds")
saveRDS(corr_p, "app_data/corr_p.rds")

