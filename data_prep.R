library(tidyverse)



# meta_data <- read.csv("DGRP QTC literature.search_20220411.csv", na.strings = "") %>%
#   filter(Status == "have" | Status == "don't") %>%
#   select(-Status, -Status.rationale, -Notes, -Data.type, -Trait.s, -Guild.s, -Data.type.1, -Sample.Size..per.sex.if.applic.)
# unscaled_data <- read.csv("all.dgrp.phenos_unscaled_20220412.csv")
# SNP_heritability_cleaned <- read.csv("SNP_heritability_cleaned.csv")
# SNP_heritability <- read.csv("SNP_heritability.csv")


# Load data from Github repo and local literature search file (need to update that file...)
lit_search_file <- read_csv("DGRP QTC literature.search_20220411.csv")
meta_data <- read_csv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/data/derived/meta_data_for_all_traits.csv")
unscaled_data <- read_csv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/data/derived/all.dgrp.phenos_unscaled.csv") 
# Fix this line once Tom fixes the file on Github...
# SNP_heritability <- read_csv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/data/derived/SNP_heritability_cleaned.csv") %>% 
#   select(Trait, Sex, Reference, starts_with("V"), starts_with("SE_"))
heritability <- read_csv("SNP_heritability.csv") %>% 
  select(-`Trait guild`, -`Trait description`) %>% 
  rename(`V(E)` = `V(e)`, 
         `V(P)` = Vp, 
         `V(G)/V(P)` = `V(G)/Vp`,
         `SE V(G)` = `SE_V(G)`,
         `SE V(E)` = `SE_V(e)`,
         `SE V(P)` = SE_Vp,
         `SE V(G)/V(P)` = `SE_V(G)/Vp`) 
gwas_hits <- read_tsv("https://raw.githubusercontent.com/tomkeaney/DGRP/master/gwas_data/derived/all_traits_significant_SNPs.tsv")


###############################################################
# Making the all_data file - contains line mean data for every trait, plus Reference column for careful cross-matching
all_data <- unscaled_data %>% 
  left_join(meta_data, by = c("Trait", "Reference")) %>% 
  left_join(heritability, by = c("Trait", "Reference", "Sex"))

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
  mutate(Trait_readable = gsub("0 2", "0.2", Trait_readable)) %>% 
  rename(Trait_old = Trait, Trait = Trait_readable)


heritability <- all_data %>% select(Trait, starts_with("V"), starts_with("SE "))
all_data <- all_data %>% select(-starts_with("V"), -starts_with("SE "))


###############################################################
# making dt file (For the first table user sees, where they can select a trait to learn about)
dt <- all_data %>% 
  select(`Trait guild`, Trait, Sex, Description = `Trait description`, Reference) %>% 
  distinct(Trait, .keep_all = TRUE) 


###############################################################
# Make pubinfo (for the table with full publication information):
pub_info <- lit_search_file %>% 
  filter(!is.na(Reference)) %>% 
  select(Reference, Authors, Title, Year, `Full Text URL`= FullTextURL) %>% 
  distinct(Reference, .keep_all = TRUE) 

pub_info <- dt %>% select(Trait, Reference) %>% 
  left_join(pub_info, by = "Reference")

###############################################################
# Make meta_info (for the experimental conditions table):
meta_info <- read_csv("DGRP QTC literature.search_20220411.csv") %>% 
  left_join(meta_data %>% select(Reference, `# lines measured`)) %>% 
  select(Reference, `# lines measured`, Age, Housing, Diet, Temperature, `Wolbachia adjusted`, 
         `Baseline reference` = `Baseline reference (eg competitor genptype)`) %>% 
  filter(!is.na(Reference)) %>% 
  distinct(Reference, .keep_all = TRUE)

meta_info <- all_data %>%       # NOTE THIS IS WRONG, WE NEED TO MAKE A NEW VERSION OF DGRP QTC literature.search_20220411.csv, with one row per trait
  select(Trait, Reference) %>% 
  left_join(meta_info, by = "Reference") %>% 
  distinct(Reference, .keep_all = TRUE)



# making corr_p file
corr_dt <- all_data %>% 
  dplyr::select(line, Trait_old, trait_value) %>% 
  pivot_wider(id_cols = line, names_from = Trait_old, values_from = trait_value, 
              values_fn = list(trait_value = mean)) %>% 
  as.data.frame()

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
  left_join(meta_data %>% select(Trait, Reference, Sex), by = c("trait" = "Trait")) %>% 
  select(trait, Reference, Sex, SNP, FBID, gene_name, site_class, distance_to_gene, MAF, minor_allele, major_allele, BETA, SE, P, log10_P) %>% 
  filter(trait %in% all_data$Trait_old) %>% 
  mutate(log10_P = format(round(log10_P, 2), nsmall = 2)) %>% 
  rename(Trait = trait, 
         Variant = SNP, 
         `Gene name` = gene_name, 
         `Site class` = site_class,
         `Distance to gene` = distance_to_gene,
         `Minor allele` = minor_allele, `Major allele` = major_allele,
         `-log10(P)` = log10_P)

saveRDS(dt, "app_data/dt.RDS") 
saveRDS(all_data, "app_data/all_data.rds")
saveRDS(heritability, "app_data/heritability.rds")
saveRDS(corr_p, "app_data/corr_p.rds")
saveRDS(gwas_hits, "app_data/gwas_hits.rds")
saveRDS(pub_info, "app_data/pub_info.rds")
saveRDS(meta_info, "app_data/meta_info.rds")
