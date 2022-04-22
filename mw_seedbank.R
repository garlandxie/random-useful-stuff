## table ----

# summarize: species as rows, site as columns, cells as abundances
table_summary <- sb %>%
  group_by(site, spp_code) %>%
  summarize(abundance = sum(total_abund, na.rm = TRUE)) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = site, values_from = abundance) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  left_join(sb_taxon, by = c("spp_code" = "Code")) %>%
  dplyr::select(
    spp_code, Genus, Species, 
    VICP, TIMH, KENN, 
    GRNB, BNSH, DAVE, 
    DVAG, AMBJ, BRIM
    )

write.csv(
  x = table_summary, 
  here("data", "analysis_data", "table_devlin.csv"), 
)

## total germination ----

# spring and fall germination total emergence for each treatment
total_germ_by_trt <- sb %>%
  group_by(season, treatment) %>%
  summarize(abund = sum(total_abund, na.rm = TRUE)) %>%
  ungroup()

write.csv(
  x = total_germ_by_trt, 
  file = here("data", "intermediate_data", "total_germ_by_sn_trt.csv")
)

## obtain species identity for plot outliers -----

### BRIM Spring 11 ----
out_brim_spr <- sb %>%
  filter(site == "BRIM" & season == "Spring") %>%
  group_by(season, site, treatment, plot) %>%
  summarize(abund = sum(total_abund, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(abund))

out_brim_spr_11 <- sb %>%
  filter(site == "BRIM" & season == "Spring" & plot == 11) 

write.csv(
  x = out_brim_spr_11, 
  file = here("data", "intermediate_data", "out_brim_spr_11.csv")
)

### BNSH Spring 15 ----
out_bnsh_spr <- sb %>%
  filter(site == "BNSH" & season == "Spring") %>%
  group_by(season, site, treatment, plot) %>%
  summarize(abund = sum(total_abund, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(abund))

out_bnsh_15 <- sb %>%
  filter(site == "BNSH" & season == "Spring" & plot == 15) 

write.csv(
  x = out_bnsh_15, 
  file = here("data", "intermediate_data", "out_bnsh_spr_15.csv")
)

### BRIM Fall 11 ----

out_brim_fall <- sb %>%
  filter(site == "BRIM" & season == "Fall") %>%
  group_by(season, site, treatment, plot) %>%
  summarize(abund = sum(total_abund, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(abund))

out_brim_fall_11 <- sb %>%
  filter(site == "BRIM" & season == "Fall" & plot == 11) 

write.csv(
  x = out_brim_fall_11, 
  file = here("data", "intermediate_data", "out_brim_fall_11.csv")
)

### AMBJ Fall ----

out_ambj_fall <- sb %>%
  filter(site == "AMBJ" & season == "Fall") %>%
  group_by(season, site, treatment, plot) %>%
  summarize(abund = sum(total_abund, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(abund))

out_ambj_fall_23 <- sb %>%
  filter(site == "AMBJ" & season == "Fall" & plot == 23) 

write.csv(
  x = out_ambj_fall_23, 
  file = here("data", "intermediate_data", "out_ambj_fall_23.csv")
)

### TIMH Fall ----

out_timh_fall <- sb %>%
  filter(site == "TIMH" & season == "Fall") %>%
  group_by(season, site, treatment, plot) %>%
  summarize(abund = sum(total_abund, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(abund))

out_timh_fall_5 <- sb %>%
  filter(site == "TIMH" & season == "Fall" & plot == 5) 

write.csv(
  x = out_timh_fall_5, 
  file = here("data", "intermediate_data", "out_timh_fall_5.csv")
)
