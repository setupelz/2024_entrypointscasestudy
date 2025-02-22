# Visualisation of European Union Case Study

# LOAD PACKAGES AND FUNCTIONS --------------------------------------------------

library(pacman)

p_load(readr, dplyr, tidyr, ggplot2, geomtextpath, here, patchwork)

# options
options(scipen = 999)

# Process arbitrary region-iso3c YAML file
process_yaml_file <- function(file_path) {
  
  # Read the YAML file
  yaml_data <- yaml::read_yaml(file_path)
  
  # Recursive function to flatten YAML data
  flatten_yaml <- function(data, prefix = character()) {
    if (is.list(data) && !("child" %in% names(data))) {
      do.call(rbind, lapply(names(data), function(x) flatten_yaml(data[[x]], c(prefix, x))))
    } else if ("child" %in% names(data)) {
      tibble(region = paste(prefix, collapse = " > "), iso3c = data$child)
    } else {
      tibble(region = paste(prefix, collapse = " > "), iso3c = data)
    }
  }
  
  # Apply the recursive function
  tidy_data <- flatten_yaml(yaml_data)
  
  # Clean up region names, remove prefixes, and exclude 'world' regions
  tidy_data <- tidy_data %>%
    separate_rows(iso3c, convert = TRUE) %>%
    mutate(region = gsub(" > child", "", region),
           region = gsub("R[0-9]+_", "", region), # Remove prefixes like R12, region, R5
           region = gsub("_", " ", region)) %>%
    filter(!grepl("World", region, ignore.case = TRUE)) %>%
    arrange(region, iso3c)
  
  return(tidy_data)
}

# LOAD DATA --------------------------------------------------------------------

#
iso3c_r11 <- process_yaml_file(here("Data", "countrygroups", "R11.yaml")) %>% 
  mutate(region =
           case_when(
             iso3c %in% c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", 
                          "FRA", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", 
                          "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE") ~ "EU27",
             TRUE ~ region
           ))

# Load previously calculated remaining carbon budgets and aggregate to regions
rcb1990_2023 <- read_csv(here("Data", "rcb_Lamboll_1.5p50_ALLCOUNTRY_gt_co2ffi_terr_yearend.csv")) %>% 
  filter(iso3c != "Bunkers") %>% 
  left_join(iso3c_r11) %>% 
  mutate(region = ifelse(is.na(region), "ROW", region),
         region = ifelse(region == "WEU", "OWEU", region)) %>% 
  select(region, iso3c, year, pp1990 = ecpc_pp1990_share_gtco2_nobunkers_rcbyearend, 
         pp1990_atp_mer_pf2 = cpc_pp1990_atp_mer_pf2_share_gtco2_nobunkers_rcbyearend) %>% 
  pivot_longer(-c(region, iso3c, year))

rcb1990_2023_agg <- rcb1990_2023 %>% 
  group_by(region, year, name) %>% 
  summarise(rcbyear = sum(value))

# Figure 1
a <- rcb1990_2023_agg %>% 
  filter(region %in% c("EU27")) %>% 
  mutate(name = ifelse(name == "pp1990", "1_ECPC1990", "2_CPC1990adjCAP")) %>% 
  ggplot(aes(x = year+1, y = rcbyear, label = name, colour = name)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_textpath(size = 5, linewidth = 1, straight = TRUE) +
  scale_x_continuous(breaks = c(seq(1990, 2020, 5), 2023)) +
  scale_color_viridis_d(begin = 0.2, end = 0.6, direction = -1) +
  labs(x = NULL, y = "Remaining budget (GtCO2)",
       subtitle = "A. Consumption of EU27 RCB") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        text = element_text(size = 14))  +
  guides(shape = "none", colour = "none")

b <- rcb1990_2023_agg %>% 
  filter(year == 2022) %>% 
  ungroup() %>% 
  mutate(name = ifelse(name == "pp1990", "1_ECPC1990", "2_CPC1990adjCAP"),
         region = forcats::fct_reorder(region, rcbyear)) %>% 
  ggplot(aes(x = region, y = rcbyear, shape = name, colour = name)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_viridis_d(begin = 0.2, end = 0.6, direction = -1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(x = NULL, y = "Remaining budget (GtCO2)",
       subtitle = "B. RCB in 2023 across illustrative regions") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))  +
  guides(shape = "none", colour = "none")

# Load previously calculated remaining carbon budgets and aggregate to regions
c <- rcb1990_2023 %>% 
  filter(year == 2022, region == "EU27") %>% 
  ungroup() %>% 
  mutate(name = ifelse(name == "pp1990", "1_ECPC1990", "2_CPC1990adjCAP"),
         rcb = value,
         iso3c = forcats::fct_reorder(iso3c, rcb)) %>% 
  ggplot(aes(x = iso3c, y = rcb, shape = name, colour = name)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, alpha = 0.7) +
  scale_color_viridis_d(begin = 0.2, end = 0.6, direction = -1) +
  labs(x = NULL, y = "Remaining budget (GtCO2)",
       subtitle = "C. RCB in 2023 across EU27 countries") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))  +
  guides(shape = "none", colour = "none")

wrap_plots(wrap_plots(a,b, ncol = 2), c, ncol = 1)

ggsave(here("fig1.png"), height = 8, width = 10)
