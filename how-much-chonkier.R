library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Montserrat", "mont")
font_add_google("Zilla Slab", "zilla")
showtext_auto()


df <- read_csv("data/pokemon.csv") %>% 
  filter(!is.na(evochain_2)) %>%   # Only keep pokemon that evolve
  select(
    national_number, english_name, starts_with("evo"),
    contains("type"), height_m, weight_kg)




# Create long evolution chains --------------------------------------------------------

stages <- c("evo_stage_0", "evo_stage_1", "evo_stage_2", "evo_stage_3")
stages <- c("evochain_0", "evochain_2", "evochain_4", "evochain_6")

evos <- df %>% select(stages) %>% 
  distinct() %>% 
  mutate(basic_pokemon = evochain_0) %>% 
  pivot_longer(
    cols = all_of(stages),
    names_to = "evo_stage",
    values_to = "english_name",
    ) %>% 
  drop_na(english_name)


# ID Problem evolutions - e.g., multiple evo stems ------------------------------------------------------

multi_evo <- evos %>% count(english_name) %>% 
  filter(n > 1) %>% select(english_name) %>% unlist()

df %>% filter(english_name %in% multi_evo) %>% View()


# Fix problem evolutions --------------------------------------------------

eevee_evos <- df %>% filter(evochain_0 == "Eevee" & english_name != "Eevee") %>% 
  select("english_name") %>% unlist()
hitmon_evos <- df %>% filter(grepl("Hitmon", english_name)) %>% 
  select("english_name") %>% unlist()


clean_evos <- df %>% mutate(
    evo_stage_0 = case_when(
      evochain_0 == "Egg" ~ evochain_2,
      english_name %in% hitmon_evos ~ "Tyrogue",  # Fix Tyrogue evolutions
      TRUE ~ evochain_0),
    evo_stage_1 = case_when(
      evochain_0 == "Egg" ~ evochain_4,
      english_name %in% eevee_evos ~ english_name,  # Fix Eevee evolutions
      english_name %in% hitmon_evos ~ english_name,  # Fix Tyrogue evolutions
      english_name == "Tyrogue" ~ NA_character_,  # Fix Tyrogue evolutions
      TRUE ~ evochain_2),
    evo_stage_2 = case_when(
      evochain_0 == "Egg" ~ evochain_6,
      english_name %in% hitmon_evos ~ NA_character_,  # Fix Tyrogue evolutions
      english_name == "Tyrogue" ~ NA_character_ , # Fix Tyrogue evolutions
      TRUE ~ evochain_4),
    evo_stage_3 = case_when(
      evochain_0 == "Egg" ~ NA_character_,
      english_name %in% hitmon_evos ~ NA_character_,  # Fix Tyrogue evolutions
      english_name == "Tyrogue" ~ NA_character_ , # Fix Tyrogue evolutions
      grepl("Cuff", evochain_6) ~ NA_character_,  # Remove Slowpoke "Galarian Cuff" error
      TRUE ~ evochain_6)) %>% 
  filter(!is.na(evo_stage_1)) %>%   # Only keep pokemon that evolve
  select(
    national_number, english_name, starts_with("evo_stage"),
    contains("type"), height_m, weight_kg)