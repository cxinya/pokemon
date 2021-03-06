library(tidyverse)


df <- read_csv("data/pokemon.csv") %>% 
  select(
    national_number, english_name, starts_with("evo"),
    contains("type"), height_m, weight_kg)


# ID problematic multiple evo stems ------------------------------------------------------

evo_chains <- c("evochain_0", "evochain_2", "evochain_4", "evochain_6")

# Eevee family is weird
eevee_evos <- df %>% filter(evochain_0 == "Eevee" & english_name != "Eevee") %>% 
  select("english_name") %>% unlist()

# Pkmn with multiple diff evo options
multi_evo <-  df %>% select(all_of(evo_chains)) %>% 
  distinct() %>% 
  mutate(basic_pkmn = evochain_0) %>% 
  pivot_longer(
    cols = all_of(evo_chains),
    names_to = "evo_chain",
    values_to = "english_name",
    ) %>% 
  drop_na(english_name) %>% 
  count(english_name) %>% 
  filter(n > 1) %>% 
  select(english_name) %>% unlist()

problem_evos <- df %>% filter(
    english_name %in% multi_evo |  # Same basic pkmn w multiple evo options
    evochain_0 == "Egg" |          # Eggs are not a basic pokemon!
    !is.na(evochain_6) |           # No pkmn evolves 3 times
    english_name %in% eevee_evos | # Eevee has weird evos
    grepl(" ", english_name)       # Look for weird names, possible typo
    ) %>%
  select(national_number, english_name, all_of(evo_chains)) %>% 
  arrange(evochain_0, national_number)

problem_evos %>% write_csv("data/problem-evolutions.csv", na = "")


# Fix problem evolutions --------------------------------------------------

# Manually Google correct evos and fix in CSV, keep rows from final form pkmn only

clean_evos <- df %>% select(national_number, english_name, all_of(evo_chains)) %>% 
  filter(!english_name %in% problem_evos$english_name) %>%
  rbind(read_csv("data/problem-evolutions-fixed.csv")) %>% 
  filter(!is.na(evochain_2)) %>%      # Keep only pkmn that evolve
  select(all_of(evo_chains)) %>% 
  distinct() %>% 
  mutate(
    evochain_0 = case_when(
      evochain_0 == "Nidoran?" & evochain_4 == "Nidoqueen" ~ "Nidoran\u2640",
      evochain_0 == "Nidoran?" & evochain_4 == "Nidoking" ~ "Nidoran\u2642",
      TRUE ~ evochain_0),
    basic_pkmn = evochain_0) %>%
  group_by(basic_pkmn) %>% 
  mutate(evo = row_number()) %>% 
  filter(!(basic_pkmn == "Swinub" & is.na(evochain_4))) %>%    # Mamoswine doubled in orig data
  pivot_longer(
    cols = all_of(evo_chains),
    names_to = "evo_stage",
    values_to = "english_name",
    ) %>% 
  drop_na(english_name) %>% 
  mutate(evo_stage = recode(evo_stage, 
    "evochain_0" = 0,
    "evochain_2" = 1,
    "evochain_4" = 2)) %>% 
  group_by(basic_pkmn, evo) %>% 
  mutate(max_stages = max(evo_stage)) %>% 
  left_join(select(df, english_name, national_number, weight_kg, primary_type)) %>% 
  left_join(select(df, english_name, basic_pkmn_num = national_number), by = c("basic_pkmn" = "english_name"))


# Calc % weight change ----------------------------------------------------

evo_weight <- clean_evos %>% 
  group_by(basic_pkmn, evo) %>% 
  mutate(per_change = weight_kg / lag(weight_kg, default = first(weight_kg)) - 1)

evo_weight %>% saveRDS("data/evolved-weights.Rds")
