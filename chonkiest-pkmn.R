library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Press Start 2P", "2p")
font_add_google("Lato", "lato")
showtext_auto()


chonk <- read_rds("data/evolved-weights.Rds") %>% 
  mutate(primary_type = str_to_title(primary_type))


# Data --------------------------------------------------------------------


chonkiest <- chonk %>% group_by(max_stages) %>% 
  filter(basic_pkmn != "Cosmog") %>% 
  arrange(desc(weight_kg)) %>% 
  filter(
    evo_stage == 2 & max_stages == 2 & row_number() <= 3 |
    evo_stage == 1 & max_stages == 1  & row_number() <= 5) %>% 
  ungroup() %>% 
  mutate(img = str_glue("sprites/{tolower(english_name)}.png"))  # https://pokemondb.net/sprites/


chonkiest_fam <- chonk %>% filter(basic_pkmn %in% chonkiest$basic_pkmn)  %>% 
  select(-english_name) %>% 
  left_join(select(chonkiest, basic_pkmn, english_name, img)) %>% 
  mutate(display_label = case_when(
    evo_stage == 2 & max_stages == 2 ~ 1,
    evo_stage == 1 & max_stages == 1 ~ 1,
    TRUE ~ 0)) %>% 
  mutate(img = ifelse(display_label == 1, img, NA))


# Chonkiest pkmn colors from sprites -----------------------------------------------------------------

chonk_hex <- tibble::tribble(
  ~english_name,      ~hex,
      # "Cosmoem", "#7653A6",
      "Avalugg", "#9FB0CA",
     "Mudsdale", "#A87B5D",
     "Melmetal", "#A9AFAC",
   "Copperajah", "#FF8847",
    "Metagross", "#313962",
      "Snorlax", "#315a7b",
       "Aggron", "#6a6a6a",
    "Coalossal", "#FF4E3F")

pal <- chonk_hex$hex
names(pal) <- chonk_hex$english_name



# Cosmoem -----------------------------------------------------------------


cosmoem <- chonk %>% filter(english_name == "Cosmoem") %>% 
  head(1) %>% 
  mutate(img = str_glue("sprites/{tolower(english_name)}.png"))


# Plot two evolutions --------------------------------------------------------------------


heaviest_pkmn_2evo <- chonk %>% filter(max_stages == 2) %>% 
  ggplot(aes(x = evo_stage, y = weight_kg)) +
  geom_path(aes(group = basic_pkmn), alpha = .3, size = .2) +
  geom_path(data = filter(chonkiest_fam, max_stages == 2),
    aes(x = evo_stage, y = weight_kg, color = english_name),
    size = .7) + 
  ggrepel::geom_text_repel(data = filter(chonkiest_fam, max_stages == 2),
    aes(x = evo_stage, y = weight_kg, color = english_name, 
        label = ifelse(display_label == 1, english_name, "")),
    hjust = 0, size = 11, show.legend = F, family = "2p", xlim = c(2.03, 2.35)) +
  geom_text(data = cosmoem,                                     # Cosmoem
    aes(label = english_name, x = 1.05, y = weight_kg),
    size = 11, color = "#7653A6", family = "2p", hjust = 0) +
  ggimage::geom_image(data = filter(chonkiest_fam, max_stages == 2),
    aes(image = img, x = 2.2,
        y = ifelse(english_name == "Coalossal", weight_kg - 60, weight_kg + 60)),
    size = .12, asp = 1) +
  ggimage::geom_image(data = cosmoem,
    aes(image = img, x = 1.2,
        y = weight_kg - 60),
    size = .12, asp = 1) +
  labs(title = "TWO EVOLUTIONS") +
  scale_y_continuous(
    limits = c(0, 1010),
    breaks = seq(0, 1000, 250),
    labels = format(seq(0, 1000, 250), big.mark = ","),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(0, 2.35),
    breaks = c(0, 1, 2),
    labels = c("Basic\nPok\u00E9mon", "Evolution 1", "Evolution 2\n(final form)")
  ) +
  scale_color_manual(values = pal) +
  theme_void() +
  theme(
    text = element_text(family = "2p"),
    axis.text.y = element_text(size = 30, margin = margin(r = 5)),
    axis.text.x = element_text(size = 35, vjust = 1, lineheight = .4, margin = margin(t = 13)),
    axis.line = element_line(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(.2, "cm"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = margin(t = 20, r = 10, b = 15),
    panel.border = element_blank(),
    plot.title = element_text(size = 50, margin = margin(b = 25)),
  )

# Plot one evo ------------------------------------------------------------


heaviest_pkmn_1evo <- chonk %>% filter(max_stages == 1) %>% 
  ggplot(aes(x = evo_stage, y = weight_kg)) +
  geom_path(aes(group = basic_pkmn), alpha = .3, size = .2) +
  geom_path(data = filter(chonkiest_fam, max_stages == 1),
    aes(x = evo_stage, y = weight_kg, color = english_name),
    size = .7) + 
  ggrepel::geom_text_repel(data = filter(chonkiest_fam, max_stages == 1),
    aes(x = evo_stage, y = weight_kg,
        color = english_name,
        label = case_when(
          english_name == "Copperajah" & display_label == 1 ~ "Copper-\najah",
          display_label == 1 ~ english_name,
          TRUE ~ "")),
    hjust = 0, size = 11, family = "2p", xlim = c(1.03, 1.3), lineheight = .3) +
  ggimage::geom_image(data = filter(chonkiest_fam, max_stages == 1),
    aes(image = img, x = 1.17,
        y = ifelse(english_name == "Snorlax", weight_kg - 60, weight_kg + 60)),
    size = .15, asp = 0.5) +
  labs(y = "Weight (kg)", title = "ONE EVOLUTION") +
  scale_y_continuous(
    limits = c(0, 1010),
    breaks = seq(0, 1000, 250),
    labels = format(seq(0, 1000, 250), big.mark = ","),
    expand = c(0,0)) +
  scale_x_continuous(
    limits = c(0, 1.35),
    breaks = c(0, 1),
    labels = c("Basic\nPok\u00E9mon", "Evolution 1\n(final form)")
  ) +
  scale_color_manual(values = pal) +
  theme_void() +
  theme(
    text = element_text(family = "2p"),
    axis.text.y = element_text(size = 30, margin = margin(r = 5)),
    axis.text.x = element_text(size = 35, vjust = 1, lineheight = .4, margin = margin(t = 13)),
    axis.title.y = element_text(size = 35, angle = 90, margin = margin(r = 7)),
    axis.line = element_line(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(.2, "cm"),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = margin(t = 20, l = 15, b = 15),
    plot.title = element_text(size = 50, margin = margin(b = 25))
  )


# Combine plots -----------------------------------------------------------

heaviest_pkmn <- heaviest_pkmn_1evo + heaviest_pkmn_2evo +
  plot_layout(widths = c(1.25, 2)) + 
  plot_annotation(
    title = "HOW MUCH CHONKIER DO\nPOK\u00E9MON GET AS THEY EVOLVE?",
    subtitle = "THE HEAVYWEIGHTS",
    caption = "Data: @cristobalmitchell & Serabii  |  Viz: Xin Yuen @so_xinteresting",
    theme = theme(
      plot.title = element_text(family = "2p", size = 80, hjust = .5, lineheight = .4, margin = margin(t = 10, b = 10)),
      plot.subtitle = element_text(family = "2p", size = 65, hjust = .5, lineheight = .4, margin = margin(t = 10, b = 25)),
      plot.caption = element_text(family = "lato", size = 40, hjust = .5, margin = margin(t = 20)),
      plot.background = element_rect(fill = "#315a7b"),
      plot.margin = margin(r = 30, l = 30, b = 20, t = 40),
        ))

ggsave("figs/heaviest_pkmn_final.png", height = 15, width = 15)
