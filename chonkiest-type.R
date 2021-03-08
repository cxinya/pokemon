library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Press Start 2P", "2p")
font_add_google("Lato", "lato")
showtext_auto()


chonk <- read_rds("data/evolved-weights.Rds") %>% 
  mutate(primary_type = str_to_title(primary_type))


# Palette -----------------------------------------------------------------

type_hex <- tibble::tribble(
       ~type,       ~hex,
       "Bug",  "#94BC4A",
      "Dark",  "#736C75",
    "Dragon",  "#6A7BAF",
  "Electric",  "#E5C531",
     "Fairy",  "#E397D1",
  "Fighting",  "#CB5F48",
      "Fire",  "#EA7A3C",
    "Flying",  "#7DA6DE",
     "Ghost",  "#846AB6",
     "Grass",  "#71C558",
    "Ground",  "#CC9F4F",
       "Ice",  "#70CBD4",
    "Normal",  "#AAB09F",
    "Poison",  "#B468B7",
   "Psychic",  "#E5709B",
      "Rock",  "#B2A061",
     "Steel",  "#89A1B0",
     "Water",  "#539AE2"
  )


# Data - chonkiness by type --------------------------------------------------------------------


chonk_type <- chonk %>% group_by(primary_type, max_stages, evo_stage) %>% 
  filter(basic_pkmn != "Cosmog") %>% 
  summarize(avg_weight_kg = mean(weight_kg)) %>% 
  ungroup() %>% 
  group_by(max_stages, evo_stage) %>% 
  arrange(avg_weight_kg) %>% 
  mutate(show = case_when(
    evo_stage == 2 & max_stages == 2 & (row_number() <= 3 | row_number() >= 16) ~ 1,
    evo_stage == 1 & max_stages == 1  & (row_number() <= 3 | row_number() >= 16) ~ 1,
    TRUE ~ 0))


# Plot two evolutions --------------------------------------------------------------------

heaviest_type_2evo <- chonk %>% filter(max_stages == 2) %>% 
  ggplot(aes(x = evo_stage, y = weight_kg)) +
  geom_path(aes(group = basic_pkmn), alpha = .3, size = .2) +
  geom_path(data = filter(chonk_type, max_stages == 2),
    aes(x = evo_stage, y = avg_weight_kg, color = primary_type),
    size = .7) + 
  ggrepel::geom_text_repel(data = filter(chonk_type, max_stages == 2),
    aes(x = evo_stage, y = avg_weight_kg,
        color = primary_type, label = ifelse(show == 1, primary_type, "")),
    hjust = 0, size = 11, show.legend = F, family = "2p", xlim = c(2.03, 2.35)) +
  labs(color = "Type", title = "TWO EVOLUTIONS") +
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
  scale_color_manual(values = type_hex$hex, labels = type_hex$type) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 2))) +
  theme_void() +
  theme(
    text = element_text(family = "2p"),
    axis.text.y = element_text(size = 30, margin = margin(r = 5)),
    axis.text.x = element_text(size = 35, vjust = 1, lineheight = .4, margin = margin(t = 13)),
    axis.line = element_line(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(.2, "cm"),
    legend.title = element_text(size = 35, hjust = .5),
    legend.text = element_text(size = 35),
    legend.position = c(.73, .8),
    legend.box.background = element_rect(size = 1.25),
    legend.box.margin = margin(rep(10, 4)),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.margin = margin(t = 20, r = 10, b = 15),
    panel.border = element_blank(),
    plot.title = element_text(size = 50, margin = margin(b = 25)),
  )


# Plot one evo ------------------------------------------------------------


heaviest_type_1evo <- chonk %>% filter(max_stages == 1) %>% 
  ggplot(aes(x = evo_stage, y = weight_kg)) +
  geom_path(aes(group = basic_pkmn), alpha = .3, size = .2) +
  geom_path(data = filter(chonk_type, max_stages == 1),
    aes(x = evo_stage, y = avg_weight_kg, color = primary_type),
    size = .7) + 
  ggrepel::geom_text_repel(data = filter(chonk_type, max_stages == 1),
    aes(x = evo_stage, y = avg_weight_kg,
        color = primary_type, label = ifelse(show == 1, primary_type, "")),
    hjust = 0, size = 11, show.legend = F, family = "2p", xlim = c(1.03, 1.35)) +
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
  scale_color_manual(values = type_hex$hex, labels = type_hex$type) +
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

heaviest_type <- heaviest_type_1evo + heaviest_type_2evo +
  plot_layout(widths = c(1.25, 2)) + 
  plot_annotation(
    title = "HOW MUCH CHONKIER DO\nPOK\u00E9MON GET AS THEY EVOLVE?",
    subtitle = "TONNAGE BY TYPE",
    caption = "Data: @cristobalmitchell & Serabii  |  Viz: Xin Yuen @so_xinteresting",
    theme = theme(
      plot.title = element_text(family = "2p", size = 80, hjust = .5, lineheight = .4, margin = margin(t = 10, b = 10)),
      plot.subtitle = element_text(family = "2p", size = 65, hjust = .5, lineheight = .4, margin = margin(t = 10, b = 25)),
      plot.caption = element_text(family = "lato", size = 40, hjust = .5, margin = margin(t = 20)),
      plot.background = element_rect(fill = "#528bac", color = "black", size = 1),
      plot.margin = margin(r = 30, l = 30, b = 20, t = 40),
        ))

ggsave("figs/heaviest_type_final.png", height = 15, width = 15)
