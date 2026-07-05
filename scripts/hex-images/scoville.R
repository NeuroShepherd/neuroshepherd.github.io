# Scoville Scale Hex Sticker
# A hex sticker representing the Scoville heat scale,
# themed with jalapeño green and habanero orange.

library(hexSticker)
library(ggplot2)
library(dplyr)
library(scales)
library(sysfonts)
library(showtext)

font_add_google("Barlow", "barlow", regular.wt = 800)
font_add_google("Barlow Condensed", "barlow-condensed")
showtext_auto()

# --- Gradient: jalapeño green -> habanero orange ---
n <- 300
cols <- colorRampPalette(c(
  "#3D7A33",
  "#6B9E2E",
  "#9FC524",
  "#E8C800",
  "#FF9E00",
  "#FF6B00",
  "#E04500"
))(n)


# Bias distribution toward higher values: power < 1 puts more lines at the top
lin_seq <- seq(0, 1, length.out = n)
y_vals <- 10^(lin_seq^0.4 * log10(3e6))

# Continuous background gradient
# Use fill_val = ymid^1.5 so the color gradient is even on the visual scale
bg <- tibble(
  ymin = y_vals,
  ymax = lead(y_vals, default = 3e6),
  idx = seq_len(n)
) |>
  mutate(
    ymid = (ymin + ymax) / 2,
    fill_val = ymid^1.5
  )

p <- bg |>
  ggplot() +
  geom_rect(
    aes(xmin = 0, xmax = 2, ymin = ymin, ymax = ymax, fill = fill_val),
    alpha = 0.7
  ) +
  scale_fill_gradientn(colors = cols) +
  scale_y_continuous(
    trans = trans_new(
      name = "compress-low",
      transform = function(x) x^1.5,
      inverse = function(x) x^(1 / 1.5)
    ),
    breaks = c(1e6, 2e6, 3e6),
    labels = c("1M", "2M", "3M")
  ) +
  coord_cartesian(xlim = c(0, 2), expand = FALSE) +
  theme_void() +
  theme(
    axis.text.y = element_text(
      family = "barlow-condensed",
      size = 18,
      color = "#FFFFFF",
      hjust = 1,
      margin = margin(r = 3)
    ),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none",
    plot.margin = margin(2, 6, 2, 2)
  )

# --- Generate and save the sticker ---
sticker(
  p,
  package = "Scoville",
  p_family = "barlow",
  p_y = 1.55,
  p_size = 20,
  p_color = "#FFFFFF",
  h_fill = "#9bc79b",
  h_color = "#3D7A33",
  h_size = 1.5,
  s_x = 0.95,
  s_y = 0.85,
  s_width = 1.3,
  s_height = 0.95,
  spotlight = TRUE,
  l_alpha = 0.3,
  url = "scoville.scale",
  u_color = "#9FC524",
  u_size = 3.5,
  filename = "scripts/hex-images/scoville.png",
  dpi = 300
)
