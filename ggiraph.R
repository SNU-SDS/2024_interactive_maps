# Loading Packages --------------------------------------------------------

library(readxl)
library(sf)
library(tidyverse)
library(ggiraph)
library(patchwork)
library(tmap)
library(htmltools)
library(htmlwidgets)

# Data Preparing ----------------------------------------------------------

median <- read_xlsx("median.xlsx")
sgg <- st_read("sigungu.shp", options = "ENCODING=EUC-KR")
sd <- st_read("sido.shp", options = "ENCODING=EUR-KR")

sgg$SGG1_CD <- sgg$SGG1_CD |> 
  as.double()

sgg_median <- sgg |> 
  left_join(median, join_by(SGG1_CD == code))

# 37, 22, 26
median2 <- median |> 
  filter(grepl("^(22|26|37)", code)) |> 
  filter(!(code %in% c(37011, 37012)))
sgg_median2 <- sgg_median |> 
  filter(grepl("^(22|26|37)", SGG1_CD)) |> 
  mutate(
    tooltip = paste0(sgg, ": ", median, "세")
  )
  

write_rds(sgg_median, "sgg_median.rds")

sd_union <- sd |> 
  st_union() |> 
  st_sf()

# Plotting ----------------------------------------------------------------

sgg_median <- read_rds("sgg_median.rds")

p1 <- sgg_median2 |> 
  ggplot(aes(
    x = reorder(sgg, median),
    y = median,
    tooltip = paste0(sgg, ": ", median, "세"),
    data_id = sgg,
    fill = median)) +
  geom_col_interactive(data = filter(median2, !is.na(median))) +
  scale_fill_gradientn_interactive(colors = c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006837")) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )

p1

p2 <- ggplot() +
  geom_sf(data = sgg_median2) +
  geom_sf_interactive(
    data = filter(sgg_median2, !is.na(median)),
    aes(fill = median, tooltip = tooltip, data_id = sgg)
  ) +
  scale_fill_gradientn_interactive(colors = c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006837"),
                                   name = "중위연령(세)",
                                   breaks = c(41, 45, 49, 53, 57),  # 레전드에 표시할 값
                                   labels = c("41", "45", "49", "53", "57")) +
  coord_sf(crs = st_crs(5179)) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 3),
    legend.key.size = unit(0.3, 'cm')
  )

p2

combined_plot <- (p1 + p2) + plot_layout(heights = c(1, 1), widths = c(1, 2))

combined_plot

# css options

tooltip_css <- "
  background: linear-gradient(145deg, #f0f0f0, #e6e6e6);
  border: none;
  border-radius: 12px;
  box-shadow: 3px 3px 10px #d1d1d1, -3px -3px 10px #ffffff;
  color: #333;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  font-size: 14px;
  padding: 12px;
  transition: all 0.5s ease-out;
"

hover_css <- "
  filter: brightness(90%);
  cursor: pointer;
  filter: brightness(1.2) drop-shadow(0 0 5px rgba(78, 84, 200, 0.5));
  transition: all 0.5s ease-out;
"

# add interactivity

interactive_plot <- girafe(ggobj = combined_plot)
interactive_plot <- interactive_plot |> girafe_options(
  opts_hover(css = hover_css),
  opts_tooltip(css = tooltip_css),
  opts_hover_inv(css = "opacity:0.5; transition: all 0.2s ease-out;")
  )

interactive_plot
saveWidget(interactive_plot, "ggiraph.html", selfcontained = TRUE)

# htmltools::save_html(interactive_plot, "ggiraph.html")
