library(tmap)
library(tidyverse)
library(readxl)
library(sf)

religion <- read_xlsx("religion.xlsx")

religion
religion_columns <- c("불교", "개신교", "천주교", "기타", "종교 없음")
plot_cols <- c("불교" = "#7161A3", "개신교" = "#F5A8B3", "천주교" = "#BE475A", 
               "기타" = "#B8B8B8", "종교 없음" = "#57C2DC")

# 파이차트를 grob으로 변환하는 함수 정의
create_grob <- function(data, region_name) {
  # 시도별 데이터 필터링 및 비율 계산
  region_data <- data %>%
    filter(sido == region_name) %>%
    select(all_of(religion_columns)) %>%
    summarise(across(everything(), sum)) %>%
    pivot_longer(cols = everything(), names_to = "religion", values_to = "count") %>%
    mutate(percentage = count / sum(count) * 100)
  
  # ggplot 객체 생성
  p <- ggplot(region_data, aes(x = "", y = percentage, fill = religion)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = plot_cols) +
    labs(title = region_name, fill = "종교") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "none")
  
  # ggplot 객체를 grob으로 변환
  ggplotGrob(p)
}

# 각 시도별 grob 객체 생성
grobs <- lapply(unique(religion$sido), function(region) create_grob(religion, region))

# 결과 확인 (grobs 리스트에 저장됨)
grobs

names(grobs) <- unique(religion$sido)
grobs
grid::grid.draw(grobs[[1]])

sido <- st_read("sido.shp", options = "ENCODING=EUC-KR")
sido <- sido |> st_transform(crs = 4326)

sido <- sido |> 
  rename(sido = SD_NM) |> 
  left_join(religion)

sido

# Mapping -----------------------------------------------------------------
tmap_mode("view")
p <- tm_shape(sido) + 
  tm_polygons() +
  tm_symbols(
    shape = "sido",
    shapes = grobs,
    border.lwd = 0,
    size = "total",
    scale = 2,
    legend.shape.show = FALSE,
    legend.size.is.portrait = TRUE
  )

tmap_save(p, "new_piemap.html")











