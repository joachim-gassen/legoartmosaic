library(dplyr)
library(readr)

# https://rebrickable.com/downloads/ for data

rebrickable_data <- c("inventories", "sets", "inventory_parts", "lego_colors")
filepaths <- paste0("data/", rebrickable_data, ".csv")

if (any(!file.exists(filepaths))) stop(paste(
  "You need to download data form https://rebrickable.com/downloads/",
  "to 'data' directory first. See code for more details."
)) 

for (i in 1:length(rebrickable_data)) {
  assign(rebrickable_data[i], read_csv(filepaths[i], col_types = cols()))
}


lego_art_palettes <- sets %>%
  left_join(inventories, by = "set_num") %>%
  left_join(inventory_parts, by = c("id" = "inventory_id")) %>%
  filter(
    theme_id == 709,
    part_num == 98138 | part_num == 6141
  ) %>% 
  mutate(source = name) %>%
  select(source, color_id, quantity) %>%
  left_join(lego_colors, by = c("color_id" = "id")) %>%
  rename(available = quantity) %>%
  mutate(
    hex = paste0("#", rgb)
  ) %>% 
  select(source, name, hex, available)

saveRDS(lego_art_palettes, "data/lego_art_palettes.rds")

