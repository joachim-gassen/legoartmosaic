library(raster)
library(tidyverse)
library(leaflet)
library(shiny)
library(shinyjs)
library(sf)
library(magick)


# devtools::install_github("gadenbuie/shinyThings")
library(shinyThings)


read_lego_art_palettes <- function() {
  if (!file.exists("data/lego_art_palettes.rds")) stop(
    "You need to source 'load_rebrickable_data.R' once to create palette data."
  )
  readRDS("data/lego_art_palettes.rds")
}


get_palette_from_image <- function(img, target_palette) {
  img_mat <- as.integer(img[[1]])/255
  hex <- as.vector(apply(
    img_mat, c(1, 2), function(x) rgb(x[1], x[2], x[3])
  ))
  a <- table(hex)
  img_palette <- tibble(
    source = "Image",
    name = "",
    hex = names(a),
    available = 0,
    used = unname(a)
  ) %>% 
    full_join(
      target_palette %>%
        rename(
          source_target = source,
          name_target = name,
          available_target = available),
      by = "hex"
    ) %>%
    mutate(
      name = ifelse(!is.na(name_target), name_target, name),
      source = ifelse(!is.na(name_target), source_target, source),
      available = ifelse(!is.na(name_target), available_target, 0),
      used = ifelse(!is.na(used), used, 0),
      value = row_number()
    ) %>%
    select(1:5, 9)
}


get_raster_values_from_image <- function(img, palette) {
  img_mat <- as.integer(img[[1]])/255
  hex <- as.vector(apply(
    img_mat, c(2, 1), function(x) rgb(x[1], x[2], x[3])
  ))
  return(match(hex, palette$hex))
}


color_distance <- function(x, y, method = "redmean") {
  if (method == "euclidian") {
    dist <- sqrt((y[1] - x[1])^2 + (y[2] - x[2])^2 + (y[3] - x[3])^2)
  } else if (method == "redmean") {
    # https://en.wikipedia.org/wiki/Color_difference
    dist <- ifelse(
      0.5*(y[1] + x[1]) < 128,
      sqrt(2*(y[1] - x[1])^2 + 4*(y[2] - x[2])^2 + 3*(y[3] - x[3])^2),
      sqrt(3*(y[1] - x[1])^2 + 4*(y[2] - x[2])^2 + 2*(y[3] - x[3])^2)
    )
  } else stop(sprintf("Method '%s' is not implemented."))
  dist
}


best_color_match <- function(x, y, method = "redmean") {
  if (length(x) == 1) data <- "col" else data <- "rgb"
  if (data == "col") {
    x <- col2rgb(x) 
    y <- col2rgb(y)
  }
  if (method == "euclidian") {
    dist <- sqrt((y[1,] - x[1])^2 + (y[2,] - x[2])^2 + (y[3,] - x[3])^2)
  } else if (method == "redmean") {
    # https://en.wikipedia.org/wiki/Color_difference
    dist <- ifelse(
      0.5*(y[1,] + x[1]) < 128,
      sqrt(2*(y[1,] - x[1])^2 + 4*(y[2,] - x[2])^2 + 3*(y[3,] - x[3])^2),
      sqrt(3*(y[1,] - x[1])^2 + 4*(y[2,] - x[2])^2 + 2*(y[3,] - x[3])^2)
    )
  } else stop(sprintf("Method '%s' is not implemented."))
  if (data == "col") return(y[match(min(dist), dist)])
  else return(y[, match(min(dist), dist)])
}


pick_text_color_for_bground <- function(color) {
  # https://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color
  rgb_code <- col2rgb(color)
  luminance <- (0.299*rgb_code[1] + 0.587*rgb_code[2] + 0.114*rgb_code[3])/255
  ifelse(luminance > 0.5, "#000000", "#ffffff")
}


mosaic_diff <- function(m, i) {
  diff <- 0
  for(x in 1:48) {
    for(y in 1:48) {
      diff <- diff + color_distance(m[x, y, ], i[x, y, ])
    }
  }
  diff/48^2
}


plot_mosaic <- function(img) {
  plot(c(100, 580), c(100, 580), type = "n", xlab = "", ylab = "")
  rasterImage(img/255, 100, 100, 580, 580, interpolate = FALSE)
}


save_mosaic <- function(img, file) {
  tiff::writeTIFF(img/255, file)
}


plot_instruction_pic <- function(img, palette, part = NULL) {
  hex <- as.vector(apply(
    img/255, c(2, 1), function(x) rgb(x[1], x[2], x[3])
  ))

  df <- expand_grid(
    y = 1:ncol(img),
    x = 1:nrow(img)
  ) %>% mutate(
    tile = factor(
      match(hex, palette$hex), 1:nrow(palette), 
      labels = paste0(1:nrow(palette), ": ", palette$name)
    )
  )
  
  df$y <- rev(df$y)
  df$color_text <- factor(
    sapply(palette$hex[df$tile], pick_text_color_for_bground),
    levels = c("#000000", "#ffffff")
  )
  
  ggplot(df, aes(x, y, fill = tile, label = as.integer(tile))) + 
    geom_point(pch=21, size=8, color = "black") + 
    scale_fill_manual(values = palette$hex, drop = FALSE) + 
    labs(
      x = "", y = "", fill = "Tile", 
      title = ifelse(!is.null(part), paste("Part", part), "")
    ) +
    geom_text(aes(
      color = color_text
    ), size = 3, show.legend = FALSE) + 
    scale_color_manual(values = c("black", "white"), drop = FALSE) +
    theme_minimal() 
}


plot_instruction_start <- function(img, palette) {
  hex <- as.vector(apply(
    img/255, c(2, 1), function(x) rgb(x[1], x[2], x[3])
  ))
  
  df <- expand_grid(
    y = 1:ncol(img),
    x = 1:nrow(img)
  ) %>% mutate(
    tile = factor(
      match(hex, palette$hex), 1:nrow(palette), 
      labels = paste0(1:nrow(palette), ": ", palette$name)
    )
  )
  
  df$y <- rev(df$y)

  pg <- tibble(
    xmin = c(0.5, 16.5, 32.5, 0.5, 16.5, 32.5, 0.5, 16.5, 32.5),
    xmax = c(16.5, 32.5, 48.5, 16.5, 32.5, 48.5, 16.5, 32.5, 48.5),
    ymin = c(32.5, 32.5, 32.5, 16.5, 16.5, 16.5, 0.5, 0.5, 0.5),
    ymax = c(48.5, 48.5, 48.5, 32.5, 32.5, 32.5, 16.5, 16.5, 16.5),
    label = 1:9
  )
  
  p <- ggplot() + 
    geom_point(
      data = df, aes(x, y, fill = tile), pch = 21, size = 2, color = "black"
    ) + 
    geom_rect(
      data = pg, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", alpha = 0.5, color = "black"
    ) + 
    geom_text(
      data = pg, 
      aes(x = xmin+(xmax - xmin)/2, y = ymin + (ymax - ymin)/2, label = label), 
      size = 8
    ) +
    scale_fill_manual(values = palette$hex, drop = FALSE) + 
    theme_void() +
    labs(
      x = NULL, y = NULL, title = NULL,
      caption = paste(
        "Created by the LEGO Art Mosaics shiny app.\n",
        "See https://github.com/joachim-gassen/legoartmosaic for more."
      )
    ) +
    theme(
      legend.position = "none", 
      plot.margin = margin(1, 1, 2, 7, "cm"),
      plot.title.position = "plot",
      plot.title = element_text(hjust = -0.4)
    )
  
  title.grob <- grid::textGrob(
    label = sprintf(
      "LEGO Art Mosaic based on '%s' set", unique(palette$source)
    ),
    x = unit(1, "lines"), 
    y = unit(-1, "lines"),
    hjust = 0, vjust = 0,
    gp = grid::gpar(fontsize = 16))
  
  gridExtra::arrangeGrob(p, top = title.grob)
}



produce_inst_pdf <- function(m, p, fname) {
  pdf(fname, width = 7.5, height = 6)
  grid::grid.draw(plot_instruction_start(m, p))
  for (r in 1:3) {
    for (c in 1:3) {
      print(plot_instruction_pic(
        m[(1 + 16*(r - 1)):(16*r), (1 + 16*(c - 1)):(16*c), ],
        p, part = (r - 1)*3 + c
      ))
    }
  }
  dev.off()
}
