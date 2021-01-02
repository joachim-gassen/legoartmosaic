library(lpSolveAPI)

source("utils.R")



greedy_mosaic <- function(orig_img, ncols, mosaic_palette) {
  identify_area <- function(area, x, y, color = NULL) {
    # https://en.wikipedia.org/wiki/Flood_fill
    if (is_area[x, y] > 0) return()
    if (is.null(color)) color <- img[x, y, ]
    if (any(img[x, y, ] != color)) return()
    is_area[x, y] <<- area
    if (x > 1) identify_area(area, x - 1, y, color)
    if (x < 48) identify_area(area, x + 1, y, color)
    if (y > 1) identify_area(area, x, y - 1, color)
    if (y < 48) identify_area(area, x, y + 1, color)
  }
  
  
  color_of_area <- function(area) {
    area_pos <- which(is_area == area, arr.ind = TRUE)
    img[area_pos[1, 1], area_pos[1, 2], ]
  }
  
  if (!is.null(ncols)) {
    img <- as.integer((image_read(orig_img/255) %>%
                         image_quantize(ncols))[[1]])
  } else img <- orig_img
  
  is_area <- array(0, dim = c(48, 48))
  
  for(x in 1:48) {
    for(y in 1:48) {
      identify_area(max(is_area) + 1, x, y)
      if (min(is_area) > 0) break()
    }
    if (min(is_area) > 0) break()
  }
  
  area_size <- unname(as.vector(table(is_area)))
  areas_by_size <- as.integer(names(sort(table(is_area), decreasing = TRUE)))
  
  tiles <- rbind(col2rgb(mosaic_palette$hex), mosaic_palette$available)
  
  find_best_tiles_for_area <- function(area) {
    suitable_tiles <- tiles[, tiles[4,] >= area_size[area]]
    if (length(suitable_tiles) == 0) stop("No suitable tiles left. Sorry.")
    if (!is.array(suitable_tiles)) col = suitable_tiles[1:3]
    else col <- best_color_match(color_of_area(area), suitable_tiles[1:3,])
    tiles[4, colSums(tiles[1:3,] == col) == 3] <<- 
      tiles[4, colSums(tiles[1:3,] == col) == 3] - area_size[area]  
    col
  } 
  
  areas_color <- array(dim = c(3, length(area_size)))
  for(i in areas_by_size) {
    areas_color[, i] <- find_best_tiles_for_area(i)
  }
  
  mosaic <- array(dim = c(48, 48, 3))
  
  for(x in 1:48) {
    for(y in 1:48) {
      mosaic[x, y, ] <- areas_color[,is_area[x, y]]
    }
  }
  
  return(mosaic)
}

optimize_mosaic <- function(
  orig_img, ncols, mosaic_palette, timeout = 30, silent = FALSE
) {
  identify_area <- function(area, x, y, color = NULL) {
    # https://en.wikipedia.org/wiki/Flood_fill
    if (is_area[x, y] > 0) return()
    if (is.null(color)) color <- img[x, y, ]
    if (any(img[x, y, ] != color)) return()
    is_area[x, y] <<- area
    if (x > 1) identify_area(area, x - 1, y, color)
    if (x < 48) identify_area(area, x + 1, y, color)
    if (y > 1) identify_area(area, x, y - 1, color)
    if (y < 48) identify_area(area, x, y + 1, color)
  }
  
  
  color_of_area <- function(area) {
    area_pos <- which(is_area == area, arr.ind = TRUE)
    img[area_pos[1, 1], area_pos[1, 2], ]
  }
  
  if (!is.null(ncols)) {
    img <- as.integer((image_read(orig_img/255) %>%
                         image_quantize(ncols))[[1]])
    
  } else img <- orig_img

  tiles <- rbind(col2rgb(mosaic_palette$hex), mosaic_palette$available)  

  is_area <- array(0, dim = c(48, 48))
  
  for(x in 1:48) {
    for(y in 1:48) {
      identify_area(max(is_area) + 1, x, y)
      if (min(is_area) > 0) break()
    }
    if (min(is_area) > 0) break()
  }
  
  area_size <- unname(as.vector(table(is_area)))
  
  if (max(area_size) > max(mosaic_palette$available)) {
    stop(sprintf(paste(
      "Largest area requires %d tiles but largest color only",
      "provides %s tiles. Consider using more colors"
    ), max(area_size), max(mosaic_palette$available)))
  }

  areas <- max(is_area)
  colors <- nrow(mosaic_palette)

  lpm <- make.lp(colors + areas, areas*colors)  
  set.type(lpm, 1:(areas*colors), "binary")
  set.rhs(lpm, c(mosaic_palette$available, rep(1, areas))) 
  set.constr.type(lpm, c(rep("<=", colors), rep("=", areas)))
  dists <- vector("integer", areas*colors)
  
  for (i in 1:areas) {
    for (j in 1:colors) {
      set.column(lpm, (i-1)*colors + j, c(area_size[i], 1), c(j, colors + i))
      dists[(i-1)*colors + j] <- area_size[i]*color_distance(color_of_area(i), tiles[1:3, j])
    }
  }
  set.objfn(lpm, dists)
  lp.control(lpm, timeout = timeout)
  
  rv <- solve(lpm)
  if (rv == 7 | rv == 1) {
    if (!silent) message("linear optimization timed out")
    warning(sprintf(paste(
      "lp_solve timed out after %d seconds.", 
      "I will work with what I have got..."
    ), timeout))
  } else if(rv != 0) {
    if (!silent) message("linear optimization failed")
    stop(sprintf(paste(
      "Sorry but the linear optimization has failed with code %d.", 
      "Consider using more colors to generate smaller areas."
    ), rv))
  } else if (!silent) message("linear optimization suceeded")
  
  areas_matched_cols <- ((which(get.variables(lpm) == 1) - 1) %% colors) + 1


  areas_color <- array(dim = c(3, areas))
  for (i in 1:areas) areas_color[, i] <- col2rgb(
    mosaic_palette$hex[areas_matched_cols[i]]
  )

  mosaic <- array(dim = c(48, 48, 3))
  
  for(x in 1:48) {
    for(y in 1:48) {
      mosaic[x, y, ] <- areas_color[,is_area[x, y]]
    }
  }
  
  return(mosaic)
}

