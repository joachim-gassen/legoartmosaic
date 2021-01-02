# Libraries go to utils.R to make them available to 'ui.R' as well

source("utils.R")
source("convert_image.R")

# source("load_rebrickable_data.R")
lego_art_palettes <- read_lego_art_palettes()

server <- function(session, input, output){
  # On selecting multiple polygons in leaflet
  # https://stackoverflow.com/questions/41104576/changing-styles-when-selecting-and-deselecting-multiple-polygons-with-leaflet-sh
  rv <- reactiveValues(
    picture = NULL,
    image = NULL,
    sel_pixels = vector(),
    picked_color = NULL,
    color_palette = NULL,
    target_palette = NULL,
    pal = NULL
  )
  
  undo_app_state <- undoHistory("history", value = reactive({
    if (!is.null(rv$image)) {
      return(list(
        number_of_colors = input$number_of_colors,
        sel_type = input$sel_type,
        picture = rv$picture,
        image = rv$image,
        sel_pixels = rv$sel_pixels,
        picked_color = rv$picked_color,
        color_palette = rv$color_palette,
        target_palette = rv$target_palettte,
        pal = rv$pal
      ))
    } else return(NULL)  
  }))
  
  observe({
    req(undo_app_state())
    updateSliderInput(
      session, "number_of_colors", value = undo_app_state()$number_of_colors
    )
    updateRadioButtons(
      session, "sel_type", selected = undo_app_state()$sel_type
    )
    rv$picture <- undo_app_state()$picture
    rv$image <- undo_app_state()$image
    rv$sel_pixels <- undo_app_state()$sel_pixels
    rv$picked_color <- undo_app_state()$picked_color
    rv$color_palette <- undo_app_state()$color_palette
    rv$target_palettte <- undo_app_state()$target_palette
    rv$pal <- undo_app_state()$pal
  })
  

  color_picker_bground_color_css <- function(palette) {
    #  https://stackoverflow.com/questions/45375160/change-background-color-of-selectinput-in-r-shiny
    css <- ""
    for (i in 1:nrow(palette)) {
      css <- paste(css, sprintf(paste0(
        "#color_picker ~ .selectize-control .option:nth-child(%d) {",
        "background-color: %s;",
        "color: %s;}"
      ), i, palette$hex[i], pick_text_color_for_bground(palette$hex[i])))
    }
    css
  }
  
  select_flood_fill <- function(id, color_id = NULL) {
    # https://en.wikipedia.org/wiki/Flood_fill
    if (is.null(color_id)) color_id <- (rv$image$value[rv$image$id == id])
    if (rv$image$value[rv$image$id == id] != color_id) return()
    if (id %in% rv$sel_pixels) return()
    if (rv$image$value[rv$image$id == id] == color_id) {
      rv$sel_pixels <- c(rv$sel_pixels, id)
    }
    if (id %% 48 != 1) select_flood_fill(id - 1, color_id)
    if (id %% 48 != 0) select_flood_fill(id + 1, color_id)
    if (id > 48) select_flood_fill(id - 48, color_id)
    if (id <= 47 * 48) select_flood_fill(id + 48, color_id)
  }
  
  output$ui_color_picker <- renderUI({
    req(rv$color_palette)
    target_palette <- rv$color_palette %>% filter(source != "Image")
    choices_vec <- target_palette$value
    names(choices_vec) <- paste(
      target_palette$name,
      sprintf("(left: %d)", target_palette$available - target_palette$used)
    )
    tagList(
      tags$style(color_picker_bground_color_css(
        rv$color_palette %>% filter(source != "Image") 
      )),
      selectInput(
        inputId = "color_picker",
        label = "Pick color for selection", 
        choices = c("", choices_vec),
        selected = rv$picked_color
      )
    )
  })
  
  observeEvent(input$image_file, {
    rv$picture <- image_read(input$image_file$datapath) %>%
      image_resize("48")
  })
  
  observe({
    req(rv$picture, input$number_of_colors, input$target)
    pic <- rv$picture %>%
      image_quantize(max = input$number_of_colors) 
    
    rv$target_palette <- lego_art_palettes %>% filter(source == input$target)
    
    rv$color_palette <- get_palette_from_image(
      pic, rv$target_palette
    )
    
    hex <- isolate(rv$color_palette$hex)
    color_ids <- isolate(rv$color_palette$value)
    
    rv$pal <- colorFactor(hex, factor(color_ids, levels = color_ids))
    
    r <- raster(ncol = 48, nrow = 48, xmn = 0.1, xmx = 4.8, ymn = 0.1, ymx = 4.8)
    values(r) <- get_raster_values_from_image(pic, isolate(rv$color_palette))
    
    sp <- st_as_sf(rasterToPolygons(r))
    cn = st_coordinates(st_transform(st_centroid(sp), 4326))
    sp = st_transform(sp, 4326)
    sp = cbind(sp, cn)
    sp$id <- 1:nrow(sp)
    colnames(sp)[1] <- "value"
    
    rv$image = sp
  })
  
  # https://stackoverflow.com/questions/30704487/interactive-plotting-with-r-raster-values-on-mouseover
  output$mosaic <- renderLeaflet({
    req(rv$image)
    leaflet() %>%
      setView(2.4, 2.4, 7)
  })
  
  output$selection_size <- renderText({
    if (length(rv$sel_pixels) > 0) {
      sprintf(paste(
        "%d", ifelse(length(rv$sel_pixels) == 1, "pixel", "pixels"), "selected."
      ), length(rv$sel_pixels))
    } 
  })
  
  observeEvent(rv$image, {
    p <- leafletProxy("mosaic") %>%
      clearGroup("image") %>%
      addPolygons(
        data = rv$image, 
        stroke = FALSE,
        fillColor = ~rv$pal(rv$image$value), 
        fillOpacity = 1,
        layerId = rv$image$id,
        group = "image"
      )
    if (length(rv$sel_pixels) > 0) {
      sp_data <- rv$image[rv$image$id %in% rv$sel_pixels,]
      
      p <- p %>%
        clearGroup("selected") %>%
        addPolygons(
          data = sp_data,
          fillOpacity = 0,
          weight = 3,
          color = "red", 
          stroke = TRUE,
          group = "selected",
          layerId = sp_data$id
        )        
    }
    
    if (!any(rv$color_palette$source[rv$image$value] == "Image")) {
      enable("download_inst")
    } else {
      disable("download_inst")
    }
    
    p  
  })
  
  
  observeEvent(rv$sel_pixels, {
    if (length(rv$sel_pixels) > 0) {
      sp_data <- rv$image[rv$image$id %in% rv$sel_pixels,]
      
      leafletProxy("mosaic") %>%
        clearGroup("selected") %>%
        addPolygons(
          data = sp_data,
          fillOpacity = 0,
          weight = 3,
          color = "red", 
          stroke = TRUE,
          group = "selected",
          layerId = sp_data$id
        )        
    } else {
      leafletProxy("mosaic") %>%
        clearGroup("selected")      
    }
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$mosaic_shape_click, {
    click_id = input$mosaic_shape_click$id
    
    if (input$sel_type == "single") {
      if (click_id %in% rv$sel_pixels) {
        rv$sel_pixels <- rv$sel_pixels[! rv$sel_pixels %in% click_id]
      } else {
        rv$sel_pixels <- c(rv$sel_pixels, click_id)
      }
    } else if (input$sel_type == "color") {
      sel_color_id <- rv$image$value[rv$image$id == click_id]
      rv$sel_pixels <- rv$image$id[rv$image$value == sel_color_id]
    } else if (input$sel_type == "area") {
      rv$sel_pixels <- vector()
      select_flood_fill(click_id)
    } else if (input$sel_type == "all") {
      img_color_ids <- rv$color_palette %>% 
        filter(source == "Image") %>% 
        pull(value)
      rv$sel_pixels <- rv$image$id[rv$image$value %in% img_color_ids]
    }
    
    if (length(rv$sel_pixels) > 0) {
      sel_color_ids <- unique(rv$image$value[rv$image$id %in% rv$sel_pixels])
      if (length(sel_color_ids) == 1 & 
          rv$color_palette$source[sel_color_ids[1]] != "Image") {
        rv$picked_color <- sel_color_ids[1]
      } else rv$picked_color <- ""
    }
  })
  
  observeEvent(input$convert_image, {
    time_in <- Sys.time()
    withProgress({
      if (input$use_orig_img) {
        img <- as.integer(rv$picture[[1]])
        ncols <- NULL                  
      } else {
        img <- aperm(array(
          col2rgb(rv$color_palette$hex[rv$image$value]),
          dim = c(3, 48, 48)
        ), dim = c(2, 3, 1))
        ncols <- input$number_of_colors
      }
      ci <- try(
        optimize_mosaic(img, ncols, isolate(rv$target_palette)), 
        silent = FALSE
      )
    }, message = "Converting image...", value = 0.5)
    
    if (inherits(ci, "try-error")) {
      session$sendCustomMessage(
        type = 'note',
        message = paste(
          "The conversion failed. Sorry. This is likely because of a too large color", 
          "area or because the optimalization did not converge. Try to modify",
          "the image, breaking large color areas to see whether it helps."
        )
      )
    } else {
      if (Sys.time() - time_in > 30) {
        session$sendCustomMessage(
          type = 'note',
          message = paste(
            "The conversion timed out. The resulting mosaic will not be optimal.",
            "Try to modify the image, breaking large color areas to see whether",
            "it helps."
          )    
        )
      }
      hex <- as.vector(apply(
        ci/255, c(2, 1), function(x) rgb(x[1], x[2], x[3])
      ))
      rv$image$value <- match(hex, isolate(rv$color_palette$hex))
      rv$color_palette$used <- unname(
        table(factor(rv$image$value, 1:nrow(rv$color_palette)))
      )
    }
  })
  
  observeEvent(input$sel_clear, {
    req(rv$sel_pixels) 
    rv$sel_pixels <- vector()
    leafletProxy("mosaic") %>% 
      clearGroup("selected")
  })
  
  
  observeEvent(input$auto_conversion, {
    if (length(rv$sel_pixels) > 0) {
      sel_pixels <- rv$image$id %in% rv$sel_pixels
      old_colors <- rv$color_palette$hex[rv$image$value[sel_pixels]]
      avail_colors <- rv$color_palette %>% 
        filter(source != "Image") %>% pull(hex)
      new_colors <- sapply(
        unique(old_colors), best_color_match, avail_colors, USE.NAMES = F
      )
      new_values <- match(
        new_colors[match(old_colors, unique(old_colors))], rv$color_palette$hex
      )
      rv$image$value[rv$image$id %in% rv$sel_pixels] <- new_values
      rv$color_palette$used <- unname(
        table(factor(rv$image$value, 1:nrow(rv$color_palette)))
      )
      
      if (length(new_colors) == 1)  rv$picked_color <- new_colors
      else rv$picked_color_name <- ""
    }
  })
  
  
  observeEvent(input$color_picker, {
    if (length(rv$sel_pixels) > 0 & 
        input$color_picker != "" &
        !isolate(
          !is.null(rv$picked_color) && 
          (rv$picked_color == input$color_picker))) {
      
      color_id <- as.integer(input$color_picker)
      rv$image$value[rv$image$id %in% rv$sel_pixels] <- color_id
      
      rv$color_palette$used <- unname(
        table(factor(rv$image$value, 1:nrow(rv$color_palette)))
      )
      
      rv$picked_color <- color_id
    }
  })
  
  
  observeEvent(input$restore_original, {
    pic <- rv$picture %>%
      image_quantize(max = input$number_of_colors) 
    rv$image$value <- get_raster_values_from_image(pic, rv$color_palette) 
    rv$sel_pixels <- vector()
  })
  
  
  output$download_inst <- downloadHandler(
    filename = "mosaic_instructions.pdf",
    content = function(file) {
      img_mat <- aperm(array(
        col2rgb(rv$color_palette$hex[rv$image$value]),
        dim = c(3, 48, 48)
      ), dim = c(2, 3, 1))
      
      produce_inst_pdf(img_mat, rv$target_palette, file)
    }
  )
  
  output$download_mosaic <- downloadHandler(
    filename = "mosaic.tiff",
    content = function(file) {
      img_mat <- aperm(array(
        col2rgb(rv$color_palette$hex[rv$image$value]),
        dim = c(3, 48, 48)
      )/255, dim = c(2, 3, 1))
      
      tiff::writeTIFF(img_mat, file)
    }
  )
}
