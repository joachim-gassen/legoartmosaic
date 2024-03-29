source("utils.R")

lego_art_palettes <- read_lego_art_palettes()


ui <- fluidPage(
  titlePanel("The LEGO Art Mosaic Generator"),
  useShinyjs(),
  tags$script(HTML(paste(
    "Shiny.addCustomMessageHandler('note',", 
    "function(message) {alert(message);});"
  ))),
  HTML("<br><br><br>"),
  fluidRow(
    column(4, HTML("<br>")),
    column(8, HTML(
      "<p>The <a href=https://www.lego.com/en-us/themes/art/about>",
      "LEGO Arts theme sets</a> allow the construction of neat 48*48 mosaics.",
      "This app enables you to create such mosaics from your own images. You",
      "can start with an automated conversion and then customize it", 
      "manually as you please before downloading a TIFF file containing the",
      "mosaic image and a PDF file containing building instructions.",
      "For more information, please see this", 
      "<a href=https://bit.ly/legoamg>blog post</a>. Enjoy!</p>" 
    ))
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "typeof output.mosaic === 'undefined'",
        selectInput(
          "target", 
          "Select the LEGO Art set that you want to develop a mosaic for",
          choices = unique(lego_art_palettes$source),
          selected = "The Beatles"
        ),
        hr(),
        fileInput(
          "image_file", "Upload a square image file"
        )
      ),
      conditionalPanel(
        "typeof output.mosaic !== 'undefined'",
        actionButton(
          "rotate_image",
          "Rotate image 90° clockwise"
        ),
        helpText("Rotating will reset the image."),
        div(style="margin-bottom:10px"),
        hr(),
        checkboxInput(
          "use_orig_img",
          "Base conversion on unmodifed uploaded image",
          value = TRUE
        ),
        actionButton(
          "convert_image",
          "Convert image to mosaic"
        ),
        helpText(
          "This will take a while and will fail when your image",
          "contains an area with a single color that requires more",
          "tiles than available for any color.",
          "In this case, use the modified image and play around with the color",
          "reduction option or manually insert pixels that 'break' the large",
          "area prior to conversion."
        ),
        hr(),
        sliderInput(
          "number_of_colors", "Reduce image to how many colors?", 
          min = 2, max = 256, value = 24
        ),
        helpText("This will reset the image. Reducing the image colors", 
                 "makes manual conversion easier."),
        hr(),
        radioButtons(
          "sel_type", "Mouse selection type",
          c("Single" = "single",
            "Color" = "color",
            "Area" = "area",
            "All non-converted" = "all")
        ),
        helpText(
          "To select all pixels that still need a mosaic color,", 
          "select 'All non-converted'",
          "above and click somewhere on the image."
        ),
        actionButton(
          "auto_conversion", "Auto-convert selection"
        ),
        helpText(
          "Click if you want to convert the selected pixels",
          "to the best matching mosaic color. Ignores",
          "part restrictions."
        ),
        actionButton("sel_clear", "Clear selection"),
        uiOutput("ui_color_picker"),
        hr(),
        undoHistoryUI(
          "history", 
          back_text = "Undo last step",
          fwd_text = "Redo last step"
        ),
        actionButton("restore_original", "Restore original picture"),
        hr(),
        downloadButton("download_mosaic", "Mosaic as TIFF file"),
        hr(),
        downloadButton(
          "download_inst", "PDF with building instructions", 
        ),
        helpText(
          "Download becomes available when all image colors are converted to",
          "mosaic colors and all tile limitations are met."
        )
      )
    ),
    mainPanel(
      leafletOutput("mosaic", width = 500, height = 500),
      textOutput("selection_size")
    )
  ),
  fluidRow(
    column(
      12,
      hr(),
      HTML(
        "<p><center>Made with <a href=https://www.r-project.org>R</a>",
        "and <a href=https://shiny.rstudio.com>Shiny</a> by",   
        "<a href=https://twitter.com/JoachimGassen> Joachim Gassen</a>, 2021",
        "<br>Visit",
        "<a href=https://github.com/joachim-gassen/legoartmosaic>Github</a>",
        "for code and more.</center></p>")
    )
  )
)
