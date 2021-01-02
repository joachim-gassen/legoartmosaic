source("utils.R")
source("convert_image.R")

# Let's use Keith Richard's twitter handle image as an example
# (one could also directly use the URL in 'image_read()' but
# let's download the data to ensure reproducibility ;-)

download.file(
  "https://pbs.twimg.com/profile_images/925768757716373504/3SwABEqS_400x400.jpg",
  "data/keith_richards_400x400.jpg", mode = "wb"
)
magick_pic <- image_read("data/keith_richards_400x400.jpg") %>%
  image_resize("48")

img <- as.integer(magick_pic[[1]])


# Pick your Lego Arts set

mosaic_palette <- read_lego_art_palettes() %>% filter(source == "The Beatles")


# Plot original image

plot_mosaic(img)


# Unconstrained mosaic
# The greedy, large area priority algorithm is much faster

full_palette <- mosaic_palette %>% mutate(available = 48*48)
unconstrained_mosaic_greedy <- greedy_mosaic(img, NULL, full_palette)
plot_mosaic(unconstrained_mosaic_greedy)

# The linear programming solution finds the same mosaic

unconstrained_mosaic_lp <- optimize_mosaic(img, NULL, full_palette)
plot_mosaic(unconstrained_mosaic_lp)

all(unconstrained_mosaic_greedy == unconstrained_mosaic_lp)


# The 'greedy' mosaic based on tile restrictions for the full color picture

mosaic_greedy_all_colors <- greedy_mosaic(img, NULL, mosaic_palette)
plot_mosaic(mosaic_greedy_all_colors)


# And the same for the optimal LP mosaic - it is an improvement

mosaic_lp_all_colors <- optimize_mosaic(img, NULL, mosaic_palette)
plot_mosaic(mosaic_lp_all_colors)


# Does color reduction help to improve the conversion of the image?
# Let's see - this takes a while!

res <- tibble(
  ncols = 2:256,
  diff_greedy = NA,
  diff_optimized = NA
)

for (i in 2:256) {
  message(sprintf("Trying greedy mosaic with %d colors...", i))
  m <- try(greedy_mosaic(img, i, mosaic_palette))
  if(!(inherits(m, "try-error"))) res$diff_greedy[i - 1] <- mosaic_diff(img, m)
  message(sprintf("Trying optimized mosaic with %d colors...", i))
  m <- try(optimize_mosaic(img, i, mosaic_palette))
  if(!(inherits(m, "try-error"))) res$diff_optimized[i - 1] <- mosaic_diff(img, m)
}

res <- res %>% 
  filter(!is.na(diff_greedy) | !is.na(diff_optimized)) %>%
  pivot_longer(
    cols = starts_with("diff"), names_to = "method", values_to = "diff"
  ) 

ggplot(res, aes(ncols, diff, color = method)) + 
  geom_line() + 
  geom_hline(yintercept = mosaic_diff(img, unconstrained_mosaic_greedy), color = "red") +
  theme_classic() 

# It does not look as if but Keith is not a good case to test this because of
# the large black area
