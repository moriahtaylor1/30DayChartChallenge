#ALL OF THIS CODE COMES FROM MICHAEL FREEMAN (@mf_viz on Twitter)
#I did not write this code, I just chose the image


#packages
library(imager)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggvoronoi)
library(ragg)

#polar stratospheric clouds
#image from h0rdur on instagram
img <- load.image(file = "h0rdur.png")

# Represent the image as a data frame
img_df <- as.data.frame(img)

# Show a table of the first 10 rows of the data frame
img_df %>% 
  arrange(x, y, cc) %>% # sort by columns for viewing
  filter(row_number() < 10) %>% # Select top 10 columns
  kable("html") %>%  # Display table in R Markdown
  kable_styling(full_width = F) # Don't take up full width

# Add more expressive labels to the colors
img_df <- img_df %>% 
  mutate(channel = case_when(
    cc == 1 ~ "Red",
    cc == 2 ~ "Green", 
    cc == 3 ~ "Blue"
  ))

# Reshape the data frame so that each row is a point
img_wide <- img_df %>%
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(
    color = rgb(Red, Green, Blue)
  )
# Take a sample of rows from the data frame
sample_size <- 5000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]

# Plot only the sampled points
points <- ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color)) +
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

# Create a Voronoi Diagram of the sampled points
voronoi <- ggplot(img_sample) +
  geom_voronoi(mapping = aes(x = x, y = y, fill = color)) +
  scale_fill_identity() +
  scale_y_reverse() +
  theme_void()

#save plots
ggsave("Day10-Abstract-Points.png",
       plot = points,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))

#save plots
ggsave("Day10-Abstract-Voronoi.png",
       plot = voronoi,
       device = agg_png(width = 7, height = 5, units = "in", res = 300))