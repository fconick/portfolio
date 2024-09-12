library(tidyverse)
library(ggrepel)
library(showtext)
library(png)
library(grid)


# Read the logo image
logo <- readPNG("nfl-logo-0.png")
logo_grob <- rasterGrob(logo, width = unit(1, "in"), height = unit(1, "in"))

color_palette = c('#1f77b4',  # Vibrant Teal
                 #'#ff9896',  # Orange
                 '#ff7f0e',  # Coral Red
                 '#2ca02c',  # Bright Green
                 '#9467bd'  # Purple
                 )

fpath = 'C:/USERS/FCO_N/APPDATA/LOCAL/MICROSOFT/WINDOWS/FONTS/RethinkSans-VariableFont_wght.ttf'

# Add Rethink Sans to your fonts
font_add(family = "Rethink Sans", regular = fpath)

# Enable the use of showtext for custom fonts
showtext_auto()

pac_df = read.csv('pca_df.csv')


cluster_names <- data.frame(
  Group = c(
  "Playmakers",   # Cluster 0
  "Mr. Consistents",          # Cluster 1
  "Lost Causes",              # Cluster 2
  "The Wild Card Crew"
),Cluster = c(0:3)) 

pac_df <- pac_df %>% 
  left_join(cluster_names)

head(pac_df)
theme_set(theme_minimal())



# Custom function to save charts ------------------------------------------

save_plot_as_png <- function(plot_to_save, 
                             file_name, 
                             plot_width = 600, 
                             plot_height = 300,
                             logo_file_name,
                             ...){
  #Create the file name to be saved as a png file
  the_file <- paste0(file_name, '.png')
  
  #Create space to add the logo in the plot
  the_plot <- plot_to_save + 
    ggplot2::labs(caption = '\n') 
  
  #1. Save the current chart 
  Cairo::CairoPNG(the_file, width = plot_width, height = plot_height, units = "px", res = 90)
  print(the_plot)
  dev.off()
  
  #2. Load the chart and the logo
  new_plot <- magick::image_read(the_file)
  logo_raw <- magick::image_read(paste0(logo_file_name, '.png'))
  
  #3. reshape the logo
  logo <- logo_raw %>%
    magick::image_scale("130") %>%
    magick::image_background('transparent', flatten = TRUE) 
  
  #4. Join both 
  final_chart <- magick::image_composite(new_plot, logo, gravity = 'southeast' )
  
  #5. Save
  magick::image_write(final_chart, the_file) 
}


# Chart -------------------------------------------------------------------

title_format =  "<img src = 'nfl-logo-0.png' height = 40 style = 'vertical-align = middle;'><span style='font-size: 20pt; vertical-align: middle; line-height: 40px;'>Quarterback Clusters: PCA Analysis in Action</span>"


p <- ggplot(pac_df, aes(x = PC1, y = PC2))+
geom_point(aes(color = Group), size = 5)  +
  ggrepel::geom_text_repel(aes(label = X), family = 'Rethink Sans', size = 3.5, box.padding = 0.5, point.padding = 0.5, color = '#666666') +
  labs(x = 'PC1 (QB Passing Ability)',
       y = 'PC2 (QB Running Ability)', 
       title = title_format, 
       subtitle = #"<img src = 'nfl-logo-0.png' height = 60><span style='font-size: 10pt'>
       "Categorizing NFL QBs into Five Distinct Groups Based on Principal Component Analysis and K-means for Performance Insights",#</span>", 
       color ="QB group") +
  scale_color_manual(values = color_palette) +
  theme_minimal(base_family = "Rethink Sans") +
  scale_y_continuous(breaks = 0,labels = NULL) +
  scale_x_continuous(breaks = 0, labels = NULL) +
  theme(
    text = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    # panel.border =element_rect(color = "gray", fill = NA, size = 0.5),
    axis.line.x = element_line(color = "gray", size = 0.5),
    axis.line.y = element_line(color = "gray", size = 0.5),
    
    plot.title = ggtext::element_markdown(), # Use Markdown for legend titles
    legend.text = element_text(size = 8), 
    # plot.title = element_text(size = 15, face = 'bold')#
  )

save_plot_as_png(plot_to_save = p,file_name = 'QB Groups',logo_file_name = '../logo_color',plot_width = 900, plot_height = 600)


