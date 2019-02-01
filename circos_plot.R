
# install packages if as needed
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("here")
# install.packages("circlize")

# load libraries
library(dplyr)
library(tidyr)
library(here)
library(circlize)

# sample data
data <- data.frame(stringsAsFactors=TRUE,
                   empID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L,
                             14L, 15L, 16L, 17L, 18L, 19L, 20L),
                   Prior_Sector = c("Red", "Red", "Red", "Red", "Red", "Red", "Blue", "Blue",
                                    "Blue", "Blue", "Blue", "Blue", "Blue",
                                    "Blue", "Green", "Green", "Green", "Green", "Green",
                                    "Green"),
                   New_Sector = c("Red", "Blue", "Blue", "Blue", "Green", "Green", "Red",
                                  "Red", "Red", "Green", "Green", "Green",
                                  "Green", "Green", "Red", "Blue", "Green", "Green",
                                  "Green", "Green")
)

# If you are concerned about the order of the "groups", sectors in this case,
# in the plot, you need to assign the factor levels in the desired order
# otherwise they will appear in alphabetical order
data$Prior_Sector <- factor(data$Prior_Sector, levels=c("Red", "Blue", "Green"))
data$New_Sector <- factor(data$New_Sector, levels=c("Red", "Blue", "Green"))

# function for plotting
# No need to have this code in a function
# but it is helpful if you plan to make several plots 
plot.circos <- function(data_plot,prior,new,filename,colours=NULL) {
  
  # Turn the input data table (data_plot) into a crosstab
  tab <- data_plot %>%
    rename(Prior_Group = prior, New_Group = new) %>%
    group_by(Prior_Group, New_Group) %>%
    summarize(n = n()) %>%
    spread(key = "New_Group", value = "n", drop = FALSE) %>%
    ungroup()
  
  # Change any NAs to 0
  tab[is.na(tab)] <- 0
  
  # must be a matrix to use chordDiagram function 
  x <- tab %>%
    select(-Prior_Group) %>%
    as.matrix() 
  
  rownames(x) <- tab %>% 
    select(Prior_Group) %>% 
    pull()
  
  # create png file for the chart
  png(filename, width = 450, height = 450)
  
  # plot setup
  circos.clear()
  par(mar = rep(0, 4))
  circos.par(start.degree = 0, gap.degree = 6)
  
  # plot circos chart
  # could add customization here to change appearance of chart
  # see https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
  chordDiagram(x, grid.col = colours,annotationTrack=c("grid"), preAllocateTracks = list(track.height = 0.1))
  # Editing the first track to customize sector labels
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, 
                facing = "bending.outside",niceFacing = TRUE,cex=1.15)
  }, bg.border = NA)
  
  # close dev
  dev.off()
  circos.clear()
}

# call plot.circos and save a chart in your current working directory as circo_chart.png
plot.circos(data_plot=data,prior="Prior_Sector",new="New_Sector",filename=here("circos_chart.png"))


# Optional: ----
# assign plot colours (can use hex codes for more options)
grid.col <- c("red", "blue", "green")
# have the names match the "groups" in your data
names(grid.col) <- c("Red","Blue","Green")

# call plot.circos (as above) with colours defined as grid.col
plot.circos(data_plot=data,prior="Prior_Sector",new="New_Sector",filename=here("circos_chart.png"), colours = grid.col)

