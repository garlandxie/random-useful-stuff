setwd("C:/Users/xie_c/Desktop")

# libraries -------------------------------------------------------------------
library(tibble)
library(ggplot2)
library(svglite)

# plot: wolve packs -----------------------------------------------------------
num_wolves_packs <- tribble(
  ~year, ~num_wolves, ~num_packs,
  1995, 21, 3,
  1996, 51, 9,
  1997, 86, 7,
  1998, 112, 11, 
  1999, 118, 11, 
  2000, 119, 8, 
  2001, 132, 10, 
  2002, 148, 14,
  2003, 174, 3,
  2004, 171, 16, 
  2005, 118, 13, 
  2006, 136, 13,
  2007, 171, 11, 
  2008, 124, 12,
  2009, 96, 14,
  2010, 97, 11,
  2011, 98, 10,
)

plot_counts <- ggplot(data = num_wolves_packs) +
  geom_point(aes(x = year, y = num_wolves, col = "red")) +
  geom_line(aes(x = year, y = num_wolves, col = "red")) +
  geom_point(aes(x = year, y = num_packs, col = "blue")) + 
  geom_line(aes(x = year, y = num_packs, col = "blue")) +
  labs(x = "Year",
       y = "Abundance") +
  scale_color_discrete(
    name = NULL,
    labels = c("packs", "wolves")
  ) + 
  theme_bw()

ggsave(
  plot = plot_counts, 
  file = "plot_counts.png",
  device = "png",
  width = 10, 
  height = 10
)

ggsave(
  plot = plot_counts, 
  file = "plot_counts.svg",
  device = "svg",
  width = 10, 
  height = 10
)

# plot: livestock -------------------------------------------------------------

livestock <- tribble(
~Year, ~Cows, ~Dogs, ~Goats, ~Sheep, ~Foal,
1997, 6, 0, 0, 68, 0,
1998, 3, 1, 0, 0, 0,
1999, 4, 6, 0, 13, 1,
2000, 7, 8, 0, 39, 0,
2001, 22, 4, 0, 117, 0,
2002, 37, 1, 0, 74, 0,
2003, 42, 0, 10, 85, 0,
2004, 74, 4, 2, 81, 0,
)
 '' 
plot_livestock <- ggplot(data = livestock) +
  geom_line(aes(x = Year, y = Cows, col = "Cows")) +
  geom_line(aes(x = Year, y = Dogs, col = "Dogs")) +
  geom_line(aes(x = Year, y = Goats, col = "Goats")) +
  geom_line(aes(x = Year, y = Sheep, col = "Sheep")) +
  geom_line(aes(x = Year, y = Foal, col = "Foal")) +
  scale_color_discrete(name = "Livestock") + 
  labs(x = "Year", 
       y = "Abundance") +
  theme_bw() 

ggsave(
  plot = plot_livestock, 
  file = "plot_livestock.svg",
  device = "svg",
  width = 10, 
  height = 10
)


