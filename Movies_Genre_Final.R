# Datasets Used
# https://www.kaggle.com/rounakbanik/the-movies-dataset
# https://www.kaggle.com/danielgrijalvas/movies
# https://data.world/sharon/bechdel-test

# Is there a relationship between the representation of women in film and a film’s genre?
# If so, how exactly have women been represented? How has this changed over time?


library(ggplot2)
library(readr)
library(plyr)
library(scales)

# Cleaning Up the large dataset
movies_metadata <- read_csv("movies_metadata.csv")
head(movies_metadata$genres, 20)

movies_metadata$genres_new <- gsub(",.*", "", movies_metadata$genres)
movies_metadata$genres_new <- gsub("[[:punct:]]", "", movies_metadata$genres_new)
movies_metadata$genres_new <- substr(movies_metadata$genres_new, 4, nchar(movies_metadata$genres_new))
movies_metadata$genres_new <- factor(movies_metadata$genres_new)

## Passing rate

bechdel_movies <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/bechdel/movies.csv")

# Split by year
bechdel_movies$year_cat <- cut(bechdel_movies$year,
  breaks = c(-Inf, 1989, 1999, Inf),
  labels = c("1970-1989", "1990-1999", "2000-2013")
)
bechdel_movies$year_cat <- factor(bechdel_movies$year_cat)

# Movies by Pass/Fail vs Year
movies_metadata$genres_new <- revalue(
  movies_metadata$genres_new,
  c(
    "10402" = "Music", "10749" = "Romance",
    "10751" = "Family", "10752" = "War",
    "10770" = "TV Movie", "12" = "Adventure",
    "14" = "Fantasy", "16" = "Animation",
    "18" = "Drama", "27" = "Horror",
    "28" = "Action", "35" = "Comedy",
    "36" = "History", "37" = "Western",
    "53" = "Thriller", "80" = "Crime",
    "878" = "Science Fiction", "9648" = "Mystery",
    "99" = "Documentary", "10769" = "Foreign",
    "e Aniplex" = NA, "e Carousel Productions" = NA,
    "e Odyssey Media" = NA
  )
)

movies_metadata_new <- movies_metadata[-c(
  which(movies_metadata$genres_new == ""),
  which(is.na(movies_metadata$genres_new))
), ]

new_movies <- merge(bechdel_movies, movies_metadata_new, by.x = "imdb", by.y = "imdb_id")

## Drop unused levels
new_movies$genres_new <- droplevels(new_movies$genres_new)

## Genres: Table and Combing Genres

new_movies$genres_new[476] <- "Family"
new_movies$genres_new[785] <- "Horror"

new_movies$genres_new[172] <- "Drama"
new_movies$genres_new[234] <- "Drama"
new_movies$genres_new[1301] <- "Action"
new_movies$genres_new[1484] <- "Action"

new_movies$genres_new[17] <- "Comedy"
new_movies$genres_new[542] <- "Animation"

new_movies$genres_new[614] <- "Romance"
new_movies$genres_new[1065] <- "Drama"
new_movies$genres_new[1340] <- "Drama"
new_movies$genres_new[1344] <- "Drama"
new_movies$genres_new[1420] <- "Crime"

new_movies$genres_new[893] <- "Adventure"
new_movies$genres_new[1722] <- "Drama"
new_movies$genres_new[407] <- "Drama"
new_movies$genres_new[57] <- "Comedy"
new_movies$genres_new[835] <- "Comedy"
new_movies$genres_new[227] <- "Comedy"
new_movies$genres_new[726] <- "Thriller"

new_movies <- new_movies[-c(
  which(new_movies$genres_new == "Music"), which(new_movies$genres_new == "Western"),
  which(new_movies$genres_new == "Documentary")
), ]

new_movies$genres_new <- droplevels(new_movies$genres_new)

new_movies$binary <- factor(new_movies$binary)
new_movies$new_binary <- factor(new_movies$binary, levels = rev(levels(new_movies$binary)))

freq_label <- unlist(tapply(
  new_movies$new_binary,
  new_movies$genres_new, table
)[names(sort(tapply(new_movies$genres_new, new_movies$binary, table)[[2]] /
  table(new_movies$genres_new)))]) /
  rep(tapply(new_movies$new_binary, new_movies$genres_new, length)[
    names(sort(tapply(new_movies$genres_new, new_movies$binary, table)[[2]] /
    table(new_movies$genres_new)))], each = 2)
freq_label_y <- freq_label
freq_label_y[seq(2, 26, by = 2)] <- 1

## Genres

## Visualization 1: Relative frequency of genres by passing the bechdel test
v1 <- ggplot(data = new_movies, aes(x = genres_new, fill = as.factor(binary))) +
  geom_bar(stat = "count", position = "fill") +
  scale_x_discrete(limits = names(sort(tapply(new_movies$genres_new, new_movies$binary,
                                              table)[[2]] / table(new_movies$genres_new)))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#778899", "#C90E17")) +
  labs(
    title = "Percentage of Movies Passing the Bechdel Test By Genre",
    subtitle = "Of the 1777 movies analyzed between 1970 and 2013, horror movies were most likely to pass the Bechdel Test",
    x = "Genre", y = "Passing Perentage", fill = "Pass/Fail",
    caption = "Source: Bechdel Test and Movies Dataset"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "ArialMT"), plot.title = element_text(size = rel(2), hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(size = rel(1)),
    axis.text.y = element_text(size = rel(1))
  ) +
  annotate("text",
    x = names(sort(tapply(new_movies$genres_new, new_movies$binary, table)[[2]] /
      table(new_movies$genres_new))),
    y = freq_label_y[seq(1, 26, by = 2)] - 0.025,
    label = percent(round(freq_label_y[seq(1, 26, by = 2)], 3)),
    color = "white", size = 3
  )
v1


## Visualization 2: Comparing Passing Rates by Genre by Decade

p1_1_x <- names(sort(tapply(new_movies$genres_new[which(new_movies$year_cat == "1970-1989")],
                            new_movies$binary[which(new_movies$year_cat == "1970-1989")], table)[[2]] /
                       table(new_movies$genres_new[which(new_movies$year_cat == "1970-1989")])))
p1_1_x[which(p1_1_x == "Science Fiction")] <- "Science \nFiction"
p1_1 <- ggplot(data = new_movies[which(new_movies$year_cat == "1970-1989"), ],
               aes(x = genres_new, fill = as.factor(binary))) +
  geom_bar(stat = "count", position = "fill") +
  scale_x_discrete(
    limits = names(sort(tapply(new_movies$genres_new[which(new_movies$year_cat == "1970-1989")],
                               new_movies$binary[which(new_movies$year_cat == "1970-1989")], table)[[2]] /
                          table(new_movies$genres_new[which(new_movies$year_cat == "1970-1989")]))),
    label = p1_1_x
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#778899", "#C90E17")) +
  labs(
    title = "1970-1989",
    x = "Genre", y = "Passing Percentage", fill = "Pass/Fail"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "ArialMT"), plot.title = element_text(size = rel(1.5), hjust = 0.5),
    axis.text.x = element_text(size = rel(1)), axis.text.y = element_text(size = rel(1))
  )

p2_1_x <- names(sort(tapply(new_movies$genres_new[which(new_movies$year_cat == "1990-1999")],
                            new_movies$binary[which(new_movies$year_cat == "1990-1999")], table)[[2]] /
                       table(new_movies$genres_new[which(new_movies$year_cat == "1990-1999")])))
p2_1_x[which(p2_1_x == "Science Fiction")] <- "Science \nFiction"
p2_1 <- ggplot(data = new_movies[which(new_movies$year_cat == "1990-1999"), ], aes(x = genres_new, fill = as.factor(binary))) +
  geom_bar(stat = "count", position = "fill") +
  scale_x_discrete(
    limits = names(sort(tapply(new_movies$genres_new[which(new_movies$year_cat == "1990-1999")],
                               new_movies$binary[which(new_movies$year_cat == "1990-1999")], table)[[2]] /
                          table(new_movies$genres_new[which(new_movies$year_cat == "1990-1999")]))),
    label = p2_1_x
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#778899", "#C90E17")) +
  labs(
    title = "1990-1999",
    x = "Genre", y = "Passing Percentage", fill = "Pass/Fail"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "ArialMT"), plot.title = element_text(size = rel(1.5), hjust = 0.5),
    axis.text.x = element_text(size = rel(1)), axis.text.y = element_text(size = rel(1))
  )


p3_1_x <- names(sort(tapply(new_movies$genres_new[which(new_movies$year_cat == "2000-2013")],
                            new_movies$binary[which(new_movies$year_cat == "2000-2013")], table)[[2]] /
                       table(new_movies$genres_new[which(new_movies$year_cat == "2000-2013")])))
p3_1_x[which(p3_1_x == "Science Fiction")] <- "Science \nFiction"
p3_1 <- ggplot(data = new_movies[which(new_movies$year_cat == "2000-2013"), ], aes(x = genres_new, fill = as.factor(binary))) +
  geom_bar(stat = "count", position = "fill") +
  scale_x_discrete(
    limits = names(sort(tapply(new_movies$genres_new[which(new_movies$year_cat == "2000-2013")],
                               new_movies$binary[which(new_movies$year_cat == "2000-2013")], table)[[2]] /
                          table(new_movies$genres_new[which(new_movies$year_cat == "2000-2013")]))),
    label = p3_1_x
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#778899", "#C90E17")) +
  labs(
    title = "2000-2013",
    x = "Genre", y = "Passing Percentage", fill = "Pass/Fail"
  ) +
  theme_bw() +
  theme(
    text = element_text(family = "ArialMT"), plot.title = element_text(size = rel(1.5), hjust = 0.5),
    axis.text.x = element_text(size = rel(1)), axis.text.y = element_text(size = rel(1))
  )


library(cowplot)

plot_row <- plot_grid(p1_1, p2_1, p3_1)
title <- ggdraw() +
  draw_label(
    "Passing Percentage by Year and Genre",
    fontface = "bold",
    x = 0,
    hjust = -1.25,
    size = 20
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
v2 <- plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

v2

