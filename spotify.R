library(dplyr)
library(spotifyr)
library(plotly)
library(ggplot2)
library(ggridges)
library(ggrepel)
library(cowplot)
library(viridis)
library(forcats)
library(plyr)


## Get access

Sys.setenv(SPOTIFY_CLIENT_ID = "136caa63205547f49bf6baa77a2028fa")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "55b3b07ee7794eab9a0bad5d15340a5a")
access_token <- get_spotify_access_token()

thenational <- get_artist_audio_features("The National") 

thenational_unique <- thenational %>% distinct(track_name, .keep_all = TRUE)

## Label Names 

top5perc_valence <- thenational_unique %>%
  top_frac(0.03, valence)

bottom5perc_valence <- thenational_unique %>%
  top_frac(-0.03, valence)

top5perc_dancability <- thenational_unique %>% 
  top_frac(0.03, danceability)

bottom5perc_dancability <- thenational_unique %>% 
  top_frac(-0.03, danceability)

labels_tracknames <- rbind(top5perc_dancability,top5perc_valence, bottom5perc_dancability, bottom5perc_valence) %>% 
  distinct(track_name, .keep_all = TRUE) %>% 
  select(track_name, valence, danceability)

## Clean Album Names

albumnames <- thenational_unique %>% distinct(album_name)

thenational_unique$album_name <- revalue(thenational_unique$album_name, c("Juicy Sonic Magic, Live in Berkeley, September 24-25, 2018" = "Juicy Sonic Magic -- Live in Berkeley"))
thenational_unique$album_name <- revalue(thenational_unique$album_name, c("Boxer Live in Brussels" = "Boxer -- Live in Brussels"))



## Valence vs Dancability

nationaltracks_valence_dancability <- 
  ggplot() +
  geom_vline(aes(xintercept = 0.5), colour = "grey") +
  geom_hline(aes(yintercept = 0.5), colour = "grey") +
  geom_point(data= thenational_unique, aes(valence, danceability, color = album_name), size = 2.5) + #, color = album_name
  geom_label_repel(data = labels_tracknames, aes(valence, danceability, label = track_name),  
                   hjust = 0, nudge_x = 0.01) +
  scale_color_viridis(discrete=TRUE, option = "magma")  +
  theme_minimal_grid() +
  theme(legend.position="bottom") +
  labs(title = "The National Songs by Dancability and Valence", subtitle = "based on Spotifys Web API with spotifyr", color = "Album Name")


## Ridgeplot 

density_valence <- thenational_unique %>% 
  mutate(album_name = fct_reorder(album_name, album_release_year)) %>%
  ggplot(aes(x = valence, y = album_name, fill = stat(x))) + 
  geom_density_ridges_gradient() +
  scale_fill_viridis(name = "Valence", option = "magma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_ridges() +
  labs(title = "Valence distribution by the National Albums", x = "Valence" , y = "Album Name (ordered by year)")

density_valence_sub <- add_sub(density_valence, "Data by Spotify via spotifyr package; #tidytuesday by @rkbruegger",
                               x = 0, hjust = 0, size = 10)

thenational <- plot_grid(nationaltracks_valence_dancability, density_valence_sub, ncol = 1)

ggsave("thenational.pdf", plot = thenational, width = 28, height = 50, units = "cm")


ggsave("thenational.jpeg", plot = thenational, width = 28, height = 50, units = "cm")

