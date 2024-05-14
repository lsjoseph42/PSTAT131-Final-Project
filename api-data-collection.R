# Authorization Credentials

set.seed(123)

Sys.setenv('SPOTIFY_CLIENT_ID' = Sys.getenv('CLIENT_ID'))
Sys.setenv('SPOTIFY_CLIENT_SECRET' = Sys.getenv('CLIENT_SECRET'))

authorization = get_spotify_authorization_code(scope = c('playlist-read-private', 'user-read-private', 
                                                         'user-read-recently-played', 'user-library-read'))

# Import-Saved Tracks

# Extracts songs saved in Spotify's "Liked Songs". JSON files extracted from Spotify Web API online environment.

saved_1 <- fromJSON("./data/saved_tracks_1-50.txt", simplifyDataFrame = TRUE, flatten = TRUE)
saved_2 <- fromJSON("./data/saved_tracks_51-100.json", simplifyDataFrame = TRUE, flatten = TRUE)
saved_3 <- fromJSON("./data/saved_tracks_101-105.txt", simplifyDataFrame = TRUE, flatten = TRUE)

saved_tracks <- saved_1$items %>% 
  bind_rows(saved_2$items) %>% 
  bind_rows(saved_3$items)

# Extracts songs from all my playlists lists

playlists <- get_user_playlists(user_id = my_id, authorization = authorization, limit = 25) %>% 
  filter(tracks.total > 0)

songs_in_playlists <- get_playlist_tracks(playlists$id[1])

for (id in playlists[-1, ]$id){
  songs_in_playlists <- bind_rows(get_playlist_tracks(id), songs_in_playlists)
}

# Combines songs from previous two categories into one data set 
liked_songs <- songs_in_playlists %>% 
  bind_rows(saved_tracks) %>% 
  distinct(track.id, .keep_all = TRUE) %>% 
  select(track.name, track.artists, track.id, track.album.name)

unique_artists <- liked_songs[, 'track.artists'] %>% 
  bind_rows() %>% 
  select(name, id) %>% 
  distinct(name, id)

write.csv(unique_artists, "./data/unique_artists.csv", row.names = FALSE)

liked_songs %>% 
  select(track.name, track.id, track.album.name) %>% 
  write.csv("./data/liked_songs.csv", row.names = FALSE)


# Find Unique Artists Albums
unique_artist_albums<- lapply(unique_artists$id, FUN = get_artist_albums) %>% 
  bind_rows()

# Generate Non-Saved Track Data Frame
# Take top 20 artists

top_artists <- fromJSON("./data/top_20_artists_long_term.txt", simplifyDataFrame = TRUE, flatten = TRUE)$items

# Create discography(as non-feature)

get_artist_albums_with_artist_id <- function(artist_id){
  return (get_artist_albums(id = artist_id, include_groups = c('album', 'single')) %>% 
            mutate(artist_id = artist_id))
}

artist_albums <- lapply(top_artists$id, FUN = get_artist_albums_with_artist_id) %>% 
  bind_rows()

get_artist_songs_from_albums <- function(album_id, artist_id){
  return (get_album_tracks(id = album_id) %>% 
            mutate(artist_id = artist_id))
}

top_songs_with_id <- lapply(1:nrow(artist_albums), function(i) get_artist_songs_from_albums(artist_albums[i, 'id'], artist_albums[i, 'artist_id'])) %>% 
  bind_rows()

joined <- top_songs_with_id %>% 
  select(name, artist_id, id) %>% 
  full_join(top_artists, by= c('artist_id'='id')) %>% 
  rename(track_name = name.x, artist_name = name.y, track_id = id) %>% 
  select(track_name, artist_name, track_id, artist_id)

write.csv(joined, "./data/top_artists_discography.csv", row.names = FALSE)

# Select 1000/20 = 50 random songs per artist

sampled_songs <- joined %>% 
  select(track_name, artist_name, track_id) %>% 
  slice_sample(n = 50, by = artist_name) %>% 
  mutate(saved = FALSE)

# Remove sampled songs that I have saved

liked_formatted <- liked_songs %>% 
  rename(track_name = track.name, track_id = track.id) %>% 
  select(track_name, track_id)

sampled_songs <- anti_join(sampled_songs, liked_formatted, by='track_name') # Better to do by name since song can be listed multiple times under same ID 

unique(sampled_songs$artist_name)

rock_artists <- c("The Growlers", "The Strokes", "No Buses", "Albert Hammond Jr", "The Last Shadow Puppets", "SE SO NEON", "The Voidz", "Arctic Monkeys", "The Audiots", "Julian Casablancas", "Gorillaz")

hip_hop_artists <- c("Knxwledge", "2Pac", "Kanye West", "JAY-Z", "Ice Cube", "Wu-Tang Clan", "MF DOOM", "Slick Rick", "Nas")

sampled_songs <- sampled_songs %>% 
  mutate(simplified_genre = case_when(artist_name %in% rock_artists ~ 'rock',
                                      artist_name %in% hip_hop_artists ~ 'hip hop'))

write.csv(sampled_songs, "./data/sampled_songs.csv", row.names = FALSE)
```

```{r Mutate Simplified Genre NOT USED, echo = F, eval=F}
# Take all artists
# Take all genres

get_artist_genres <- function(artist_id){
  return (as.character(get_artist(artist_id)$genres))
}

unique_artists_genre <- lapply(unique_artists$id, FUN = get_artist_genres)

unique_genres <- unique(unlist(unique_artists_genre)) %>% 
  as.data.frame() %>% 
  rename(genre = 1) %>% 
  mutate(simplified = case_when(grepl('rock', genre, fixed=TRUE) ~ 'rock',
                                grepl('hip hop', genre, fixed=TRUE) ~ 'hip hop',
                                grepl('rap', genre, fixed=TRUE) ~ 'hip hop')
  )

# Generate case-when table to classify genres as rock, hip-hop, add in other for outliers on final check
# Mutate simplified genre onto artists
# Mutate simplified genre from artists onto tracks
```

# Combine saved songs and control, attach audio features 
liked_songs <- read.csv("./data/liked_songs_with_genre.csv") %>% 
  select(track.id, simplified_genre) %>% 
  inner_join(read.csv("./data/liked_songs_with_stats.csv"), by= c('track.id' = 'id')) %>% 
  rename(track_id = track.id, track_name = track.name) %>% 
  mutate(saved = TRUE) %>% 
  select(-track.album.name)

sampled_songs <- read.csv("./data/sampled_songs_stats.csv") %>% 
  select(-artist_name)

dataset <- bind_rows(liked_songs, sampled_songs) %>% 
  select(-track_href, -uri, -analysis_url, -type)

sampled_songs_stats_1 <-lapply(sampled_songs$track_id[1:410], FUN = get_track_audio_features) %>%
   bind_rows()

 sampled_songs_stats_2 <-lapply(sampled_songs$track_id[411:819], FUN = get_track_audio_features) %>%
   bind_rows()

 sampled_songs_stats <- sampled_songs_stats_1 %>%
   bind_rows(sampled_songs_stats_2) %>%
   distinct()

 sampled_songs <- inner_join(sampled_songs,sampled_songs_stats, by=c('track_id'='id'))

 write.csv(sampled_songs, "./data/sampled_songs_stats.csv", row.names = FALSE)

dataset <- liked_songs %>%
   select(track.name, track.id, simplified_genre) %>%
   mutate(saved = TRUE) %>%
   rename(track_name = track.name, track_id = track.id) %>%
   distinct() %>%
   bind_rows(select(.data = sampled_songs, track_name, track_id, simplified_genre, saved))

dataset_stats <- lapply(dataset$track_id, FUN = get_track_audio_features) %>%
  bind_rows()

combined <- dataset_stats %>%
  select(-track_href, -uri, -analysis_url, -type) %>%
  left_join(dataset, by= c('id' = 'track_id'))

combined <- dataset[-which(duplicated(dataset$track_id)), ] # About four songs were duplicated after combining

write.csv(combined, "./data/complete_dataset.csv", row.names = FALSE)

