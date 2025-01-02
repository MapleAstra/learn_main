library(spotifyr)
library(tidyverse)
library(plotly)
library(ggimage)
library(httr)
library(httpuv)
library(kableExtra)
library(viridis)

convert_duration_seconds_to_minutes_seconds <- function(duration_ms) {
  # Convert milliseconds to seconds
  duration_seconds <- duration_ms / 1000
  # Calculate minutes and seconds
  minutes <- floor(duration_seconds / 60)
  seconds <- round(duration_seconds %% 60)
  # Format as "m's"
  sprintf("%d'%02d", minutes, seconds)
}

# Sys.setenv(SPOTIFY_CLIENT_ID = "5b7104f6a68c4e9fbde09d7688282426")
# Sys.setenv(SPOTIFY_CLIENT_SECRET = "3adbc409f3674d63bb2216d08bba5eac")
# Sys.setenv(SPOTIFY_REDIRECT_URI = "http://localhost:1410/")

access_token <- get_spotify_access_token()

ksj_albums <- get_artist_albums(id = "1lFLniFTaPjYCtQZvDXpqu")
ksj_details <- get_artist(id = "1lFLniFTaPjYCtQZvDXpqu")

ksj_album_types <- unique(ksj_albums$album_type)
ksj_album_ids <- unique(ksj_albums$id)


# #Deprecated Web API features 
# ksj_tracks <- get_artist_audio_features(artist = 'kimsejeong', 
#                                         include_groups = ksj_album_types)

#Empty list to store
ksj_album_track_details <- list()

#For loops for each album IDs
for (i in seq_along(ksj_album_ids)) {
  ksj_album_details <- get_album(id = ksj_album_ids[i])
   
  ksj_all_tracks <- get_album_tracks(id = ksj_album_ids[i])

  ksj_all_tracks$album_name <- ksj_album_details$name
  ksj_all_tracks$release_date <- ksj_album_details$release_date
  ksj_all_tracks$image_url <- ifelse(length(ksj_album_details$images) > 0,
                                     ksj_album_details$images$url[[1]], NA)
  ksj_all_tracks$spotify_link <- ksj_album_details$external_urls$spotify

  # ksj_all_tracks <- ksj_all_tracks[, c("name", "id", "duration_ms", "album_name",
  #                      "release_date", "image_url", "external_url"),
  #                  drop = FALSE]

  ksj_album_track_details[[i]] <- ksj_all_tracks
}

filter_artist_OST <- data.frame(
  album_name = c("Crash Landing on You (Original Television Soundtrack)",
            "Mr. Sunshine (Original Television Soundtrack)",
            "The Uncanny Counter (Original Television Soundtrack)",
            "The Uncanny Counter 2, Pt. 2 (Original Television Soundtrack)",
            "A Business Proposal OST",
            "BREWING LOVE, Pt. 10 (Original Soundtrack)", 
            "FLOWER 9"),
  track_name = c("All of My Days",
                 "Paramour", 
                 "Meet Again", 
                 "Once again",
                 "Love, Maybe (Acoustic Ver.)",
                 "Two of us",
                 "Can I go back (Feat. KIMSEJEONG)")
  )

remove_dup_albums <- 
  c("Crash Landing on You (Original Television Soundtrack), Pt. 8",
    "Mr. Sunshine, Pt. 13 (Original Television Soundtrack)",
    "The Uncanny Counter (Original Television Soundtrack), Pt. 2", 
    "Love, Maybe (A Business Proposal OST Bonus Track)")


sejeong_extra <- get_tracks(ids = c("3mxYRXIblaJSWjhGFFNQZB", "3qKI6XiNiAnnPkEuj8YlZs",
                                    "6lwl4UZmfozIxafPPkbfqc", "11msA8zwlCWThs4QFTZaLO",
                                    "1EZYtpVrM116vaULIcruIf", "4URU9AeuWQlE16pJBKmvEp")) 

sejeong_extra_list <- sejeong_extra |> 
  select(album.name, track_number, name, duration_ms, album.release_date,
         album.images, album.external_urls.spotify, external_urls.spotify) |> 
  mutate(album.images = map_chr(album.images, ~ {
    if (is.data.frame(.x) && nrow(.x) > 0 && ncol(.x) > 0) {
      .x[1, 1]  # Extract first row, first column
    } else {
      NA_character_  # Return NA for empty or invalid data
    }
  })) |> 
  rename(album_name = album.name,
         track_name = name,
         release_date = album.release_date,
         image_url = album.images, 
         album_link = album.external_urls.spotify,
         track_link = external_urls.spotify)

# sejeong_extra_list <- data.frame(
#   album_name = sejeong_extra$album$name,
#   track_number = sejeong_extra$track_number,
#   track_name = sejeong_extra$name, 
#   duration_ms = sejeong_extra$duration_ms,
#   release_date = sejeong_extra$album$release_date,
#   image_url = sejeong_extra$album$images$url[[1]],
#   album_link = sejeong_extra$album$external_urls$spotify,
#   track_link = sejeong_extra$external_urls$spotify)


#Combine track details in single data frame
ksj_all_tracks_combined <- do.call(rbind, ksj_album_track_details) |> 
  select(album_name, track_number, name, duration_ms, release_date, image_url, 
         spotify_link, external_urls.spotify) |> 
  rename(track_name = name,
         album_link = spotify_link,
         track_link = external_urls.spotify) |> 
  filter(!album_name %in% remove_dup_albums) |> 
  filter(!str_detect(track_name, "Inst")) |> 
  filter(album_name %in% filter_artist_OST$album_name & 
           track_name %in% filter_artist_OST$track_name |
           !album_name %in% filter_artist_OST$album_name) |> 
  bind_rows(sejeong_extra_list) |> 
  mutate(duration_minutes = convert_duration_seconds_to_minutes_seconds(duration_ms)) |> 
  relocate(duration_minutes, .after = duration_ms)

# 
# #Album = Door - 5MO48BnK6X9VlYtFHpJtvy
# ksj_tracks_door <- get_album_tracks(id = ksj_album_ids[1])
# 
# #Album - Plant 
# ksj_tracks_plant <- get_album_tracks(id = ksj_album_ids[10])
# 
# #Album - I'm 
# ksj_tracks_im <- get_album_tracks(id = ksj_album_ids[7])
# 
# #Single - Flower  Way 
# ksj_tracks_flowerway <- get_album_tracks(id = ksj_album_ids[15])
# 
# #Single - Whale 
# ksj_tracks_whale <- get_album_tracks(id = ksj_album_ids[9])
# 
# #Single - Tunnel
# ksj_tracks_tunnel <- get_album_tracks(id = ksj_album_ids[12])
# 
# #OST - Brewing Love - Two of Us 
# ksj_tracks_OST_BL <- get_album_tracks(id = ksj_album_ids[2])
# 
# #OST - Uncanny Counter = Meet Again
# ksj_tracks_OST_UC1 <- get_album_tracks(id = ksj_album_ids[8])
# 
# #OST - Uncanny Counter 2 = Once Again
# ksj_tracks_OST_UC2 <- get_album_tracks(id = ksj_album_ids[4])
# 
# #OST - Business Proposal  - Love, Maybe
# ksj_tracks_BP <- get_album_tracks(id = ksj_album_ids[5])
# 
# #OST - All of My Days
# ksj_tracks_AOMD <- get_album_tracks(id = ksj_album_ids[11])
# 
# #OST - Mr Sunshine - Paramour 
# ksj_tracks_paramour <- get_album_tracks(id = ksj_album_ids[13])
# 
# #Baby I love you
# ksj_tracks_BabyILOVEU <- get_album_tracks(id = ksj_album_ids[6])
# 
# #My Seasons
# ksj_tracks_myseasons <- get_album_tracks(id = ksj_album_ids[3])
# 
# #Duet - Star Blossom 
# ksj_tracks_star <- get_album_tracks(id = ksj_album_ids[14])
# 
# #Feature - Can I go back - 5BV2LfKKzCuQBpJKOPGO80
# ksj_tracks_CanIGoBack <- get_album_tracks(id = ksj_album_ids[19]) |> 
#   filter(str_detect(name, "KIMSEJEONG"))
# 
# #16 - 18, 20 index are full drama albums
# # ksj_tracks_p <- get_album_tracks(id = ksj_album_ids[20])
# 
# ksj_tracks_combibne <- ksj_tracks_plant |> 
#   bind_rows(ksj_tracks_im) |> 
#   bind_rows(ksj_tracks_door) |> 
#   bind_rows(ksj_tracks_flowerway) |>
#   bind_rows(ksj_tracks_whale) |>
#   bind_rows(ksj_tracks_tunnel) |>
#   bind_rows(ksj_tracks_AOMD) |> 
#   bind_rows(ksj_tracks_paramour) |>
#   bind_rows(ksj_tracks_OST_UC1) |>
#   bind_rows(ksj_tracks_OST_UC2) |> 
#   bind_rows(ksj_tracks_BP) |>
#   bind_rows(ksj_tracks_OST_BL) |>
#   bind_rows(ksj_tracks_BabyILOVEU) |>
#   bind_rows(ksj_tracks_myseasons) |> 
#   bind_rows(ksj_tracks_star) |>
#   bind_rows(ksj_tracks_CanIGoBack) |> 
#   select(name, track_number, duration_ms, external_urls.spotify) |> 
#   filter(!str_detect(name, "Inst"))
# 
# test <- get_album("5MO48BnK6X9VlYtFHpJtvy")
# 
# test_t <- test$tracks
