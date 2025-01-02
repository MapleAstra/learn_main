library(viridis)


collab_list <- c("별빛이 피면 (Star Blossom) - SM STATION",
                 "전화하기 전에 생각했나요", 
                 "RE:main",
                 "We, the Reds",
                 "TAEIL X SEJEONG 'Love or Not'",
                 "FLOWER 9")

# Prepare the data according to this R graph gallery circular bar plot chart. https://r-graph-gallery.com/circular-barplot.html
data <- ksj_all_tracks_combined|> 
  filter(!is.na(duration_ms)) |> 
  filter(!is.na(track_name)) |> 
  filter(!is.na(album_name)) |> 
  mutate(duration_ms = duration_ms/1000) |> 
  rename('individual' = 'track_name') |> 
  # rename('group' = 'album_name') |> 
  rename('value' = 'duration_ms') |> 
  mutate(group = case_when(album_name %in% c("Door", "Plant", "I'm") ~ album_name,
                           album_name %in% collab_list ~ "Collab",
                                    str_detect(album_name, "OST|Original") ~ "OST",
                                    TRUE ~ "Single")) |> 
  mutate(group = factor(group, levels = c("Plant",  "I'm", "Door",
                                          "Single", "OST", "Collab"))) |> 
  arrange(group, release_date,  track_number)  |> 
  select(individual, album_name, group, value, duration_minutes)


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))



# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 600, xend = start, yend = 600), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start, yend = 500), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 400, xend = start, yend = 400), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 300, xend = start, yend = 300), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  #annotate("text", x = rep(max(data$id),6), y = c(100, 200, 300, 400,500,600), label = c("100", "200", "300", "400","500","600") , color="black", size=5 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-500,700) +
  scale_fill_albums() +
  scale_fill_manual(values = c("#B1B1B1","#D0AFD7", "black","#FFBA08", "#7E91DB","#FF7B9C","#FF9B85")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"),
    plot.background = element_rect(color = "white",fill = "white")
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), 
            color="black",alpha=0.8, size=5, fontface = "bold", angle= label_data$angle, 
            inherit.aes = FALSE ) +
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,1,0,0,0), colour = "#4A14A7", alpha=0.8, size=5, fontface="bold", inherit.aes = FALSE) +
  geom_text(data=label_data, aes(x=id, y=10, label=duration_minutes, hjust=hjust), color="white",
            fontface = "bold", alpha=1, size=4, angle= label_data$angle, inherit.aes = FALSE ) 

p

# Save at png
ggsave(p, file="ksj_spotify_2024.png", width=11, height=15)
