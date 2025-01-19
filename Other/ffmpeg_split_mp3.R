# Load libraries ----------------------------------------------------------------
library(tidyverse)
library(stringr)

# Directory ---------------------------------------------------------------------
input_dir <- 
output_dir <- 
ffmpeg_path <-"ffmpeg.exe"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Function to Split Mp3 file ----------------------------------------------------
segment_duration <- 19  # Duration in seconds
split_mp3 <- function(file_path, output_dir, segment_duration) {
  file_name <- tools::file_path_sans_ext(basename(file_path))
  # ffmpeg_path <- 
  # Obtain Song File duration using ffmpeg
  cmd_duration <- sprintf('"%s" -i "%s"', ffmpeg_path, file_path)
  duration_info <- system(cmd_duration, intern = TRUE, ignore.stderr = FALSE)
  
  # Parse Duration (Look for "Duration: HH:MM:SS.mmm")
  duration_match <- str_match(paste(duration_info, collapse = " "), "Duration:\\s*(\\d+):(\\d+):(\\d+\\.\\d+)")
  
  if (is.na(duration_match[1])) {
    stop(sprintf("Unable to extract duration for file: %s", file_path))
  }
  
  duration_secs <- as.numeric(duration_match[, 2]) * 3600 +  # Hours to seconds
    as.numeric(duration_match[, 3]) * 60 +    # Minutes to seconds
    as.numeric(duration_match[, 4])          # Seconds
  
  # Calculate number of segments
  n_segments <- ceiling(duration_secs / segment_duration)
  
  # Iterate over each segment
  for (i in seq_len(n_segments)) {
    start_time <- (i - 1) * segment_duration
    end_time <- min(i * segment_duration, duration_secs)
    
    # Ensure times are numeric
    start_time <- as.numeric(start_time)
    end_time <- as.numeric(end_time)
    
    # Generate Output File Name
    index <- sprintf("%02d", i)
    start_str <- sprintf("%02d:%02d:%02d", start_time %/% 3600, (start_time %% 3600) %/% 60, start_time %% 60)
    end_str <- sprintf("%02d:%02d:%06.3f", end_time %/% 3600, (end_time %% 3600) %/% 60, end_time %% 60)
    output_file <- file.path(output_dir, sprintf("%s_%s.mp3", file_name, index))
    
    # Split using ffmpeg with overwrite enabled
    cmd_trim <- sprintf(
      '"%s" -y -i "%s" -ss %s -to %s -c copy "%s"',
      ffmpeg_path, file_path, start_str, end_str, output_file
    )
    system(cmd_trim, intern = FALSE, ignore.stderr = FALSE)
  }
}


# Process files -----------------------------------------------------------------
mp3_files <- list.files(input_dir, pattern = "\\.mp3$", full.names = TRUE)

walk(mp3_files, split_mp3, output_dir = output_dir, segment_duration = segment_duration)

