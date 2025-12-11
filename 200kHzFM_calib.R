#this code plots the calculated calibration values for WBAT 200 kHz FM transducers, calibrated with a 25 and 38.1 mm tungsten spheres
#the result are merged calibration values that can be entered in an ecs file directly for processing on echoview
#author: Arienne Calonge
#date: December 2025

library(tidyverse)

setwd("C:/Users/arienne.calonge/OneDrive - VLIZ/Ari/Echosounder - general/R")
WBAT_SN <- '284143'
Code <- ' Grafton 2025 200 kHz'

# Set your folder path
path <- "C:/Users/arienne.calonge/OneDrive - VLIZ/Ari/Echosounder - general/R/Calibration data"  # change this!

read_calib_output <- function(path) {
  lines <- readLines(path, warn = FALSE)
  
  ## find "Calibration output values"
  start <- grep("^Calibration output values", lines)
  if (!length(start)) stop("No 'Calibration output values' block found")
  start <- start[1]
  
  ## move down until the first real data line (this should be 'Frequency (kHz):')
  i <- start + 1
  while (i <= length(lines) &&
         (grepl("^-+$", trimws(lines[i])) ||  # dashed line
          !nzchar(trimws(lines[i])))) {      # blank line
    i <- i + 1
  }
  
  ## end at the line just before "Calibration input values"
  end <- grep("^Calibration input values", lines)
  if (!length(end)) end <- length(lines) + 1
  end <- end[1] - 1
  
  block <- lines[i:end]
  
  ## parse labelled numeric blocks
  res <- list()
  current_label <- NULL
  
  for (ln in block) {
    if (!nzchar(trimws(ln))) next
    if (grepl("^-+$", trimws(ln))) next
    
    if (grepl(":", ln)) {
      current_label <- trimws(sub(":.*", "", ln))   # text before ":"
      num_part      <- sub(".*:", "", ln)           # text after ":"
    } else {
      num_part      <- ln                           # continuation line
    }
    
    nums <- as.numeric(strsplit(num_part, "\\s+")[[1]])
    nums <- nums[!is.na(nums)]
    if (!length(nums)) next
    
    if (is.null(res[[current_label]])) {
      res[[current_label]] <- nums
    } else {
      res[[current_label]] <- c(res[[current_label]], nums)
    }
  }
  
  res
}

txt_files <- list.files(path, pattern = "\\.txt$", full.names = TRUE)

all_outputs <- lapply(txt_files, read_calib_output)
names(all_outputs) <- basename(txt_files)

#convert to data frame
df_25  <- as.data.frame(all_outputs[[1]], check.names = FALSE)
df_38  <- as.data.frame(all_outputs[[2]], check.names = FALSE)

#fill in nulls of the 38 mm tungsten sphere with values from the 25 mm sphere
nulls <- df_25 %>% filter(`Frequency (kHz)` %in% c(207:210,225:227,243:246))
df <- df_38 %>% filter(!`Frequency (kHz)` %in% c(207:210,225:227,242:251)) %>% rbind(nulls)
df <- df[order(df$`Frequency (kHz)`), ]

#plot frequency x transducer gain
ggplot() +
  geom_line(data=df_25, aes(x = `Frequency (kHz)`, y = `Transducer gain (dB)`, colour = "25 mm")) +
  geom_line(data=df_38, aes(x = `Frequency (kHz)`, y = `Transducer gain (dB)`, colour = "38 mm")) +
  geom_line(data=df, aes(x = `Frequency (kHz)`, y = `Transducer gain (dB)`, colour = "merged")) +
  scale_colour_manual(values = c("25 mm" = "black", "38 mm" = "blue", "merged"="red"))+
  ggtitle("Calibration WBAT 284143 200kHz FM - Grafton 2025") +
  xlab("Frequency (kHz)") + ylab("Transducer gain (dB)")

ggsave(paste0("Plots/calibration_", WBAT_SN, Code), device= jpeg, dpi = 250, width = 12, height = 3)

#convert df into txt file 

#create functions
make_block_lines <- function(label, values, per_line = 70, digits = 4) {
  # format numbers as text
  vals_chr <- format(round(values, digits),
                     nsmall = digits,
                     trim = TRUE,
                     scientific = FALSE)
  
  n <- length(vals_chr)
  idx <- split(seq_len(n), ceiling(seq_len(n) / per_line))
  
  lines <- character(length(idx))
  indent <- paste0(strrep(" ", nchar(label) + 2))  # spaces to align wrapped lines
  
  i <- 1
  for (g in idx) {
    if (i == 1) {
      lines[i] <- paste0(label, " = ",
                         paste(vals_chr[g], collapse = "  "))
    } else {
      lines[i] <- paste0(indent,
                         paste(vals_chr[g], collapse = "  "))
    }
    i <- i + 1
  }
  
  lines
}
write_calib_txt <- function(df, file) {
  # build the block
  lines <- c(
    "Calibration output values",
    "-------------------------",
    "",
    make_block_lines("FrequencyTableWideband", df$`Frequency (kHz)`),
    make_block_lines("GainTableWideband", df$`Transducer gain (dB)`),
    make_block_lines("MinorAxisAngleOffsetTableWideband", df$`Minor-axis angle offset (º)`),
    make_block_lines("MajorAxisAngleOffsetTableWideband", df$`Major-axis angle offset (º)`),
    make_block_lines("MinorAxisBeamWidthTableWideband", df$`Minor-axis 3dB beam angle (º)`),
    make_block_lines("MajorAxisBeamWidthTableWideband", df$`Major-axis 3dB beam angle (º)`)
  )
  
  writeLines(lines, file)
}

write_calib_txt(df, paste0("calibration",WBAT_SN, Code, "_merged.txt"))
