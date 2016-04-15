# Required packages.
library("ggplot2")
library("stats")

# Import the raw data.
combined.data <- read.csv("Flower_Gray_Combined_Data.csv", header=TRUE)
colnames(combined.data) <- c("type", "id", "tp", paste0(seq(1:25)))

# Extract all unique subject IDs.
subject.matrix <- levels(combined.data[,2])

# Isolate the flower data
flower.rows <- combined.data[,1] == "flower"
flower.data <- combined.data[flower.rows, ]

# Isolate the gray data
gray.rows <- combined.data[,1] == "gray"
gray.data <- combined.data[gray.rows, ]

# Apply FFT to each electrode of each tester (subject).
ApplyFFT <- function(df) {
  # Temp variable k corresponds to k in BrainEmotion pdf.
  for (k in 4:length(df)) {
      
  }
}
