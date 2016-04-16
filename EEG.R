# Required packages.
library("ggplot2")
library("stats")

# Import the raw data.
combined.data <- read.csv("Flower_Gray_Combined_Data.csv", header=TRUE)
colnames(combined.data) <- c("type", "id", "tp", paste0(seq(1:25)))

# Extract all unique subject IDs.
subject.ids <- levels(combined.data[,2])

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

# Apply FFT to each electrode for each tester (subject).
ApplyFFT <- function(df, subject.ids) {
  initial.frame <- data.frame(x=1:256)
    
  # frequencyFrame stores all fft data in a 256 x (34*25) data frame.
  fft.frame <- initial.frame[,FALSE] # Create the frequency frame
  
  for(i in 1:length(subject.ids)) {
    for(j in 4:28) {
      # Create a matrix of fft values for 1 electrode, 1 subject.
      fft.array <- matrix(fft(df[df[,2]==subject.ids[i],j]), nrow = 256, ncol = 1)
      # Name the matrix columns.
      colnames(fft.array) <- paste0(subject.ids[i],"-El",(j-3))
      # Store the frequencies in the frequencyFrame.
      fft.frame <- cbind(fft.frame, fft.array)
    }
  }

  row.names(fft.frame) = paste0(1:256)
  fft.frame # Return the binFrame
}


# Calculate the magnitudes, then create frames for the delta, theta, alpha, and 
# beta frequencies.
fft.data <- ApplyFFT(combined.data, subject.ids)
magnitude.frame <- abs(fft.data[,2:length(fft.data)])
delta.frame <- magnitude.frame[1:7,]
theta.frame <- magnitude.frame[8:9,]
alpha.frame <- magnitude.frame[10:13,]
beta.frame <- magnitude.frame[14:31,]

# Obtain the mean of magnitudes for each electrode from magnitudes stored in 
# data.frames (delta.frame, etc.).
delta <- col.means(delta.frame)
theta <- col.means(theta.frame)
alpha <- col.means(alpha.frame)
beta <- col.means(beta.frame)
