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

# Apply the Fast Fourier Transform to each electrode for each tester (subject).
ApplyFFT <- function(df, subject.ids) {
  # Build the initial data frame...
  # fft.frame stores all FFT data in a 256 x (34*25) data frame.
  fft.frame <- data.frame(id = matrix(nrow = 0, ncol = 1),
                          tp = matrix(nrow = 0, ncol = 1),
                          matrix(nrow = 0, ncol = 25))
  
  # Loop through all the data in combined table.
  for (i in 1:length(subject.ids)) {
    # Declare an empty matrix to store FFT results.
    fft.results <- matrix(nrow = 256, ncol = 0)

    # Now, loop through the electrodes contained in columns [4,28].
    for (j in 4:length(df)) {
      # Create a matrix of FFT values for 1 electrode, 1 subject.
      # fft.table <- matrix(fft(df[df[,2]==subject.ids[i],j]), nrow = 256, ncol = 1)
      # Get the results of FFT.
      fft.results <- cbind(fft.results, fft(df[df[,2] == subject.ids[i],j]))
    }

    # Populate the table.
    fft.table <- data.frame(id = rep(subject.ids[i],256),
                            tp = 1:256,
                            fft.results)
    # Correct the electrode column names.
    colnames(fft.table) <- c("id", "tp", seq(1,25))
    
    # Store the frequencies in the fft.frame.
    fft.frame <- rbind(fft.frame, fft.table)
  }

  fft.frame # Return the fft.frame.
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
delta <- colMeans(delta.frame)
theta <- colMeans(theta.frame)
alpha <- colMeans(alpha.frame)
beta <- colMeans(beta.frame)
