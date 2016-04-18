# Required packages.
library("ggplot2")
library("stats")
library("reshape2")
library("plyr")
library("plotly")

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
magnitude.frame <- data.frame(fft.data[,1], 
                              abs(fft.data[,2:length(fft.data)]))
# Rename electrode columns back to 1..25 instead of X1..X25.
colnames(magnitude.frame) <- c("id", "tp", seq(1,25))

# Build one data frame for each significant frequency spectrum and append each
# subject's mean values for their respective electrode readings.
delta.frame <- data.frame(matrix(nrow = 0, ncol = 27))
theta.frame <- data.frame(matrix(nrow = 0, ncol = 27))
alpha.frame <- data.frame(matrix(nrow = 0, ncol = 27))
beta.frame <- data.frame(matrix(nrow = 0, ncol = 27))
# Rename initial frame columns.
colnames(delta.frame) = c("id", "tp", seq(1,25))
colnames(theta.frame) = c("id", "tp", seq(1,25))
colnames(alpha.frame) = c("id", "tp", seq(1,25))
colnames(beta.frame) = c("id", "tp", seq(1,25))

# Iterate through each subject.
j <- 1
for (i in seq(1,length(subject.ids))) {
  # Bind the rows to the ends of each frame.
  delta.frame <- rbind(delta.frame, magnitude.frame[(j):(j+6), ])
  theta.frame <- rbind(theta.frame, magnitude.frame[(j+7):(j+8), ])
  alpha.frame <- rbind(alpha.frame, magnitude.frame[(j+9):(j+12), ])
  beta.frame <- rbind(beta.frame, magnitude.frame[(j+13):(j+30), ])
  
  # Create a matrix for each list of electrode means.
  delta.means <- cbind(id = subject.ids[i], 
                       electrode = seq(1,25), 
                       mean = colMeans(delta.frame[3:27]))
  theta.means <- cbind(id = subject.ids[i], 
                       electrode = seq(1,25), 
                       mean = colMeans(theta.frame[3:27]))
  alpha.means <- cbind(id = subject.ids[i], 
                       electrode = seq(1,25), 
                       mean = colMeans(alpha.frame[3:27]))
  beta.means <- cbind(id = subject.ids[i], 
                       electrode = seq(1,25), 
                       mean = colMeans(beta.frame[3:27]))
  
  # Convert each means to a data frame.
  delta.means <- data.frame(delta.means)
  theta.means <- data.frame(theta.means)
  alpha.means <- data.frame(alpha.means)
  beta.means <- data.frame(beta.means)

  # Output a plot for each subject.
  

  # Increment j.
  if (i < 34) {
    j <- (j+256)
  }
}

delta.plot <- ggplot(data=delta.means, aes(x=electrode, y=factor(mean), fill=factor(mean))) + geom_bar(stat="identity") + guides(fill=FALSE) + ggtitle(subject.ids[1]) + scale_x_discrete(limits=seq(1,25))
