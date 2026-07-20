install.packages("FFTrees") 
library("FFTrees")                # Load the package
FFTrees.guide()   

heart.fft <- FFTrees(formula = diagnosis ~ ., # Criterion
                     data = heart.train,      # Training data
                     data.test = heart.test,  # Testing data
                     main = "Heart Disease",  # Optional labels
                     decision.labels = c("Low-Risk", "High-Risk"))

# Step 3: Inspect and summarize FFTs
heart.fft            # Print statistics of the final FFT
inwords(heart.fft)   # Print a verbal description of the final FFT
summary(heart.fft)   # Print statistics of all FFTs

# Step 4: Visualize the final FFT and performance results
# a) plot final FFT applied to test data:
plot(heart.fft, data = "test")

# b) plot individual cue accuracies in ROC space:
plot(heart.fft, what = "cues")
