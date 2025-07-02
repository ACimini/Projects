# TransformData.R

# Step 1: Read the input
input <- read.csv("C:/Temp/temp_input.csv")

# Step 2: Do calculations (e.g., multiply each value by 2)
output <- data.frame(Result = input$Value * 2)

# Output a single column CSV with header "Result"
write.csv(data.frame(Result = input$Value * 2), "C:/Temp/temp_output.csv", row.names = FALSE)
