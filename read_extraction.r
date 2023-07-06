setwd("C:/Users/fe00112/OneDrive - University of Surrey/PhD/genome_ascaris/assembly")

# First, set up the file paths and output file
sequence_file <- "s7_r1_txt_file.txt"
output_file <- "s7__R1_100_sequences.txt"
# Open the sequence file and read the lines into a character vector
sequence_lines <- readLines(sequence_file)
# Open the output file for writing
write_file <- file(output_file, "w")
# Loop over the desired number of lines (in this case, 200)
for (i in 1:30) {
# Write the ">" character and the line number to the output file
write(paste0(">", i), write_file)
# Calculate the index of the sequence line we want to retrieve
sequence_index <- 2 + 4*(i-1)
# Write the corresponding sequence line to the output file
write(sequence_lines[sequence_index], write_file)
}
# Close the output file
close(write_file)


# First, set up the file paths and output file
sequence_file <- "s8_r1_txt_file.txt"
output_file <- "s8__R1_100_sequences.txt"
# Open the sequence file and read the lines into a character vector
sequence_lines <- readLines(sequence_file)
# Open the output file for writing
write_file <- file(output_file, "w")
# Loop over the desired number of lines (in this case, 200)
for (i in 1:30) {
# Write the ">" character and the line number to the output file
write(paste0(">", i), write_file)
# Calculate the index of the sequence line we want to retrieve
sequence_index <- 2 + 4*(i-1)
# Write the corresponding sequence line to the output file
write(sequence_lines[sequence_index], write_file)
}
# Close the output file
close(write_file)

setwd("C:/Users/fe00112/OneDrive - University of Surrey/PhD/genome_ascaris/assembly")
# First, set up the file paths and output file
sequence_file <- "s9_r1_txt_file.txt"
output_file <- "s9__R1_100_sequences.txt"
# Open the sequence file and read the lines into a character vector
sequence_lines <- readLines(sequence_file)
# Open the output file for writing
write_file <- file(output_file, "w")
# Loop over the desired number of lines (in this case, 200)
for (i in 1:30) {
# Write the ">" character and the line number to the output file
write(paste0(">", i), write_file)
# Calculate the index of the sequence line we want to retrieve
sequence_index <- 2 + 4*(i-1)
# Write the corresponding sequence line to the output file
write(sequence_lines[sequence_index], write_file)
}
# Close the output file
close(write_file)


setwd("C:/Users/fe00112/OneDrive - University of Surrey/PhD/genome_ascaris/assembly")
# First, set up the file paths and output file
sequence_file <- "s10_r1_txt_file.txt"
output_file <- "s10__R1_100_sequences.txt"
# Open the sequence file and read the lines into a character vector
sequence_lines <- readLines(sequence_file)
# Open the output file for writing
write_file <- file(output_file, "w")
# Loop over the desired number of lines (in this case, 200)
for (i in 1:30) {
# Write the ">" character and the line number to the output file
write(paste0(">", i), write_file)
# Calculate the index of the sequence line we want to retrieve
sequence_index <- 2 + 4*(i-1)
# Write the corresponding sequence line to the output file
write(sequence_lines[sequence_index], write_file)
}
# Close the output file
close(write_file)
