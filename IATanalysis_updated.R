# Accounts for new NEW TEMPLATES Gender-Happy; Race-Creativity (2 word templates)


# Set the path to the IAT folder here (be sure to include trailing slash)
base.dir = "/Users/Ulichka/Desktop/Research/RtF/IAT/Template/"

# File delimiter = '/' for Unix/Mac, '//' for Windows
fd = '/'

# Set the template you want to analyze here
template.name = "Gender-Happy" # change to your template
output.dir = paste(base.dir, "templates", fd, template.name, fd, "output", fd, sep="")

setwd(output.dir)
output.files = list.files()
data.list = lapply(output.files, read.table, sep = ",")

summarized <- data.frame(matrix(ncol=9, nrow=length(data.list)))
names(summarized) <- c("Id","Date","Block4.m","Block4.sd","Block7.m","Block7.sd","diff","full.sd",'d')

for(i in 1:length(data.list)) {
  filename = strsplit(output.files[i], '-')[[1]]
  
  # Debugging: Print filename structure
  print(paste("Processing:", output.files[i]))
  
  # Ensure filename has at least 7 parts
  if (length(filename) < 7) {
    print("Error: Unexpected filename format. Skipping file.")
    next
  }
  
  # Extract ID (3rd element)
  id = filename[3]
  
  # Format date as MM-DD-YYYY HH:MM
  iat.date = paste(filename[5], "-", filename[4], "-", filename[6], " ", filename[7], ":", substr(filename[8], 1, 2), sep="")
  
  # Debugging: Print extracted values
  print(paste("Extracted ID:", id))
  print(paste("Formatted Date:", iat.date))
  
  # Compute means and SDs
  block4 = as.numeric(unlist(subset(data.list[[i]], V1 == 3 & V6 > 300 & V6 < 3000, select = "V6")))
  block7 = as.numeric(unlist(subset(data.list[[i]], V1 == 6 & V6 > 300 & V6 < 3000, select = "V6")))
  
  block4.m = ifelse(length(block4) > 0, mean(block4, na.rm = TRUE), NA)
  block7.m = ifelse(length(block7) > 0, mean(block7, na.rm = TRUE), NA)
  block4.sd = ifelse(length(block4) > 1, sd(block4, na.rm = TRUE), NA)
  block7.sd = ifelse(length(block7) > 1, sd(block7, na.rm = TRUE), NA)
  
  full_data = c(block4, block7)
  full.sd = ifelse(length(full_data) > 1, sd(full_data, na.rm = TRUE), NA)
  diff = block7.m - block4.m
  d = diff / full.sd
  
  # Ensure all 9 values are present
  summarized[i, ] <- c(id, iat.date, block4.m, block4.sd, block7.m, block7.sd, diff, full.sd, d)
}

# Remove unnecessary variables
rm(block4, block4.m, block4.sd, block7, block7.m, block7.sd, d, data.list, diff, filename, full.sd, i, iat.date, id, output.files)

setwd(paste(base.dir, "templates", fd, template.name, sep=""))
write.csv(summarized, "summarized.csv") # change to your file name
