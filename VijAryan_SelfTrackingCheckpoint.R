library(readxl)
library(tibble)

mydata <- read_excel(
  "C:/Users/aryan/Downloads/midpoint_data.xlsx", sheet = "Midpoint Data", skip = 2)

mydata$date <- as.POSIXct(mydata$date, origin = "1970-01-01", tz = "UTC")

mydata <- as_tibble(mydata)
mydata

tibble(
  variable = names(mydata),
  data_type = sapply(mydata, class)
)

data.frame(
  Feature = names(mydata)[sapply(mydata, is.numeric)],
  Mean = sapply(mydata[sapply(mydata, is.numeric)], mean, na.rm = TRUE),
  SD = sapply(mydata[sapply(mydata, is.numeric)], sd, na.rm = TRUE)
)
