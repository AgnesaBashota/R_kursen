
req <- c(
  'readxl','writexl','tidyverse','janitor','lubridate','stringr',
  'broom','car','patchwork','GGally','ggplot2','modelr'
)
to_install <- req[!req %in% installed.packages()[,1]]
if(length(to_install)) install.packages(to_install, repos = 'https://cloud.r-project.org')
message('Packages klara.')
