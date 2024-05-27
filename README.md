# TennisDataAnalysis
## Description 
This project contains a data analysis of the 2022 Men's ATP Tour. The goal of this project is to uncover interest insights and trends in the game of tennis, which could be valuable for players, coaches, and fans alike. 

## Data 
Data was collected from the following open source links in November 2023. 
http://www.tennis-data.co.uk/alldata.php

https://www.kaggle.com/datasets/guillemservera/tennis

## Installation 
This project uses R. You will need to have the latest verison of R installed on your system.

You can download R from the Comprehensive R Archive Network (CRAN) at the following link: https://cran.r-project.org/ 

Once you have R installed, you will need to install the following R packages using the install.packages() command: 
- tidyverse
- dplyr
- psych
- ggplot2
- readxl
- janitor
- stringdist
- stringr
- ggrepel
- pander

## Usage
Once you have the necessary dependencies installed and the source files saved to your computer, you can run the main script 'Analysis.Rmd' in RStudio. This will source any dependant script files as needed (i.e. 'Libraries.R', 'Functions.R', 'Data_Cleaning.R') and any data files ('atp_matches_2022.csv', 'ATP_tour_2022.xlsx') so please ensure that you have them located in the same file location. 

If you would like to simply look at the results of the analysis I suggest investigating the 'analysis.html' file. 


## Contributing
Contributions are welcome! If you have any ideas or suggestions, feel free to open an issue or submit a pull request. 

## License 
This project is licensed under the MIT License. See the 'License.txt' file for more details. 

## Contact 
If you have any questions or comments, feel free to contact me at goedenallyson@gmail.com 