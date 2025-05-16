## Welcome to the Vermont Futures Interactive Dashboard repository!
This project is a collaboration between Vermont Futures Projet and the senior seminar for the statistics major
at Middlebury College. Will Lavey, Eujin Chae, Carly McAdam (all Middlebury '25), and Alex Lyford (Middlebury College Department of Statistics) 
worked with Kevin Chu to create this dashboard for VFP in spring 2025.

### Navigation
Each page in the app (population, homes, and jobs) has its own R script in the repository. The vt-shiny.R file includes the code that builds the app itself. There is also an R script (pull_data.R) that includes the code for pulling the data from the census bureau that we used to create the app. These data have been downloaded and stored in a [Google Drive folder](https://drive.google.com/drive/folders/1IlANNyHgUhrQPuZXcgKt00Kl1vo3G3mF), and the app pulls from this folder. To update the data, edit the pull_data.R file. To see documentation for the functions stored in the population.R, homes.R, and jobs.R files, see the [function documentation]([link](https://github.com/laveywill/vt-futures/blob/main/function-documentation.md)). 

### Contact Info
This is an ongoing project. If you have questions, concerns, or feedback, please reach out to Kevin Chu (kchu@vtfuturesproject.org) or Alex Lyford (alyford@middlebury.edu).
