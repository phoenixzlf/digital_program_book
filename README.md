# Digital Program Book for Conferences

This repository includes some scripts that can generate digital program books for conferences.

## Background
First of all, here is a bit of the history of this project. In 2021, when we were still in the pandemic era, the 24th International Congress on Insurance: Mathematics and Economics was held virtually and hosted by the University of Illinois Urbana-Champaign and the Pennsylvania State University in the United States, Ulm University in Germany, and the University of New South Wales (UNSW Sydney) in Australia. Researchers, practitioners, and students participated this conference at different places all over the world. 

To facilitate this event, we needed a program book that was capable of showing the schedule corresponding to different time zones, which led to the idea of creating a digital program book with great accessibility and some customizability with respect to time zone. We also wanted the digital version to be approachable to people who were more familiar with more traditional formats, such as paper and PDF documents. Therefore, we decided to go with a single-page static website, which could be easily printed out to PDF files, and the HTML file could be downloaded to local devices, such as mobile phones and tablets, and be viewed in browsers without requiring Internet access.

In 2023, when I was working at the Drake University, we hosted the 58th Actuarial Research Conference. We had a small organizing team and had to be hands-on for lots of tasks, including designing and producing the program book. I reused the template that I created for IME 2021 with some modifications. In this version, time zone choices were no longer necessary thus being removed because the conference was in person. The scripts and outputs in this repository correspond to this version. 


## Instructions
### Preparing Data
In the `session_info` folder, there are two `.csv` files. `full_program.csv`is for detailed information about talks, including their time slots, titles, abstracts, speakers' information, etc. `outline.csv` is for a high-level overview of the agenda. Please use the existing two sample files as templates to prepare your own data files. 

### Generating HTML Tables
Based on the prepared `.csv` files, `prepare_data_combined_outline_full_program.R` creates HTML tables that will be put into the program book. These tables are saved in the`saved_tables` folder.

### Output HTML Program Book
`ARC2023 Outline and Full Program Information.Rmd` (You should probably name it something else.) assembles all the HTML tables saved in the previous step, and you can knit it into an HTML file. 


## Future Work
This program book is nowhere close to being a polished product. I find its current state adequate for various occasions and may make some improvements in the future. Feel free to use and modify the code to suit your needs. 



