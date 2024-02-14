# administrative_discrimination_analysis

## Overview

This repo features a reproduction of Asmus Leth Olsen, Jonas Høgh Kyhse-Andersen, Donald Moynihan's paper, [**`The Unequal Distribution of Opportunity: A National Audit Study of Bureaucratic Discrimination in Primary School Access`**]( https://doi.org/10.1111/ajps.12584),. 

A replication using the Social Science Reproduction Platform was also produced: 
<https://www.socialsciencereproduction.org/reproductions/b5ba971d-4eee-4a10-8225-4c7d7654d5e0/index>

Aspects of the code were written with the help of the ChatGPT 3.5,the entire chat history is 
available in `inputs/llms/usage.txt`.

## File Structure

The repo is structured as:

-   `input/data` contains the data sources used in analysis including the raw data.
-   `outputs/data` contains the cleaned dataset that was constructed.
-   `outputs/paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts used to simulate, download and clean data.