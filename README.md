# Intelligent Systems: NLP Project

## Contents
```
├── README.md
├── input
│   └── arxiv_archive.zip
├── analysis.R
├── report.rmd
└── utils.R
```
The `input/arxiv_archive.zip` is the input data set. It gets unzipped when the code is first executed.

The `analysis.R` holds the source code for the analysis that is shown in the `report.rmd`. 

The `report.rmd` is the R Markdown file that created the submitted `report.pdf`.

The `utils.R` holds the source code for helper functions such as data loading, creation of corpus, tdm, etc.

## How to Run Code

This code makes use of the `here` package to define paths relative to the active working directory. It might be relevant to set the working directory explicitly.

The `report.rmd` can be rendered from an active R session by executing
```R
r$> rmarkdown::render("report.rmd")
```

The `analysis.R` is intended for interactive usage to experiment with the data. It can be executed line by line to.



