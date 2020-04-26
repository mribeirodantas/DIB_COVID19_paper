# How to work with this repository

## Clone the DAGsHub repository
`git clone https://dagshub.com/mrd/DIB_COVID19_paper.git`

This will copy the git repository to your computer. After that, move into the
directory by typing:

`cd DIB_COVID19_paper`

### Install DVC
If you're on GNU/Linux, you can install DVC with pip:

`pip install dvc`

More install options [here](https://dvc.org/doc/install).

## Get data
If you move into data/preprocessed/ you will see the folder is empty. Yes, it
is, dataset files should not be there, afterall they are not tracked by git. The
same thing applies to data/raw. In order to get the data, you should run:

`dvc pull`

It's the same reasoning as if you wanted to get the latest tracked code by git
(git pull). Now you will find the files in the data/raw and data/preprocessed
files.

## Reproduce the pipeline
Let's say you have an updated version of one or more of the raw files, on the
same format (column names did not change, for example), and you want to
reproduce the pipeline (generate an updated version of the preprocessed file).
From the root directory of the repository, run:

`dvc repro preprocess.dvc`

DVC will automatically notice that one of the raw files changed and therefore it
will reproduce the pipeline and generate a new output file in the
data/preprocessed folder. If nothing changed, dvc will realize there is no
reason to reproduce the pipeline and you will see the following message:

`Data and pipelines are up to date.`

Well, maybe you want to *force* a reproduction fo the pipeline because... well,
you want to see how it would be like. For this, you should run:

`dvc repro -f preprocess.dvc`

# Download
This final dataset can also be downloaded from Mendeley Data by clicking [here](https://data.mendeley.com/datasets/tggrsbz3bb/6).



