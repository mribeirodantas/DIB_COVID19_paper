# How to work with this repository

# Paper published in Data in Brief
Originally, this is the repository for the publication _"Dataset for country 
profile and mobility analysis in the assessment of COVID19 pandemic"_ in the 
journal [Data in Brief](https://www.journals.elsevier.com/data-in-brief). You 
can check the paper [here](https://doi.org/10.1016/j.dib.2020.105698). Even 
after the publication, we still update it with the most updated data on 
COVID-19 and mobility. Therefore, right now this repository has more data than 
when the paper was published.

If you want the dataset and data dictionary just like it was on the publication,
click [here](https://data.mendeley.com/datasets/tggrsbz3bb/11). If you want the
most updated version of the dataset and data dictionary, click
[here](https://data.mendeley.com/datasets/tggrsbz3bb). The source code and
pipelines are also updated. If you want to see the repository just like it was
on the moment of the publication, click
[here](https://dagshub.com/mrd/DIB_COVID19_paper/src/39a9a8e54b142cf415c8c87d04e89362b48b24dc).

## Clone the DAGsHub repository
`git clone https://dagshub.com/mrd/DIB_COVID19_paper.git`

This will copy the git repository to your computer. After that, move into the
directory by typing:

`cd DIB_COVID19_paper`

### Install DVC
If you're on GNU/Linux, you can install DVC with pip:

`pip install dvc`

More install options [here](https://dvc.org/doc/install).

If you don't want the most updated version of this repository, but the version
of the publication, you should move the repository back in time by typing:
`git checkout tags/v1.0`

## Get data
If you move into data/preprocessed/ you will see  that the folder is empty. Yes,
it is, dataset files should not be there, afterall they are not tracked by git.
The same thing applied to data/raw. It is not recommended to track big files and
objects with git. The documentation folder folder will not be there either
(though you can see the documentation.dvc file there, which means DVC tracks it
).

`dvc pull`

It's the same reasoning as if you wanted to get the latest tracked code by git
(git pull). Now you will find the files in the data/raw and data/preprocessed
files, and also the documentation files in the documentation folder.

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



