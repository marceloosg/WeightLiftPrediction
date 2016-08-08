---
title: "Weight Lifting Execice Detection"
author: "Marcelo Guimaraes"
date: "3 de agosto de 2016"
output: html_document
---
```{r}
library(RefManageR)
bib <- ReadBib(system.file("Bib", "document.bib", 
                           package = "RefManageR"), check = FALSE)
BibOptions(check.entries = FALSE, style = "markdown", bib.style = "alphabetic", cite.style = 'alphabetic')
```
*Introduction:

This document is part of a final project of the machine learning class provided by the John Hopkins university through Coursera plataform. 
The maching learning in this document is applied in the context of Human Activity Recognition (HAR), specifically is applied in the recognition of the performance of 5 classes of weight lifting exercises from 6 subjects. The subjects were asked to perform barbell lifts correctly and incorrectly in 5 different ways.
```{r}
Cite(bib, ..., textual = FALSE, before = NULL, after = NULL,
  .opts = list())
```
