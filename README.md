# Somora

## Installing somora
`somora` can be installed directly from GitHub. The packages `devtools`, `knitr`, `rmarkdown`, and `seewave` are required. If any of those packages is not present in R, install it with the following code:

```{r}
install.packages("devtools")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("seewave")
```

Then install `somora` with the following code:

```{r}
install_github(repo= "crodriguez-saltos/somora", build_vignettes= TRUE)
```
