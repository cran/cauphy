## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- label="data", fig.show='hold', fig.height=6, fig.width=7, warning=FALSE, message=FALSE----
library(cauphy)
data(lizards)
attach(lizards)

plot(phy, show.tip.label = FALSE, x.lim = 65)
phydataplot(svl, phy, scaling = 2)
nodelabels(node = c(142, 151, 157))

## ---- label="fit_cauphy_reml"-------------------------------------------------
cauchy_reml <- fitCauchy(phy, svl, model = "cauchy", method = "reml")
cauchy_reml

## ---- label="fit_cauphy_reml_helpers"-----------------------------------------
confint(cauchy_reml)

## ---- label="reml_prl_plot", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
prl <- profile(cauchy_reml)
plot(prl)

## ---- label="fit_cauphy_ml"---------------------------------------------------
cauchy_ml <- fitCauchy(phy, svl, model = "cauchy", method = "fixed.root")
cauchy_ml

## ---- label="fit_cauphy_ml_helpers"-------------------------------------------
confint(cauchy_ml)

## ---- label="ml_prl_plot", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
prl <- profile(cauchy_ml)
plot(prl)

## ---- label="fit_phylolm"-----------------------------------------------------
library(phylolm)
bm_ml <- phylolm(svl ~ 1, phy = phy, model = "BM")
ou_ml <- phylolm(svl ~ 1, phy = phy, model = "OUfixedRoot")
eb_ml <- phylolm(svl ~ 1, phy = phy, model = "EB")

data.frame(model = c("BM", "OU", "EB", "CP"),
           AIC = c(AIC(bm_ml), AIC(ou_ml), AIC(eb_ml), AIC(cauchy_ml)))

## ---- label="reml_anc"--------------------------------------------------------
anc_reml <- ancestral(cauchy_reml, n_values = 200)

## ---- label="reml_anc_plot", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
plot(anc_reml, node = c(142, 151), type = "l")

## ---- label="reml_anc_plot_hdi", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
library(HDInterval)
anc_int <- hdi(anc_reml) # defaults to 95% HDI
plot(anc_reml, node = c(142, 151), intervals = anc_int, type = "l")

## ---- label="reml_anc_hdi_bi", warning=FALSE, message=FALSE-------------------
anc_int[["151"]]

## ---- label="reml_anc_plot_hdi_bad", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
anc_bad <- ancestral(cauchy_reml, n_values = 10) # grid is too coarse
anc_int_bad <- hdi(anc_bad)
plot(anc_bad, node = c(142, 151), intervals = anc_int_bad, type = "l")

## ---- label="reml_inc"--------------------------------------------------------
inc_reml <- increment(cauchy_reml, node = c(142, 151, 157), n_cores = 1)

## ---- label="reml_inc_plot", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
plot(inc_reml, node = c(142, 151), type = "l")

## ---- label="reml_inc_plot_hdi", fig.show='hold', fig.height=4, fig.width=7, warning=FALSE, message=FALSE----
inc_int <- hdi(inc_reml) # defaults to 95% HDI
plot(inc_reml, node = c(142, 151), intervals = inc_int, type = "l")

## ---- label="reml_asr_plot", fig.show='hold', fig.height=8, fig.width=7, warning=FALSE, message=FALSE----
plot_asr(cauchy_reml, anc = anc_reml, inc = inc_reml,
         show.tip.label = FALSE,
         width.node = 0.8, height.node = 1.8,
         width.edge = 1.5, height.edge = 0.8)

