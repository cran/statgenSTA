## ----setup, include = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dim = c(7, 4)
)
op <- options(width = 90)
options("statgen.trialColors" = c("#9E0142FF", "#35B779FF", "#B4DE2CFF",
                                  "#006837FF", "#D53E4FFF"))
library(statgenSTA)

## ----loadData---------------------------------------------------------------------------
data(dropsRaw)

## ----createTD---------------------------------------------------------------------------
## Create a TD object containing data for 2012.
dropsTD <- createTD(data = dropsRaw[dropsRaw$year == 2012, ],
                    genotype = "Variety_ID", 
                    trial = "Experiment",
                    loc = "Site",
                    repId = "Replicate", 
                    subBlock = "block",
                    rowCoord = "Row", 
                    colCoord = "Column", 
                    trLat = "Lat", 
                    trLong = "Long")

## ----getMeta----------------------------------------------------------------------------
## Extract metadata from the TD object. 
(dropsMeta <- getMeta(TD = dropsTD))

## ----setMeta----------------------------------------------------------------------------
## Set trial data as 1-1-2012.
dropsMeta$trDate <- as.Date(rep("010112", times = 5), "%d%m%y")
dropsTD <- setMeta(TD = dropsTD, meta = dropsMeta)

## ----addTD, R.options=list(width=90)----------------------------------------------------
## Add the data for the 2013 trials to the TD object.
dropsTD <- addTD(TD = dropsTD, 
                 data = dropsRaw[dropsRaw$year == 2013, ],
                 genotype = "Variety_ID", 
                 trial = "Experiment",
                 loc = "Site", 
                 repId = "Replicate", 
                 subBlock = "block",
                 rowCoord = "Row", 
                 colCoord = "Column", 
                 trLat = "Lat", 
                 trLong = "Long")

## Inspect the metadata after the extra trial was added.
getMeta(TD = dropsTD)

## ----TDsum------------------------------------------------------------------------------
## Create a summary for grain yield in Gai12W.
summary(dropsTD, 
        trial = "Gai12W", 
        traits = "grain.yield")

## ----TDsumGroup-------------------------------------------------------------------------
## Create a summary per family in Gai12W
summary(dropsTD, 
        trial = "Gai12W", 
        traits = "grain.yield", 
        groupBy = "geneticGroup")

## ----colorOpts, eval=FALSE--------------------------------------------------------------
#  ## Set default colors for genotypes and trials.
#  options("statgen.genoColors" = c("blue", "green", "yellow"))
#  options("statgen.trialColors" = c("red", "brown", "purple"))

## ----layoutPlot-------------------------------------------------------------------------
plot(dropsTD, 
     trials = "Gai12W")

## ----layoutPlotHL-----------------------------------------------------------------------
## Plot the layout for Gai12W.
## Highlight genotypes A3 and 11430 in red and blue.
plot(dropsTD, 
     trials = "Gai12W", 
     highlight = c("A3", "11430"),
     colHighlight = c("red", "blue"))

## ----layoutPlotSB, fig.dim = c(7, 5)----------------------------------------------------
## Plot the layout for Gai12W.
## Color sub blocks using polychrome colors for high contrast.
## Colors are specified here since this color palette is only available as such
## from R > 4.0.
cols <- c("#5A5156", "#E4E1E3", "#F6222E", "#FE00FA", "#16FF32", "#3283FE", 
          "#FEAF16", "#B00068", "#1CFFCE", "#90AD1C", "#2ED9FF", "#DEA0FD", 
          "#AA0DFE", "#F8A19F", "#325A9B", "#C4451C", "#1C8356", "#85660D", 
          "#B10DA1", "#FBE426", "#1CBE4F", "#FA0087", "#FC1CBF", "#F7E1A0", 
          "#C075A6", "#782AB6", "#AAF400", "#BDCDFF", "#822E1C", "#B5EFB5", 
          "#7ED7D1", "#1C7F93", "#D85FF7", "#683B79", "#66B0FF", "#3B00FB")
plot(dropsTD, 
     trials = "Gai12W", 
     colorSubBlock = TRUE,
     colSubBlock = cols)

## ----layoutPlotSG, fig.dim = c(7, 6)----------------------------------------------------
## Plot the layout for Gai12W, label the genotypes.
plot(dropsTD, 
     trials = "Gai12W", 
     showGeno = TRUE)

## ----layoutPlotGY, fig.dim = c(7, 5)----------------------------------------------------
## Plot the layout for Gai12W, show raw data for grain yield.
plot(dropsTD, 
     trials = "Gai12W", 
     traits = "grain.yield")

## ----mapPlot----------------------------------------------------------------------------
## Plot the locations of the trials on a map.
plot(dropsTD, 
     plotType = "map")

## ----mapPlotCol-------------------------------------------------------------------------
## Plot the locations of the trials on a map.
## Color the trials by water scenario.
plot(dropsTD, 
     plotType = "map",
     colorTrialBy = "scenarioWater",
     colTrial = c("red", "darkgreen"))

## ----boxPlot----------------------------------------------------------------------------
## Create a boxplot for grain yield.
plot(dropsTD, 
     plotType = "box", 
     traits = "grain.yield")

## ----boxPlotGR--------------------------------------------------------------------------
## Create a boxplot for grain yield with boxes grouped by year 
## Color the boxes by scenario within years.
plot(dropsTD, 
     plotType = "box", 
     traits = "grain.yield", 
     groupBy = "year", 
     colorTrialBy = "scenarioFull")

## ----corPlot----------------------------------------------------------------------------
## Create a correlation plot for grain yield.
plot(dropsTD, 
     plotType = "cor", 
     traits = "grain.yield")

## ----scatPlot, fig.dim = c(8, 8)--------------------------------------------------------
## Create a scatter plot matrix for grain yield.
## Color trials by scenario and genotypes by family.
plot(dropsTD, 
     plotType = "scatter", 
     traits = "grain.yield", 
     colorTrialBy = "scenarioFull", 
     colorGenoBy = "geneticGroup")

## ----fitSp, message=FALSE---------------------------------------------------------------
## Fit a single trial model using a model based on a resolvable row column design.
modDropsSp <- fitTD(TD = dropsTD, 
                    trials = "Gai12W", 
                    traits = "grain.yield",
                    design = "res.rowcol")

## ----fitSpSm, message=FALSE-------------------------------------------------------------
## Fit a single trial model with genotype as random effect.
modDropsSp2 <- fitTD(TD = dropsTD, 
                     trials = "Gai12W", 
                     traits = "grain.yield", 
                     what = "random", 
                     design = "res.rowcol")

## ----fitSpCtr, message=FALSE------------------------------------------------------------
## Fit a spatial single trial model using SpATS. 
## Manually specify the number of segments for rows and columns.
modDropsSp3 <- fitTD(TD = dropsTD, 
                     trials = "Gai12W", 
                     traits = "grain.yield", 
                     design = "res.rowcol", 
                     control = list(nSeg = c(28, 18)))

## ----fitAs, message=FALSE, results='hide', warning=FALSE--------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  ## Fit a spatial single trial model using asreml.
  modDropsAs <- fitTD(TD = dropsTD, 
                      trials = "Gai12W", 
                      traits = "grain.yield", 
                      design = "res.rowcol", 
                      spatial = TRUE, 
                      engine = "asreml",
                      control = list(criterion = "BIC"))
}

## ----spatCh, R.options=list(width=90)---------------------------------------------------
if (exists("modDropsAs")) {
  ## Overview of fitted models.
  print(modDropsAs$Gai12W$sumTab$grain.yield, digits = 2, row.names = FALSE)
}  

## ----fitSum, message=FALSE--------------------------------------------------------------
## Set nBest to 5 to limit the output to the best 5 genotypes.
summary(modDropsSp, 
        nBest = 5)

## ----basePlot---------------------------------------------------------------------------
## Base plots for the model with genotype fitted as random effect.
plot(modDropsSp, 
     plotType = "base", 
     what = "random")

## ----spatPlot---------------------------------------------------------------------------
## Spatial plot for the model with genotype fitted as fixed effect.
plot(modDropsSp, 
     plotType = "spatial")

## ----spatPlotPerc-----------------------------------------------------------------------
## Spatial plot for the model with genotype fitted as fixed effect.
## Display the spatial trend as a percentage.
plot(modDropsSp, 
     plotType = "spatial", 
     spaTrend = "percentage")

## ----outDet-----------------------------------------------------------------------------
## Outlier detection for the model with genotype fitted as random.
outliers <- outlierSTA(modDropsSp, 
                       traits = "grain.yield",
                       what = "random")

## ----outDetCom--------------------------------------------------------------------------
## Outlier detection for the model with genotype fitted as random.
## A custom limit is used and commonFactors set to genotype.
outliers <- outlierSTA(modDropsSp, 
                       traits = "grain.yield", 
                       what = "random",
                       rLimit = 2.7, 
                       commonFactors = "genotype")

## ----modRep, eval=FALSE-----------------------------------------------------------------
#  ## Create a report in the current working directory
#  report(modDropsSp)
#  ## Create a report for the model with genotype fitted as random.
#  report(modDropsSp,
#         outfile = "./myReports/dropsReport.pdf",
#         what = "random")

## ----extractOpts, results="as.is", echo=FALSE, out.width = "\\textwidth"----------------
## Generate table of options for extract from internal data.
optsTab <- statgenSTA:::extractOptions[, c("result", "model", "description", "asDataFrame")]
optsTab$asDataFrame <- factor(ifelse(optsTab$asDataFrame == 0, 2, 1),
                              labels = c("yes", ""))
optsTab <- optsTab[order(optsTab[["model"]]), ]
knitr::kable(optsTab, align = "l", row.names = FALSE)

## ----extBLUEs---------------------------------------------------------------------------
## Extract BLUEs.
BLUEsDrops <- extractSTA(STA = modDropsSp, 
                         what = "BLUEs")
## Extract BLUEs and BLUPs.
predDrops <- extractSTA(STA = modDropsSp, 
                        what = c("BLUEs", "BLUPs"))

## ----extBLUEsKeep-----------------------------------------------------------------------
## Extract BLUEs from the fitted model.
BLUEsDrops2 <- extractSTA(STA = modDropsSp, 
                          what = "BLUEs", 
                          keep = "scenarioWater")
head(BLUEsDrops2)

## ----extFit-----------------------------------------------------------------------------
## Extract fitted values from the model.
## Add repId and family to the output.
fitVals <- extractSTA(STA = modDropsSp, 
                      what = "fitted", 
                      keep = c("repId", "geneticGroup"))
head(fitVals)

## ----STAtoTD, message=FALSE, eval=FALSE-------------------------------------------------
#  ## Fit a model for all trials with genotype as fixed factor.
#  modDropsSpTot <- fitTD(TD = dropsTD,
#                         traits = "grain.yield",
#                         what = "fixed",
#                         design = "res.rowcol")
#  ## Create a TD object containing BLUEs and standard errors of BLUEs.
#  TDGxE <- STAtoTD(STA = modDropsSpTot,
#                   what = c("BLUEs", "seBLUEs"))
#  ## Add weights and water scenario to the output.
#  TDGxE2 <- STAtoTD(STA = modDropsSpTot,
#                    what = c("BLUEs", "seBLUEs"),
#                    keep = "scenarioWater",
#                    addWt = TRUE)

## ----winddown, include = FALSE------------------------------------------------
options(op)

