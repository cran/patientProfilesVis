## ----options, echo = FALSE----------------------------------------------------
	
	library(knitr)
	opts_chunk$set(
		error = FALSE, 
		fig.width = 14, fig.height = 7,
		dev = "png",
		out.width = "100%",
		fig.path = "./figures_vignette/",
		fig.align = 'center'
	)
	# include warnings where they occur
	options(warn = 1)
	
	heightLineIn  <- 0.2
	

## ----loadPackages, message = FALSE--------------------------------------------

	library(patientProfilesVis)
	library(pander)


## ----loadData-SDTM------------------------------------------------------------
	
library(clinUtils)

# import example data:
data(dataSDTMCDISCP01)
# formatted as a list of data.frame (one per domain)
dataSDTM <- dataSDTMCDISCP01
names(dataSDTM)

# and corresponding labels
labelVarsSDTM <- attr(dataSDTM, "labelVars")
head(labelVarsSDTM)	
	

## ----loadData-ADaM------------------------------------------------------------
	
# import example data:
data(dataADaMCDISCP01)
# formatted as a list of data.frame (one per domain)
dataADaM <- dataADaMCDISCP01
names(dataADaM)

# and corresponding labels
labelVarsADaM <- attr(dataADaM, "labelVars")
head(labelVarsADaM)

# example subjects for the vignette:
subjectAE <- "01-718-1427"
subjectMH <- "01-718-1371"
subjectCM <- "01-701-1148"
subjectLB <- "01-704-1445"
	

## ----text-wideFormat----------------------------------------------------------

	# annotate subject demographics meta-data
	# by specifying a set of variables to include
	dmPlots <- subjectProfileTextPlot(
		data = dataSDTM$DM,
		paramValueVar = c("SEX|AGE", "RACE|COUNTRY", "ARM"),
		labelVars = labelVarsSDTM
	)
	

## ----text-wideFormat-include, echo = FALSE, fig.height = attributes(dmPlots[[1]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Demographic information with the 'subjectProfileTextPlot' function for patient:", names(dmPlots)[1])----

	print(dmPlots[[1]][[1]])


## ----text-longFormat-noGrouping-----------------------------------------------

	# annotate subject medical history
	# by specifying a combination of parameter value/name
	mhPlots <- subjectProfileTextPlot(
		data = dataSDTM$MH,
		paramNameVar = c("MHDECOD"),
		paramValueVar = c("MHSTDTC", "MHSEV"),
		paramGroupVar = "MHCAT",
		title = "Medical History: status",
		labelVars = labelVarsSDTM
	)
		

## ----text-longFormat-noGrouping-include, echo = FALSE, fig.height = attributes(mhPlots[[subjectMH]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Medical history with the 'subjectProfileTextPlot' function for patient:", subjectMH)----

	print(mhPlots[[subjectMH]][[1]])


## ----text-tableFormat---------------------------------------------------------

	aeListingPlots <- subjectProfileTextPlot(
		data = dataSDTM$AE,
		paramValueVar = c(
			"AEBODSYS", "AESOC", "AEHLT", 
			"AELLT", "AEDECOD", "AESTDTC", 
			"AEENDTC", "AESER", "AEACN"
		),
		paramGroupVar = "AESTDTC",
		labelVars = labelVarsSDTM,
		table = TRUE
	)
	

## ----text-tableFormat-include, echo = FALSE, fig.height = attributes(aeListingPlots[[subjectAE]][[1]])$metaData$nLines*heightLineIn, fig.width = 14, fig.cap = paste("Adverse event listing with the 'subjectProfileTextPlot' function for patient:", subjectAE)----

	print(aeListingPlots[[subjectAE]][[1]])


## ----text-tableFormat-customWidth---------------------------------------------

	aeListingPlots <- subjectProfileTextPlot(
		data = dataSDTM$AE,
		paramValueVar = c(
			"AEBODSYS", "AESOC", "AEHLT", 
			"AELLT", "AEDECOD", "AESTDTC", 
			"AEENDTC", "AESER", "AEACN"
		),
		paramGroupVar = "AESTDTC",
		labelVars = labelVarsSDTM,
		table = TRUE,
		colWidth = c(
			0.2, 0.2, 0.05, 
			0.1, 0.1, 0.05, 
			0.05, 0.05, 0.05
		)
	)
	

## ----text-tableFormat-customWidth-include, echo = FALSE, fig.width = 14, fig.height = attributes(aeListingPlots[[1]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Adverse event listing with the 'subjectProfileTextPlot' function for patient:", names(aeListingPlots)[1])----

	print(aeListingPlots[[subjectAE]][[1]])


## ----text-longFormat-multipleVariables----------------------------------------

	# annotate subject medical history
	# by specifying a combination of parameter value/name
	paramValueVarFct <- function(data)
		with(data, paste0(
			ifelse(MHSEV != "", paste("severity:", MHSEV, ""), ""),
			"(start = ", ifelse(MHSTDTC != "", MHSTDTC, "undefined"), ")"
		))
	mhPlotsMultipleVars <- subjectProfileTextPlot(
		data = dataSDTM$MH,
		paramNameVar = "MHDECOD",
		paramValueVar = paramValueVarFct,
		title = "Medical History: status with dates",
		labelVars = labelVarsSDTM
	)
		

## ----text-longFormat-multipleVariables-include, echo = FALSE, fig.height = attributes(mhPlotsMultipleVars[[subjectMH]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Medical history with the 'subjectProfileTextPlot' function for patient:", subjectMH)----

	print(mhPlotsMultipleVars[[subjectMH]][[1]])


## ----text-longFormat-grouping-------------------------------------------------

	# annotate subject medical history
	# by specifying a combination of parameter value/name
	mhPlotsGroup <- subjectProfileTextPlot(
		data = dataSDTM$MH,
		paramNameVar = "MHDECOD",
		paramValueVar = c("MHDECOD", "MHSTDTC"),
		paramGroupVar = "MHCAT",
		title = "Medical History: grouped by category",
		labelVars = labelVarsSDTM
	)
	

## ----text-longFormat-grouping-include, echo = FALSE, fig.height = attributes(mhPlotsGroup[[subjectMH]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Medical history with the 'subjectProfileTextPlot' function for patient:", subjectMH)----

	print(mhPlotsGroup[[subjectMH]][[1]])


## ----interval-ae--------------------------------------------------------------

	dataAE <- dataSDTM$AE
	
	# sort severities
	dataAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
	
	aePlots <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTM,
		title = "Adverse events"
	)
		

## ----interval-ae-include, echo = FALSE, fig.height = attributes(aePlots[[subjectAE]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Adverse events with the 'subjectProfileIntervalPlot' function for patient:", subjectAE)----

	print(aePlots[[subjectAE]][[1]])


## ----interval-ex--------------------------------------------------------------

	exPlots <- subjectProfileIntervalPlot(
		data = dataSDTM$EX,
		paramVar = c("EXTRT", "EXDOSE", "EXDOSU"),
		timeStartVar = "EXSTDY",
		timeEndVar = "EXENDY",
		colorVar = "EXDOSFRM",
		labelVars = labelVarsSDTM,
		title = "Treatment exposure"
	)


## ----interval-ex-include, echo = FALSE, fig.height = attributes(exPlots[[1]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Exposure interval with the 'subjectProfileIntervalPlot' function for patient:", names(exPlots)[1])----

	print(exPlots[[1]][[1]])


## ----interval-cm--------------------------------------------------------------

	cmPlots <- subjectProfileIntervalPlot(
		data = dataSDTM$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMCLAS",
		colorVar = "CMCLAS",
		labelVars = labelVarsSDTM,
		title = "Concomitant medications"
	)


## ----interval-cm-include, echo = FALSE, fig.height = attributes(cmPlots[[subjectCM]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Concomitant medications with the 'subjectProfileIntervalPlot' function for patient:", subjectCM)----

	print(cmPlots[[subjectCM]][[1]])


## ----interval-ae-default, message = TRUE--------------------------------------

	aePlots <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTM,
		title = "Adverse events"
	)
		

## ----interval-ae-default-include, echo = FALSE, fig.height = attributes(aePlots[[subjectAE]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Adverse events with the 'subjectProfileIntervalPlot' function for patient:", subjectAE)----

	print(aePlots[[subjectAE]][[1]])


## ----formatSVData-------------------------------------------------------------

dataSV <- dataSDTM$SV
dataSV$RFSTDTC <- dataSDTM$DM[match(dataSV$USUBJID, dataSDTM$DM$USUBJID), "RFSTDTC"]
dataSV$SVSTDY <- with(dataSV, as.numeric(as.Date(SVSTDTC)-as.Date(RFSTDTC)+1))
dataSV$SVENDY <- with(dataSV, as.numeric(as.Date(SVENDTC)-as.Date(RFSTDTC)+1))



## ----interval-ae-timeLimData, message = TRUE----------------------------------
	
	aePlotsTimLimFromSV <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		colorVar = "AESEV",
		labelVars = labelVarsSDTM,
		title = "Adverse events",
		timeLimData = dataSV,
		timeLimStartVar = "SVSTDY", timeLimStartLab = "First subject visit", 
		timeLimEndVar = "SVENDY", timeLimEndLab = "Last subject visit", 
	)
	

## ----interval-ae-timeLimData-include, echo = FALSE, fig.height = attributes(aePlotsTimLimFromSV[[subjectAE]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste0("Adverse events with the 'subjectProfileIntervalPlot' function for patient:", subjectAE, ". Missing start/end date are extracted from the subject-level dataset.")----

	print(aePlotsTimLimFromSV[[subjectAE]][[1]])


## ----interval-ae-timeLimData-svData-------------------------------------------
	svSubjectAE <- subset(dataSV, USUBJID == subjectAE)[, c("VISIT", "SVSTDY", "SVENDY")]
	pander(svSubjectAE)

## ----interval-cm-restrictedTimeLimits-----------------------------------------

	cmPlotsTimeSV <- subjectProfileIntervalPlot(
		data = dataSDTM$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMCLAS",
		colorVar = "CMCLAS",
		labelVars = labelVarsSDTM,
		title = "Concomitant medications",
		timeLimData = dataSV,
		timeLimStartVar = "SVSTDY",
		timeLimEndVar = "SVENDY",
		timeAlign = FALSE
	)


## ----interval-cm-restrictedTimeLimits-include, echo = FALSE, fig.height = attributes(cmPlotsTimeSV[[subjectCM]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Concomitant medications with the 'subjectProfileIntervalPlot' function for patient:", subjectCM, "with time limits restricted to subject visits")----

	print(cmPlotsTimeSV[[subjectCM]][[1]])


## ----interval-ae-customMissingPartialDates------------------------------------

	# add status for dates:
	dataAE$AESTDYST <- with(dataAE, 
		ifelse(is.na(AESTDY) & !is.na(AESTDY), "Missing start", "")
	)
	
	shapePalette <- c(
		`Missing start`= "\u25C4", # left-pointing arrow
		'NOT RECOVERED/NOT RESOLVED' = "\u25BA", # right-pointing arrow
		'RECOVERED/RESOLVED' = "\u25A0", # small square
		'FATAL' = "\u2666", # diamond
		UNKNOWN = "+"
	)
	
	aePlotsShape <- subjectProfileIntervalPlot(
		data = dataAE,
		paramVar = "AETERM",
		timeStartVar = "AESTDY", timeEndVar = "AEENDY",
		timeStartShapeVar = "AESTDYST", timeEndShapeVar = "AEOUT",
		shapePalette = shapePalette,
		shapeLab = "Study date status", 
		colorVar = "AESEV",
		labelVars = labelVarsSDTM,
		title = "Adverse events"
	)


## ----interval-ae-customMissingPartialDates-include, echo = FALSE, fig.height = attributes(aePlotsShape[[subjectAE]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Adverse events with the 'subjectProfileIntervalPlot' function for patient:", subjectAE, "with custom shape specification")----

	print(aePlotsShape[[subjectAE]][[1]])


## ----interval-cm-restrictedTimeLimits2----------------------------------------

	timeLim <- c(0, 182)
	cmPlotsTimeSpec <- subjectProfileIntervalPlot(
		data = dataSDTM$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMCLAS",
		colorVar = "CMCLAS",
		labelVars = labelVarsSDTM,
		title = "Concomitant medications",
		timeLim = timeLim
	)


## ----interval-cm-restrictedTimeLimits2-include, echo = FALSE, fig.height = attributes(cmPlotsTimeSpec[[subjectCM]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Concomitant medications with the 'subjectProfileIntervalPlot' function for patient:", subjectCM, "with time limits restricted to: (", toString(timeLim), ")")----

	print(cmPlotsTimeSpec[[subjectCM]][[1]])


## ----interval-cm-timeAlign-FALSE----------------------------------------------

	cmPlotsNotAligned <- subjectProfileIntervalPlot(
		data = dataSDTM$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMCLAS",
		colorVar = "CMCLAS",
		labelVars = labelVarsSDTM,
		title = "Concomitant medications",
		timeAlign = FALSE
	)


## ----interval-cm-timeAlign-FALSE-include-1, echo = FALSE, fig.height = attributes(cmPlotsNotAligned[[subjectCM]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Adverse events with the 'subjectProfileIntervalPlot' function for patient:", subjectCM, "with custom shape specification")----

	print(cmPlotsNotAligned[[subjectCM]][[1]])



## ----formatLBData-------------------------------------------------------------

# consider a subset of the laboratory data for example:
lbTests <- c("CHOL", "PHOS", "ANISO", "MCHC", "PLAT", "KETONES")
dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)
# sort the categories (empty values '', if any, becomes NA)
dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))


## ----event--------------------------------------------------------------------
	
	# create plot
	lbPlots <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = c("LBCAT", "LBTEST"),
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		labelVars = labelVarsSDTM,
		title = "Laboratory test measurements"
	)


## ----event-include, echo = FALSE, fig.height = attributes(lbPlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function for patient:", subjectLB)----

	print(lbPlots[[subjectLB]][[1]])


## ----event-color--------------------------------------------------------------

	# create plot
	lbPlotsColorShape <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		colorVar = "LBCAT",
		labelVars = labelVarsSDTM,
		shapeVar = "LBNRIND",
		shapePalette = c(
			'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
			'ABNORMAL' = 11
		),
		title = "Laboratory test measurements: reference range indicator"
	)
	

## ----event-color-include, echo = FALSE, fig.height = attributes(lbPlotsColorShape[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with reference range with the 'subjectProfileEventPlot' function for patient:", subjectLB)----

	print(lbPlotsColorShape[[subjectLB]][[1]])


## ----line---------------------------------------------------------------------

	# create plot
	lbLinePlots <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)
	

## ----line-include, echo = FALSE, fig.height = attributes(lbLinePlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileLinePlot' function for patient:", subjectLB)----

	print(lbLinePlots[[subjectLB]][[1]])


## ----line-colorShape----------------------------------------------------------

	# create plot
	lbLinePlotsColorShape <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		colorVar = "LBCAT",
		shapeVar = "LBNRIND",
		shapePalette = c(
			'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
			'ABNORMAL' = 11
		),
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)
	

## ----line-colorShape-include, echo = FALSE, fig.height = attributes(lbLinePlotsColorShape[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with reference range with the 'subjectProfileLinePlot' function for patient:", subjectLB)----

	print(lbLinePlotsColorShape[[subjectLB]][[1]])


## ----line-paramValueRangeVar--------------------------------------------------

	# create plot
	lbLineRefRangePlots <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramGroupVar = "LBCAT",
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		shapeVar = "LBNRIND",
		shapePalette = c(
			'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
			'ABNORMAL' = 11
		),
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)
	

## ----line-paramValueRangeVar-value-include, echo = FALSE, fig.height = attributes(lbLineRefRangePlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileLinePlot' function with a reference range for patient:", subjectLB)----

	print(lbLineRefRangePlots[[subjectLB]][[1]])


## ----line-yLimFrom-value------------------------------------------------------

	# create plot
	lbLineYLimFromValuePlots <- subjectProfileLinePlot(
		data = dataLB,
		paramNameVar = "LBTEST", 
		paramValueVar = "LBSTRESN",
		paramGroupVar = "LBCAT",
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		shapeVar = "LBNRIND",
		shapePalette = c(
			'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
			'ABNORMAL' = 11
		),
		yLimFrom = "value",
		timeVar = "LBDY",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)
	

## ----line-yLimFrom-value-include, echo = FALSE, fig.height = attributes(lbLineYLimFromValuePlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileLinePlot' function for patient:", subjectLB)----

	print(lbLineYLimFromValuePlots[[subjectLB]][[1]])


## ----event-subset-------------------------------------------------------------

	# create plot
	lbPlotsSubset <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		# select subjects of interest:
		subsetData = dataSDTM$AE,
		subsetVar = "AESEV", subsetValue = "SEVERE",
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		shapeVar = "LBNRIND",
		shapePalette = c(
			'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
			'ABNORMAL' = 11
		),
		title = "Hematology test measurements",
		labelVars = labelVarsSDTM
	)
	cat("Only the", length(lbPlotsSubset), "patients with severe adverse events:", toString(names(lbPlotsSubset)), "are considered.\n")
	

## ----event-subset-2-----------------------------------------------------------

	# create plot
	lbPlotsSubjectSubset <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		subsetVar = "LBCAT", subsetValue = "HEMATOLOGY",
		subjectSubset = subjectLB,
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		shapeVar = "LBNRIND",
		shapePalette = c(
			'LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24, 
			'ABNORMAL' = 11
		),
		title = "Laboratory test measurements for subject of interest",
		labelVars = labelVarsSDTM
	)
	cat("Only the patient:", toString(names(lbPlotsSubjectSubset)), "is considered.\n")
	

## ----lab-SDTM-categories-default----------------------------------------------
			
	dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)

	# LBRIND is a character: elements sorted in alphabetical order
	lbPlotsColor <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)
	

## ----lab-SDTM-categories-default-include, echo = FALSE, fig.height = attributes(lbPlotsColor[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function with color/shape ordered alphabetically for patient:", subjectLB)----

print(lbPlotsColor[[subjectLB]][[1]])


## ----lab-SDTM-categories-sorted-----------------------------------------------
	
	dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)
	# sort LBRIND
	dataLB$LBNRIND <- with(dataLB, 
		factor(LBNRIND, levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))
	)
	
	# create plot
	lbPlotsColor <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		colorVar = "LBNRIND",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)
	

## ----lab-SDTM-categories-sorted-include, echo = FALSE, fig.height = attributes(lbPlotsColor[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function with color/shape ordered as specified for patient:", subjectLB)----

print(lbPlotsColor[[subjectLB]][[1]])


## ----lab-ADaM-categories-sorted-numeric---------------------------------------
	
	dataLB <- subset(dataSDTM$LB, LBTESTCD %in% lbTests)
	
	# for the demo, creates numeric variable associated to reference range
	# (often already available)
	dataLB$LBNRINDN <- c(LOW = 1, NORMAL = 2, HIGH = 3, ABNORMAL = 10)[dataLB$LBNRIND]
	
	dataLB$LBNRIND <- with(dataLB, reorder(LBNRIND, LBNRINDN))
	
	lbPlotsColor <- subjectProfileEventPlot(
		data = dataLB,
		paramVar = "LBTEST",
		paramGroupVar = "LBCAT",
		timeVar = "LBDY",
		colorVar = "LBNRIND", shapeVar = "LBNRIND",
		title = "Laboratory test measurements: actual value",
		labelVars = labelVarsSDTM
	)


## ----lab-SDTM-categories-sorted-numeric, echo = FALSE, fig.height = attributes(lbPlotsColor[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function with color/shape ordered based on the corresponding numeric variable for patient:", subjectLB)----

print(lbPlotsColor[[subjectLB]][[1]])


## ----palettes-default-get-----------------------------------------------------

# display default palettes
colorsDefault <- getOption("patientProfilesVis.colors")
str(colorsDefault)
shapesDefault <- getOption("patientProfilesVis.shapes")
shapesDefault


## ----palettes-default-example-------------------------------------------------

# create plot
lbPlots <- subjectProfileEventPlot(
	data = dataLB,
	paramVar = "LBTEST",
	paramGroupVar = "LBCAT",
	timeVar = "LBDY",
	colorVar = "LBNRIND", 
	shapeVar = "LBNRIND", 
	title = "Laboratory test measurements: actual value",
	labelVars = labelVarsSDTM
)


## ----palettes-default-example-include, echo = FALSE, fig.height = attributes(lbPlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with reference range with the 'subjectProfileLinePlot' function with default colors/shapes for patient:", subjectLB)----

print(lbPlots[[subjectLB]][[1]])


## ----palettes-customGeneral-set-----------------------------------------------

# change palettes for the entire R session
options(patientProfilesVis.colors = c("gold", "pink", "cyan"))
options(patientProfilesVis.shapes = c("cross", "diamond", "circle", "square"))


## ----palettes-customGeneral-example-------------------------------------------

# create plot
lbPlots <- subjectProfileEventPlot(
	data = dataLB,
	paramVar = "LBTEST",
	paramGroupVar = "LBCAT",
	timeVar = "LBDY",
	colorVar = "LBNRIND", 
	shapeVar = "LBNRIND", 
	title = "Laboratory test measurements: actual value",
	labelVars = labelVarsSDTM
)


## ----palettes-customGeneral-example-include, echo = FALSE, fig.height = attributes(lbPlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with reference range with the 'subjectProfileLinePlot' function with default colors/shapes for patient:", subjectLB)----

	print(lbPlots[[subjectLB]][[1]])


## ----palettes-default-reset---------------------------------------------------

# change palettes for the entire R session
options(patientProfilesVis.colors = colorsDefault)
options(patientProfilesVis.shapes = shapesDefault)


## ----palettes-----------------------------------------------------------------

# sort LBNRIND
dataLB$LBNRIND <- with(dataLB, 
	factor(LBNRIND, levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))
)

colorPaletteLBNRIND <- getPaletteCDISC(dataLB$LBNRIND, var = "NRIND", type = "color")
print(colorPaletteLBNRIND)

shapePaletteLBNRIND <- getPaletteCDISC(dataLB$LBNRIND, var = "NRIND", type = "shape")
print(shapePaletteLBNRIND)

# create plot
lbPlots <- subjectProfileEventPlot(
	data = dataLB,
	paramVar = "LBTEST",
	paramGroupVar = "LBCAT",
	timeVar = "LBDY",
	colorVar = "LBNRIND", colorPalette = colorPaletteLBNRIND,
	shapeVar = "LBNRIND", shapePalette = shapePaletteLBNRIND,
	title = "Laboratory test measurements: actual value",
	labelVars = labelVarsSDTM
)


## ----palettes-include, echo = FALSE, fig.height = attributes(lbPlots[[subjectLB]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Laboratory data with the 'subjectProfileEventPlot' function with generic color/shape palettes for patient:", subjectLB)----

print(lbPlots[[subjectLB]][[1]])


## ----interval-cm-example------------------------------------------------------

	cmPlots <- subjectProfileIntervalPlot(
		data = dataSDTM$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMCLAS",
		colorVar = "CMCLAS",
		title = "Concomitant medications",
		labelVars = labelVarsSDTM
	)
	
	subjectCMTimeTrans <- "01-701-1192"


## ----interval-cm-example-include, echo = FALSE, fig.height = attributes(cmPlots[[subjectCMTimeTrans]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Concomitant medications with the 'subjectProfileIntervalPlot' function for patient:", subjectCMTimeTrans)----

	print(cmPlots[[subjectCMTimeTrans]][[1]])


## ----interval-cm-timeTransformation-------------------------------------------

	timeTrans <- getTimeTrans("asinh-neg")
	
	cmPlotsTimeTrans <- subjectProfileIntervalPlot(
		data = dataSDTM$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		paramGroupVar = "CMCLAS",
		colorVar = "CMCLAS",
		timeTrans = timeTrans,
		title = "Concomitant medications",
		labelVars = labelVarsSDTM
	)


## ----interval-cm-timeTransformation-include, echo = FALSE, fig.height = attributes(cmPlotsTimeTrans[[subjectCMTimeTrans]][[1]])$metaData$nLines*heightLineIn, fig.cap = paste("Concomitant medications with the 'subjectProfileIntervalPlot' function with asinh negative transformation of the time axis for patient:", subjectCMTimeTrans)----

	print(cmPlotsTimeTrans[[subjectCMTimeTrans]][[1]])


## ----createReport-SDTM, eval = FALSE------------------------------------------
# 
# # demography
# dmPlots <- subjectProfileTextPlot(
# 	data = dataSDTM$DM,
# 	paramValueVar = c("SEX|AGE", "RACE|COUNTRY", "ARM"),
# 	labelVars = labelVarsSDTM
# )
# 
# # medical history
# mhPlots <- subjectProfileTextPlot(
# 	data = dataSDTM$MH,
# 	paramNameVar = c("MHDECOD"),
# 	paramValueVar = c("MHCAT", "MHTERM", "MHSTDTC"),
# 	title = "Medical History: status",
# 	labelVars = labelVarsSDTM
# )
# 
# # concomitant medications
# cmPlots <- subjectProfileIntervalPlot(
# 	data = dataSDTM$CM,
# 	paramVar = c(
# 		"CMTRT",
# 		"CMDOSE", "CMDOSU", "CMROUTE",
# 		"CMDOSFRQ"
# 	),
# 	timeStartVar = "CMSTDY",
# 	timeEndVar = "CMENDY",
# 	paramGroupVar = "CMCLAS",
# 	colorVar = "CMCLAS",
# 	timeTrans = timeTrans,
# 	title = "Concomitant medications",
# 	labelVars = labelVarsSDTM
# )
# 
# # treatment exposure
# exPlots <- subjectProfileIntervalPlot(
# 	data = dataSDTM$EX,
# 	paramVar = c("EXTRT", "EXDOSE", "EXDOSU"),
# 	timeStartVar = "EXSTDY",
# 	timeEndVar = "EXENDY",
# 	colorVar = "EXDOSFRM",
# 	labelVars = labelVarsSDTM,
# 	title = "Treatment exposure"
# )
# 
# # adverse events:
# dataAE <- dataSDTM$AE
# # sort severities
# dataAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
# aePlots <- subjectProfileIntervalPlot(
# 	data = dataAE,
# 	paramVar = "AETERM",
# 	timeStartVar = "AESTDY",
# 	timeEndVar = "AEENDY",
# 	colorVar = "AESEV",
# 	labelVars = labelVarsSDTM,
# 	title = "Adverse events"
# )
# 
# # laboratory parameter
# lbLinePlots <- subjectProfileLinePlot(
# 	data = dataSDTM$LB,
# 	paramNameVar = "LBTEST",
# 	paramValueVar = "LBSTRESN",
# 	paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
# 	paramGroupVar = "LBCAT",
# 	timeVar = "LBDY",
# 	title = "Laboratory test measurements: actual value",
# 	labelVars = labelVarsSDTM
# )
# 
# # create report
# pathReport <- "subjectProfile_SDTM.pdf"
# createSubjectProfileReport(
# 	listPlots = list(
# 		dmPlots,
# 		mhPlots,
# 		cmPlots,
# 		exPlots,
# 		aePlots,
# 		lbLinePlots
# 	),
# 	outputFile = pathReport
# )
# 

## ----createReport-ADaM, eval = FALSE------------------------------------------
# 
# # demography
# adslPlots <- subjectProfileTextPlot(
# 	data = dataADaM$ADSL,
# 	paramValueVar = c("SEX|AGE", "RACE", "TRT01P"),
# 	labelVars = labelVarsADaM
# )
# 
# # adverse events:
# dataADAE <- dataADaM$ADAE
# # sort severities
# dataADAE[, "AESEV"] <- factor(dataAE[, "AESEV"], levels = c("MILD", "MODERATE", "SEVERE"))
# adaePlots <- subjectProfileIntervalPlot(
# 	data = dataADAE,
# 	paramVar = "AEDECOD",
# 	timeStartVar = "ASTDY",
# 	timeEndVar = "AENDY",
# 	colorVar = "AESEV",
# 	labelVars = labelVarsADaM,
# 	timeTrans = getTimeTrans("asinh-neg"),
# 	title = "Adverse events"
# )
# 
# # laboratory parameter
# adlbcPlots <- subjectProfileLinePlot(
# 	data = dataADaM$ADLBC,
# 	paramNameVar = "PARAM",
# 	paramValueVar = "AVAL",
# 	paramValueRangeVar = c("A1LO", "A1HI"),
# 	paramGroupVar = "PARCAT1",
# 	timeVar = "ADY",
# 	title = "Laboratory test measurements: actual value",
# 	labelVars = labelVarsADaM
# )
# 
# # create report
# pathReport <- "subjectProfile_ADaM.pdf"
# createSubjectProfileReport(
# 	listPlots = list(
# 		adslPlots,
# 		adaePlots,
# 		adlbcPlots
# 	),
# 	outputFile = pathReport
# )
# 

## ----createReport-referenceLines-list, eval = FALSE---------------------------
# 
# # reference lines input parameter
# refLinesParam <- list(
# 	list(
# 		time = -7,
# 		label = "Screening 1",
# 		color = "purple"
# 	),
# 	list(
# 		time = -7,
# 		label = "Screening 2",
# 		color = "purple"
# 	),
# 	list(
# 		time = 1,
# 		label = "Baseline",
# 		color = "darkblue"
# 	)
# )
# 
# # create report
# pathReport <- "subjectProfile_SDTM_referenceLines_custom.pdf"
# createSubjectProfileReport(
# 	listPlots = list(
# 		dmPlots,
# 		mhPlots,
# 		cmPlots,
# 		exPlots,
# 		aePlots,
# 		lbLinePlots
# 	),
# 	refLines = refLinesParam,
# 	outputFile = pathReport
# )
# 

## ----createReport-referenceLines-data, eval = FALSE---------------------------
# 
# # create report
# pathReport <- "subjectProfile_SDTM_referenceLines_subjectVisit.pdf"
# 
# # only retain screening, baseline and planned visits
# dataSV <- subset(dataSDTM$SV, grepl("SCREENING|WEEK|BASELINE", VISIT))
# 
# createSubjectProfileReport(
# 	listPlots = list(
# 		dmPlots,
# 		mhPlots,
# 		cmPlots,
# 		exPlots,
# 		aePlots,
# 		lbLinePlots
# 	),
# 	# reference line(s)
# 	refLinesData = dataSV,
# 	refLinesTimeVar = "VISITDY",
# 	refLinesLabelVar = "VISIT",
# 	outputFile = pathReport
# )
# 

## ----createReport-bookmarks, eval = FALSE-------------------------------------
# 
# # create report
# pathReport <- "subjectProfile_SDTM_bookmarks.pdf"
# 
# dataDM <- dataSDTM$DM
# # sort arm categories
# dataDM$ARM <- factor(dataDM$ARM,
# 	levels = c("Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"))
# 
# createSubjectProfileReport(
# 	listPlots = list(
# 		dmPlots,
# 		mhPlots,
# 		cmPlots,
# 		exPlots,
# 		aePlots,
# 		lbLinePlots
# 	),
# 	subset = c("01-718-1427", "01-704-1445", "01-701-1211"),
# 	# bookmark(s)
# 	bookmarkData = dataDM,
# 	bookmarkVar = c("SEX", "ARM"),
# 	# sort subjects in the report based on:
# 	subjectSortData = dataDM,
# 	subjectSortVar = "ARM",
# 	outputFile = pathReport
# )
# 

## ----createReport-example-----------------------------------------------------

	# create the list of visualizations
	# The list is named in order that the names are used
	# to reference the module for the alignment parameters
	listPlots <- list(AE = aePlots, LB = lbLinePlots)
	subsetPatients <- c(subjectAE, subjectLB)


## ----createReport-timeAlign, out.width = "100%", out.height = "700px", eval = FALSE----
# 
# 	pathReport <- "subjectProfile_timeAlign-all_timeAlignPerSubject-none.pdf"
# 	createSubjectProfileReport(
# 		listPlots = listPlots,
# 		outputFile = pathReport,
# 		subset = subsetPatients
# 	)
# 	

## ----createReport-timeAlign-domain, out.width = "100%", out.height = "700px", eval = FALSE----
# 
# 	pathReport <- "subjectProfile_timeAlign-AE_timeAlignPerSubject-none.pdf"
# 	createSubjectProfileReport(
# 		listPlots = listPlots,
# 		outputFile = pathReport,
# 		subset = subsetPatients,
# 		timeAlign = "AE"
# 	)
# 	

## ----createReport-timeAlign-none, out.width = "100%", out.height = "700px", eval = FALSE----
# 
# 	pathReport <- "subjectProfile_timeAlign-none_timeAlignPerSubject-none.pdf"
# 	createSubjectProfileReport(
# 		listPlots = listPlots,
# 		outputFile = pathReport,
# 		subset = subsetPatients,
# 		timeAlign = "none"
# 	)
# 	

## ----createReport-timeAlign-perSubject, out.width = "100%", out.height = "700px", eval = FALSE----
# 
# 	pathReport <- "subjectProfile_timeAlign-all_timeAlignPerSubject-all.pdf"
# 	createSubjectProfileReport(
# 		listPlots = listPlots,
# 		outputFile = pathReport,
# 		subset = subsetPatients,
# 		timeAlignPerSubject = "all"
# 	)
# 	

## ----includeSessionInfo, echo = FALSE-----------------------------------------

	pander(sessionInfo())


