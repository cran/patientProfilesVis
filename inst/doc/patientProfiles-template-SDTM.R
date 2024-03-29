## ----setUpTemplateForYourStudy, eval = FALSE----------------------------------
#  
#  # get template from the package
#  pathTemplate <- system.file(
#  	"doc", "patientProfiles-template-SDTM.Rmd",
#  	package = "patientProfilesVis"
#  )
#  file.copy(from = pathTemplate, to = ".")
#  # Note: your current working directory can be checked with: getwd()

## ----patientProfiles-optionsChunks, echo = FALSE, cache = FALSE---------------

	library(knitr)

	knitr::opts_chunk$set(
		error = FALSE, # stop document execution if error (not the default)
		fig.align = "center"
	)
	

## ----patientProfiles-loadPackages---------------------------------------------

	library(patientProfilesVis)


## ----patientProfiles-loadData-------------------------------------------------
	
	library(clinUtils)
	
	# For this vignette, an example STDM dataset is used
	# (subset of the CDISC Pilot 01 study) 
	data(dataSDTMCDISCP01, package = "clinUtils")
	dataAll <- dataSDTMCDISCP01
	labelVars <- attr(dataSDTMCDISCP01, "labelVars")
	
	# If SDTM datasets should be imported from the external folder,
	# the following code could be used:
#	pathFiles <- list.files(path = "/path/to/data", pattern = "*.sas7bdat$", full.names = TRUE)
#	dataAll <- loadDataADaMSDTM(files = pathFiles)
#	labelVars <- attr(dataAll, "labelVars")


## ----patientProfiles-initialization-------------------------------------------

	patientProfilesPlots <- list()


## ----patientProfiles-demographics---------------------------------------------

	dmPlots <- subjectProfileTextPlot(
		data = dataAll$DM,
		paramValueVar = c(
			"SEX", "RACE", "ETHNIC",
			"COUNTRY", "ARM",
			"AGE", "RFSTDTC"
		),
		# optional:
		labelVars = labelVars
	)
	patientProfilesPlots <- c(patientProfilesPlots, list(DM = dmPlots))


## ----patientProfiles-medicalHistory-------------------------------------------
	
	mhPlots <- subjectProfileTextPlot(
		data = dataAll$MH,
		paramValueVar = c("MHDECOD", "MHTERM", "MHSTDTC"), #"MHENDTC" if available
		# optional:
		paramGroupVar = c("MHSTDTC", "MHDY"),
		title = "Medical history (Start - End)",
		labelVars = labelVars,
		table = TRUE
	)
	patientProfilesPlots <- c(patientProfilesPlots, list(MH = mhPlots))


## ----patientProfiles-concomitantMedications-----------------------------------

	cmPlots <- subjectProfileIntervalPlot(
		data = dataAll$CM,
		paramVar = c(
			"CMTRT", 
			"CMDOSE", "CMDOSU", "CMROUTE", 
			"CMDOSFRQ"
		),
		timeStartVar = "CMSTDY",
		timeEndVar = "CMENDY",
		# optional:
		paramGroupVar = "CMCLAS", # or CMINDC
		colorVar = "CMCLAS", # or CMINDC
		labelVars = labelVars,
		title = "Concomitant medications",
		# To zoom in axis scale in study time frame
		# (to avoid scale is focused on negative pre-study time frame for CM)
		timeTrans = getTimeTrans(type = "asinh-neg"), 
		alpha = 0.8,
		timeAlign = FALSE
	)
	
	patientProfilesPlots <- c(patientProfilesPlots, list(CM = cmPlots))


## ----patientProfiles-treatmentExposure----------------------------------------

	exPlots <- subjectProfileIntervalPlot(
		data = dataAll$EX,
		paramVar = c(
			"EXTRT", "EXDOSE", "EXDOSU", 
			"EXDOSFRM", "EXDOSFRQ", "EXROUTE"
		), # "EXTPT" if available
		timeStartVar = "EXSTDY",
		timeEndVar = "EXENDY",
		# optional:
		colorVar = "EXDOSFRM",
		title = "Treatment exposure",
		alpha = 0.8,
		labelVars = labelVars
	)
	
	patientProfilesPlots <- c(patientProfilesPlots, list(EX = exPlots))


## ----patientProfiles-adverseEvents--------------------------------------------

	dataAE <- dataAll$AE$AESEV
	# to adapt for specific dataset
	dataAE$AESEV <- factor(dataAll$AE$AESEV, levels = c("MILD", "MODERATE", "SEVERE"))
	
	aePlots <- subjectProfileIntervalPlot(
		data = dataAll$AE,
		paramVar = "AEDECOD", # AETERM, depending on coding
		timeStartVar = "AESTDY",
		timeEndVar = "AEENDY",
		# optional:
		paramGroupVar = "AESOC",
		colorVar = "AESEV", 
		title = "Adverse events",
		timeAlign = FALSE,
		alpha = 0.8,
		labelVars = labelVars
	)
	
	patientProfilesPlots <- c(patientProfilesPlots, list(AE = aePlots))
	

## ----patientProfiles-laboratory-----------------------------------------------

	dataLB <- dataAll$LB
	# to adapt for dataset
	dataLB$LBNRIND <- factor(dataLB$LBNRIND, levels = c("LOW", "NORMAL", "HIGH", "ABNORMAL"))

	# specify custom color and shape palette
	colorPaletteLB <- clinUtils::getPaletteCDISC(x = dataLB$LBNRIND, var = "NRIND", type = "color")
	shapePaletteLB <- clinUtils::getPaletteCDISC(x = dataLB$LBNRIND, var = "NRIND", type = "shape")
	
	lbPlots <- subjectProfileLinePlot(
		data = dataLB,
		paramValueVar = "LBSTRESN",
		paramNameVar = "LBTEST",
		timeVar = "LBDY",
		# optional
		paramValueRangeVar = c("LBSTNRLO", "LBSTNRHI"),
		paramGroupVar = "LBCAT", # "LBSCAT" if available
		colorVar = "LBNRIND", colorPalette = colorPaletteLB,
		shapeVar = "LBNRIND", shapePalette = shapePaletteLB,
		shapeSize = 4,
		title = "Laboratory test measurements",
		alpha = 0.8,
		labelVars = labelVars
	)
	
	patientProfilesPlots <- c(patientProfilesPlots, list(LB = lbPlots))
	

## ----patientProfiles-vitalSigns-----------------------------------------------

	# If available, reference range indicator
	# could be displayed via the color/shape variable

	vsPlots <- subjectProfileLinePlot(
		data = dataAll$VS,
		paramValueVar = "VSSTRESN",
		paramNameVar = "VSTEST",
		timeVar = "VSDY",
		# optional
#		paramGroupVar = "VSCAT", # if available
		colorVar = "VSPOS", # if available
		shapeSize = 2,
		title = "Vital signs",
		alpha = 0.8,
		labelVars = labelVars
	)
	patientProfilesPlots <- c(patientProfilesPlots, list(VS = vsPlots))
	

## ----patientProfiles-ECG, eval = FALSE----------------------------------------
#  
#  	# If available, reference range indicator
#  	# could be displayed via the color/shape variable
#  
#  	egPlots <- subjectProfileLinePlot(
#  		data = dataAll$EG,
#  		paramValueVar = "EGSTRESN",
#  		paramNameVar = "EGTEST",
#  		timeVar = "EGDY",
#  		# optional
#  		title = "Electrocardiogram",
#  		alpha = 0.8,
#  		labelVars = labelVars
#  	)
#  	patientProfilesPlots <- c(patientProfilesPlots, list(EG = egPlots))
#  	

## ----patientProfiles-createSubjectProfileReport, message = FALSE, warning = TRUE, eval = FALSE----
#  
#  	pathsPatientProfiles <- createSubjectProfileReport(
#  			
#  		listPlots = patientProfilesPlots,
#  		
#  		# optional
#  		reportPerSubject = TRUE,
#  		verbose = TRUE,
#  		outputFile = './patientProfiles/subjectProfile.pdf',
#  		timeAlign = "all", timeAlignPerSubject = "all",
#  		exportBatchSize = 5,
#  		
#  		# export subjects with highest adverse events severity
#  		subjectSortData = dataAll$AE,
#  		subjectSortVar = "AESEV",
#  		subjectSortDecreasing = TRUE,
#  		
#  #		# only patients with severe adverse events
#  #		subjectSubsetData = dataAll$AE,
#  #		subsetVar = "AETOXGR",
#  #		subsetValue = "SEVERE"
#  	)
#  

## ----sessionInformation, echo = FALSE-----------------------------------------

	library(pander)
	pander(sessionInfo())


