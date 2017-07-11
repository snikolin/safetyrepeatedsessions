# safetyrepeatedsessions

The contents in this folder relate to the manuscript: "Safety of repeated sessions of transcranial direct current stimulation: a systematic review"

This review sought to examine whether increased exposure to tDCS, measured using cumulative charge over the study duration, resulted in a greater likelihood of adverse events. Analysis was conducted at three levels according to the detail of adverse event reporting in each study; this includes session incidence data, participant incidence data, and study incidence data.

Data and scripts used for statistical analyses and figure generation have been made available within the main folder. The script provided, titled 'tDCS.rep.safety', can be run using open source software, R. This script relies on a document containing all adverse events, titled 'AE master sheet' (excel document).  

The folder labelled 'figure' contains all scripts and data used to generate figures and statistics for the manuscript titled above.

To run the 'tDCS.rep.safety' script you must first install R software and ensure that the 'AE master sheet' is saved on your computer's directory. The script will produce figures and can be used to create a variable detailing the associated statistics results. This variable (referred to as 'result' from now on) can list all outputs at once, or can be called on to detail specifc analyses using the commands: result$session, result$incidence, result$forest, and result$study. 
 
 Examples
 - Conducting a participant incidence analysis for the adverse event, 'headache', one would enter the following into the R console:

filename <- "C:/yourcomputersfilepath/AE master sheet.xlsx"

AE.type <- "Headache"

level <- 2

result <- tDCS.rep.safety(filename, AE.type, level)

 
 - Note that multiple levels, and AE types, can be analysed at once. For example, examining paraesthesia can be achieved by searching for the adverse events of 'itching','tingling', and 'burning'. This can be done for both participant and session incidence rates at the same time.

filename <- "H:/yourcomputersfilepath/AE master sheet.xlsx"

AE.type <- c("Itching","Tingling","Burning")

level <- c(1,2)

result <- tDCS.rep.safety(filename, AE.type, level)

 
To view only session incidence results: result$session
  
 Contributing
 - If you'd like to contribute your own data to this repository email stevan.nikolin@unsw.edu.au with the following information: your study citation details, sample size, current intensity, electrode size (smallest if different), electrode montage, number of sessions, session duration, and a detailed list of adverse events.
 
 License
 - This data is free to use for your own purposes 
 - No Rights Reserved license (CC-0)
