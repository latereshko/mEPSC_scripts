//******************************************************************************
// DetectMinis6.4 - Modified from DetectMinis6.3b --modified to comply with igor 4 syntax. Also replaced 
//	calls to SNUtilities 2 with calls to SNUtilities 2.3, eliminated calls to concatenate waves and strings as lists
//
//******************************************************************************

#pragma rtGlobals = 1	// Use run-time lookup of globals.

//*******************
//* Includes
//*******************
#Include "wormdisk:Users:Lauren:Desktop:IGOR_Procs:MiniAnalysis:SNUtilities2_5"				// put an alias to bStim_2, igor will search for the file via 'User procedures'	

//********************************************************************************
// InitDetect - initialization routine for detect minis program, some global variables are initialized and the initdetect
// 			   control panel is put up.
//********************************************************************************
Macro InitDetect ()
	PauseUpdate							//Dont update graphs and tables while in this macro
	Silent 1									// Don't display commands in this macro on the command line
//*******************
//* Global Variables
//*******************
Variable /G gStart = 200						//Start of section of wave list to detect minis on. Units are ms.
Variable /G gEnd = 99999 					//  End of section of wave list to detect minis on.
Variable /G gWavePts = 10000				// Points per wave of data to be detected									
Variable /G gBox = 5						//Num of points in sliding ave for peak detection.
Variable /G gBoxHW = 5						//Num of points in sliding ave for HW and even end calculations
Variable /G gBoxRise = 3						//Num of points in sliding ave for rise time calc.
Variable /G gFiltWid = 3					//  ms of width input to smooth()

Variable /G gMinDur = 2					//Mini Duration criterion, max 
Variable  /G gMaxDur = 40					// and min length in ms.

Variable /G gBWid = 50						// length of "baseline period" i.e. portion of wave used to calc threshold.
Variable /G gBpreWid = 5					// smaller baseline used for mini measurments, once detected
Variable /G gBasefraction= 0.1
Variable /G gMaxRise = 3, gMinHW=1		// Slowest allowable rise time, minimum allowable half width
Variable /G gMinUpdate = 50				
Variable /G gMaxUpdate = 500				//How often to update baseline. if crossing not found not found (max)
											//min - if threshold is crossed update baseline only if gminupdate
											//time has passed since last update.

Variable /G gThresh=3							//Threshold above baseline for mini detect in absolute (mv or pa )  or relative (sd above mean) 
Variable /G gMinThresh = 5					// Absolute threshold (used in conjunction with relative)
Variable /G  gBThresh=2						// Relative Threshold for end of mini...
Variable /G  gPre = 10							// For extracting the mini from the wave
Variable /G gPost= 40
Variable /G gPkAvg = 0.5						// Size of window to define peak. (ms)

Variable /G gXstart								// time axis start and end for main display
Variable /G gXend
Variable /G gYstart								// voltage or current axis values for main display
Variable /G gYend

Variable /G gNreject							// keep track of the number of rejected minis

String /G gETBase = "*"


// See if a data file is selected, if not prompt for it
//abb, removed this it is already in 
	analysisParams()				// this is part of measure waves and helps avoid errors
	PanDetectParams()
	cvtETpanel()
	resizepanel()
	PanSelectWaves ()
EndMacro


//********************************************************************************//
// 
//********************************************************************************
Macro DetectMinis (bInter,bWriteMini,bDisplayExt, bDisplayDet,bWriteSrc, bmeasure, alignMode, pol, riseType, doFilter)
variable bInter, bWriteMini, bDisplayExt, bDisplayDet, bWriteSrc, bmeasure, alignMode, pol 
String riseType, doFilter

	String/G wlist
	Variable /G gStart, gEnd, gBox, gBoxHW, gBoxRise, gFiltWid, gBWid, gBPreWid, gMinDur, gMaxDur, gMaxRise
	Variable /G gThresh, gMinThresh, gPre, gPost, gBThresh, gPkAvg, gMinUpdate, gMaxUpdate
	Variable wnum, preDisplay = 20, postDisplay = 100//50	praveen
	Variable  filtPts, endWave, bfound, numNANs
	Variable nfound, bskipfilt=0, nFiles, bDiagnose
	Variable nxtChk, endLook, nPkDur, bstrt= gStart, srchstrt = gStart, nxtBaseStrt, bWholeW
	String w, wOld, fltrType, threshMode

	Silent 1;PauseUpdate
// Wave containing current thresholds, and other data used for detection:
// This array is loaded in UpdateThresh ()
//	wTInfo [0] = mean baseline,
// 	wTinfo [1] = baseline sdev
//  	wTInfo[2] =  peak threshold
//  	wTInfo[3] = return to baseline thresh
Make /O /N=4 wTInfo
doWindow /B PanDetectParams								
print"*"
print "****************************RESTART***********************************"
print"*"
      nFiles = NumItemsInList (wlist, ";")
      print "nFiles=",nFiles
// If we are outputting files, get the path 
	if (bWriteMini%|bWriteSrc) 
		doAlert 1, "Do you want to use the old path:"
		if (v_flag==2)						// no				
			NewPath /C /O /M="Select or create a folder to hold results" outpath
		else									// yes, use an old path
			pathinfo outpath
			if (V_flag==0)					// if path doesnt exist, create it
				NewPath /C /O /M="path doesnt exist, please select or create a folder to hold results" outpath
			endif
		endif			
	endif
	InitWaves (2000, 0)						//Initialize the waves that hold info about each psp
												// Note: !!!: Current setting allows maximum of 2000 minis
// find out which type of threshold to use, options are absolute, noise relative, greater of the two, or lesser of the two												
	controlInfo /W=pandetectparams popThreshMode
	threshMode = S_Value												
// find out whether or not to print diagnostics												
	controlInfo /W=pandetectparams chkDiagnostics
	bDiagnose = V_Value
	wnum =	 0									//index into wave list
	gNreject = 0								// set the number of rejected minis to 0
	Do											// loop through each wave in the list
		w = GetStrFromList(wlist,wnum,";")	// Get the name of the first data wave from wave list. 
		if (exists(w) != 1)
			print "couldn't get wave ",wnum," from the list:",wlist		// No waves, for some reason
		else
			print "starting on wave#",wnum,"=",w
		endif
// Check to see if the filtered wave is already loaded, prompt the user to make sure it is the right one
		if (cmpstr(doFilter,"None")== 0)			// if the user requested no filtering
			bskipfilt = 1
		endif
		if ((exists("wf")==1)%&(nFiles==1))
			doAlert 1, "WF exists, should I skip filtering?"
			if (V_Flag==1)							// Yes, skip
				bskipfilt = 1
			endif
		endif
//*** ***********
//* Set up filter width
//***************
		if (!bskipfilt)	
// Filter out any NANs
			numNANs = RemoveNANs($w)			// The wavemetrics procedure
			if (bDiagnose)
				print "got rid of ",numNANs," NAN's in wave",w
			endif
			filtPts = trunc (gFiltWid/(deltax($w))) //Calc number of points wide to filter
			if (mod (filtPts,2) == 0)				// could do this error checking in panel, but would need to know scaling of wave
				filtPts += 1						//  Make it at least one wide.
			endif
			if (filtPts >25)							//Set max width as well.
				filtPts = 25
			endif
			if (filtPts <7)
				filtPts = 7
			endif									//well, actually set min to seven
	
//*** ***********
//* Make a copy of the wave and filter it if specified by the panel.
//* 
//* Must first rescale if necessary so that x scaling is in msec units. 
//* use kludgy assumption that waves will be < 1000 and >1 sec long
//***************
			endWave = pnt2X($w,numpnts($w)-1)
			if (endWave<=1000)
				endWave = endWave*1000							
				setScale /I x, leftX($w)*1000, endWave,"ms",$w
				if (bDiagnose)
					print "converting wave to msec format"
				endif
			endif
			ControlInfo /W=PanDetectParams  chkWholeWave
			bWholeW=V_value
			if ((bWholeW==0)%&(gEnd < endWave))			// if user hasnt specified whole wave and end value is
				endWave = gEnd									// less than end of wave, use entered end value
			endif
	     		duplicate /D /O $w, wf					
	      		print "filtering..."
			smooth /S=4 filtPts,wf					// use S-G 4th order, n=filtPts
			Note wf "filter: SG 4th order, #pnts="+num2str(filtPts)		// !!! need to make sure miscXOP is loaded
// add filter parameter info to wave note
		endif
	
		if (bDisplayDet)													// if we are displaying the detection process
			if (exists("wCurrBase")+exists("wCurrThresh")==0)
				Make /D /O /N=2 wCurrBase, wCurrThresh   				// these are for displaying the baseline and threshold values stored in wTInfo
			endif
			SetScale /I x, leftX(wf), endWave, wCurrBase, wCurrThresh
			wCurrBase := wTInfo[0]
			wCurrThresh := wTInfo[2]
// create the graph which will be used to display individual minis, if it exists already use the same X resolution.
			if (cmpstr(WinList ("miniGraph", ";", ""),"") == 0)			// if a graph named miniGraph doesnt exist
				display /W=(6,42,800,300) $w
				DoWindow/C miniGraph	
				appendToGraph wf, wCurrBase, wCurrThresh
				ModifyGraph rgb (wCurrBase) = (0,0,65535)				// blue baseline
				ModifyGraph rgb (wf) = (0,0,65535)						// blue smoothed wave
				ModifyGraph rgb (wCurrThresh) = (0,65535, 0)			// green baseline
// !!!need to check that it is w that is displayed, and not something else
			else	
				DoWindow /F miniGraph
				if (wnum > 0) 
					ReplaceWave trace=$wOld $w
				endif
			endif
//abb to get a better y scale, noise spikes causes max and min to be much larger than mini amp
			wavestats /Q/R=(gStart, gEnd) $w
//			wavestats /Q/R=(gStart, gStart+preDisplay+postDisplay) $w
			SetAxis left V_min, V_max	
			SetAxis bottom gStart,  gStart+preDisplay+postDisplay
			ModifyGraph live = 1
		endif																	// if displaying detection process
// if decimating and splining to remove sloping baseline do it here.
// first, if desired, write the source file and filtered version to disk
		if (bWriteSrc) 
			Save /O/P=outpath $w as w
			Save /O/P=outpath wf as w+".f"
		endif

		wnum += 1												//    Increment index into wave list
	
		nFound = doWave(nFound,$w,wf, WTInfo, bInter,bWriteMini,bDisplayExt, bDisplayDet, bmeasure, alignMode, bDiagnose, pol, riseType, threshMode, doFilter)
		print "found",nFound," minis"
		print "rejected",gNreject," minis"
		if (nfound == (numpnts ($"M_endX")-1))		// make more room for minis if we need it
			print "reinitializing, nfound=",nfound,"numpnts mstrtx=",numpnts ($"M_endX")
			if (nfound > 9999)
				abort "Maximum # of minis (>9999) found"
			endif
			InitWaves (2*numpnts($"M_endX"),1)
		endif
	       wOld = w
	while (wnum < nFiles)													// loop until we've runout of waves	
//*******************
//* Finally, display the table and delete the wave windows.
//*******************
	InitWaves (nfound+1, 1)		//Set the table length to the actual number of minis
	deletePoints 0,1, $"M_file",$"M_strtX", $"M_pkX", $"M_pkY", $"M_Base",$"M_rise", $"M_endX", $"M_decay", $"M_HW", $"M_HR", $"M_area"	
	Edit $"M_file",$"M_strtX", $"M_pkX", $"M_pkY", $"M_Base",$"M_rise", $"M_endX", $"M_decay", $"M_HW", $"M_HR", $"M_area" as "Detected Mini's"
	DoWindow /K wavegraph	//kill window
	DoWindow /K miniGraph	//kill window
	wavestats $"M_pkY"
	wavestats $"M_rise"
	if (bWriteMini)
//		Save /O/P=outpath $"M_file" as w+"_file"					// NOTE: we are overwriting, if exists
//		Save /O/P=outpath $"M_strtX" as w+"_strtX"				
//		Save /O/P=outpath $"M_pkX" as w+"_pkX"				
//		Save /O/P=outpath $"M_pkY" as w+"_pkY"				
//		Save /O/P=outpath $"M_Base" as w+"_Base"				
//		Save /O/P=outpath $"M_rise" as w+"_rise"				
//		Save /O/P=outpath $"M_endX" as w+"_endX"				
//		Save /O/P=outpath $"M_decay" as w+"_decay"				
//		Save /O/P=outpath $"M_HW" as w+"_HW"				
//		Save /O/P=outpath $"M_HR" as w+"_HR"				
//		Save /O/P=outpath $"M_area" as w+"_area"		
															// praveen: added .ibw extension for windows
		Save /O/P=outpath $"M_file" as w+"_file.ibw"					// NOTE: we are overwriting, if exists
		Save /O/P=outpath $"M_strtX" as w+"_strtX.ibw"				
		Save /O/P=outpath $"M_pkX" as w+"_pkX.ibw"				
		Save /O/P=outpath $"M_pkY" as w+"_pkY.ibw"				
		Save /O/P=outpath $"M_Base" as w+"_Base.ibw"				
		Save /O/P=outpath $"M_rise" as w+"_rise.ibw"				
		Save /O/P=outpath $"M_endX" as w+"_endX.ibw"				
		Save /O/P=outpath $"M_decay" as w+"_decay.ibw"				
		Save /O/P=outpath $"M_HW" as w+"_HW.ibw"				
		Save /O/P=outpath $"M_HR" as w+"_HR.ibw"				
		Save /O/P=outpath $"M_area" as w+"_area.ibw"				
	endif
	
End							

//********************************************************************************
//* 	This is the main function which loops through a wave detecting etc.
//********************************************************************************
Function doWave (nFound,w, wf, WTInfo, bInter,bWriteMini,bDisplayExt, bDisplayDet, bmeasure, alignMode, bDiagnose, pol, riseType, threshMode,  doFilter)
variable nFound
wave w, wf, wTInfo
variable bInter, bWriteMini, bDisplayExt, bDisplayDet, bmeasure, alignMode, bDiagnose, pol
String riseType, threshMode, doFilter

	Variable /G gStart, gEnd, gBox, gBoxHW, gBoxRise, gBWid, gBPreWid, gMinDur, gMaxDur, gMaxRise
	Variable /G gThresh, gMinThresh, gPre, gPost, gBThresh, gPkAvg, gMinUpdate, gMaxUpdate, gNreject
	Variable tErr, preDisplay = 20, postDisplay = 100 // 50	praveen
	Variable endWave, bfound, bmakemini
	Variable iwerr, thresh
	Variable nxtChk, endLook, nPkDur, bstrt= gStart, srchstrt = gStart, nxtBaseStrt
	String miniBasStr, miniWstr, wname

	wave wend=$"M_endX"
//abb added /D
	wave/D wstrt=$"M_strtX"
	wave/D wpkx=$"M_pkX"
	wave wbase=$"M_base"
	wave wHR=$"M_HR"
	wave /T wFile = $"M_file"

	Silent 1;PauseUpdate

// make the basename for the mini files
	wname = NameofWave(w)
	if (strlen(wname) < 13)			// total name can be 18 char, we want to reserve 1 for "M" to start and 
		miniBasStr = wname			//    5 for _XXXX where XXXX= a # from 1-9999
	else
		miniBasStr = wname[0,11]
	endif
	endWave = pnt2X(wf,numpnts(wf))
// Calculate the threshold
	thresh = SetThresh (threshMode, bstrt, bstrt+gBWid, bDiagnose, pol, w, wTInfo)
	if (bDiagnose)
		print "initial threshold=",wTInfo[2]
	endif

// First find the next negative crossing (assume these are more frequent, if we run out of room, set a flag which says so: bNoMorePosPks
// Then, look for a positive crossing which occurs sooner. Evaluate the sooner of the two. Update the threshold
// Next, find the point at which we next cross back over the same level in order to check the min and max duration
// If o.k., move bkward to find the start of the mini, then move forward from detection point to find the peak, then
// move forward to find the end of the mini. Then copy the mini + pre and post baseline to another wave, if writing write it,
// measure rise time, area and 1/2 width.
	do													// while (srchstrt<endWave)
		bfound = 0
		bmakemini = 0
//abb
		Variable bStop = 0
// First update the thresholds
//*******************
//* 
//*******************		
// Update the baseline period to be just prior to the next searching period if there's room
// First check that we have advanced enough to make updating the baseline worthwhile. Update if this is either
// 1) the first search, or if 2) the last update was > gMinUpdate ago. We make sure that we update at least every
// gMaxupdate by only incrementing srchstrt by this amount at most.
//
		if ((srchstrt-bstrt)>=gMinUpdate)
			bstrt = srchstrt
			tErr = updateThresh (bstrt, bstrt+gBWid, wTInfo[0], wf, wTInfo)
			doUpdate
		endif
	     	endLook=srchstrt+gMaxUpdate
	     	nxtchk = endLook 
//******************* 
//*  
//*******************	
		if (bDiagnose)	
			print "looking for level from: ", srchstrt," to ", endLook, "base=",wTInfo[0],"thresh=",wTInfo[2], "pol=",pol
		endif
		FindLevel /B=(gBox) /Q /R=(srchstrt,endLook) wf, wTInfo[2]
		if (V_Flag==0)											//If a crossing was found ...
			nxtChk = V_LevelX									// Save the x value of the crossing
			bfound = 1
		endif
//*******************
//* 
//*******************
// Now if we have the next candidate peak, check its duration to see if its a valid peak, otherwise keep looping. 
//
//	First, display it (may want to move this once its working)
//
		if (bDiagnose)
			print "srchstrt=",srchstrt," next candidate, nxtChk=",nxtChk, "endlook=", endlook
		endif
		if (bfound)
			srchstrt = nxtchk+gMinDur							// assume we will move forward the minimum allowable peak width
			if (bDisplayDet)
				DoWindow/F miniGraph	
				SetAxis bottom nxtChk-preDisplay, nxtChk+postDisplay
//abb / in front of A
//				Cursor A, w, nxtChk
				Cursor A, wf, nxtChk
				DoUpdate						  				// erase any old A or B cursors
			endif
			nFound += 1
//
//	In order to check if this is a valid peak, we will update the baseline so that it ends at maxRise ms. before
//	the threshold crossing (maxRise should be set > max. expected rise time of event) We then check that the event
//	meets the min and max duration criteria.
//
			nxtBaseStrt = max(nxtChk-gmaxRise-gBpreWid,  leftX(wf))
			tErr = updateThresh (nxtBaseStrt,nxtBaseStrt+gBpreWid, wTInfo[0], wf, wTInfo)
			if  (bdisplayDet)
				doUpdate
			endif
			wbase[nFound] = wTInfo[0]
			nPkDur = measureMini(nxtChk, nPkDur, nFound, bDiagnose, pol, w, wTInfo, riseType)
// ERROR CODES ARE:
//	-1:			Rise too long
//	-20--29:	peak too small, or rise too long, refound pk, HW too short or long
//	-3:			Return to baseline not found
//	-wid			mini  too long or too short
//				
			if (nPkDur > 0)										// nPkDur returns either the width, or a neg. error code
				if (binter)											// If interactive mode
//abb so we can cancel the current wave anal, go onto next
//					doAlert 1, "Accept?"
//abb so we can see how many minis we have (on the mac the command window does not update)
String cmdStr = "Accept? (N=" + num2str(nFound) + ")"
					doAlert 2, cmdStr
//					doAlert 2, "Accept?"
					if (V_Flag ==1)								// If wave accepted...
						bMakeMini = 1
					elseif (V_Flag==2)											// Not accepting
						srchstrt = nxtChk+nPkDur
						nFound -= 1
						gNreject +=1					// keep track of number of rejects
					elseif (V_Flag==3)
						bStop = 1
//abb 3/31
						nFound -= 1
						gNreject +=1					// keep track of number of rejects
					endif											// If accepting
				else													// Not prompting for acceptance
					bmakeMini = 1
				endif												// if prompting interactively
			else														// pk not long enough or too long
				nFound -= 1
 //				srchstrt = nxtchk+gMinDur	
				srchstrt = nxtchk+gMaxDur		//praveen: there is an event which is too long or too short. advancing by gMinDur
													// gives wrong baseline, threshold. so advance to max. event duration. 01/04/2007 
				if (bDiagnose)
//					print "Rejecting, npkdur=", npkdur, "srchstrt=(nxtchk+gmindur)",nxtChk,"+",gMinDur
					print "Rejecting, npkdur=", npkdur, "nxtchk+gMaxDur",nxtchk+gMaxDur		//praveen
				endif
			endif													// if  acceptable duration
			if (bmakeMini == 1)
				wfile[nFound] =  wname
				makeMiniWave(bWriteMini, bDisplayExt, alignMode, nFound, deltaX(w), bDiagnose, miniBasStr, w, wHR, wTInfo, wpkX, wstrt)
				srchstrt = wend[nFound]
				if (bDiagnose)
					print "advancing to end of mini: ", wend[nFound]
				endif
			endif
		else
			srchstrt = nxtchk+gMinDur	
		endif														// if we found something
	while ( (srchstrt < endWave) %& (bStop==0) )
	
	return nFound
end 

//********************************************************************************
// Sets the baseline and threshold values stored in the wave wTInfo (see top of DetectMinis)
// NOTE: in the current incarnation, this routine is called only once per wave. It returns the threshold which is the
// greater of the relative threshold and the absolute threshold set by the user.
//********************************************************************************
Function SetThresh (threshMode, bstrt, bend, bDiagnose, pol, w, wTInfo)
string threshMode
variable  bstrt, bend, bDiagnose, pol
wave w
wave /D  wTInfo

	variable /G gThresh, gMinThresh, gBThresh, gEnd
	variable endWave, ipt, threshval
	variable err, ithresh=1
	
	endWave = pnt2X(w,numpnts(w))

	if (endWave < bend)	
		err = -1
		Return err	
	else
		WaveStats /Q /R=(bstrt, bend) w					// NOTE we are using the raw data wave for calculating						
	endif													//   baseline mean and variance.
	wTInfo [0] = V_avg
	wTInfo [1] = V_sdev
	if (cmpstr(threshMode,"Absolute")==0) 
		threshval = gMinThresh
	endif
	if (cmpstr(threshMode,"Relative")==0) 
		threshval = wTInfo [1]*gThresh
	endif
	if (cmpstr(threshMode,"greater")==0) 
		threshval = max(wTInfo [1]*gThresh, gMinThresh)		// use the greater of the relative and absolute thresholds
	endif
	if (cmpstr(threshMode,"greater")==0) 
		threshval = min(wTInfo [1]*gThresh, gMinThresh)		// use the lesser of the relative and absolute thresholds
	endif
	wTInfo [2]= wTInfo [0]+pol*threshval					// Update the regular peak (findLevel) threshold
	wTInfo [3] = wTInfo [0]+pol*wTInfo [1]*gBThresh		// and the return to baseline threshold
	if (bDiagnose)
		print "new baseline from ", bstrt, "to ", bend,"= ",  wTInfo[0], "+/-",  wTInfo[1], "pol=",pol
	endif
	Return  threshval
End
//********************************************************************************
// Updates the baseline and threshold values stored in the wave wTInfo (see top of DetectMinis)
//********************************************************************************
Function updateThresh (bstrt, bend, oldbase, wf, wTInfo)
variable  bstrt, bend, oldbase
wave /D wf,  wTInfo

	wTInfo [0] = Mean(wf, bstrt, bend)
	wTInfo [2,3]+= (wTInfo [0]-oldbase)					

End
//********************************************************************************
//
//********************************************************************************
Function measureMini(nxtChk, nPkDur, nFound, bDiagnose, pol, w, wTInfo, riseType)
Variable nxtChk, nPkDur, nFound, bDiagnose, pol
wave /D w, wTInfo
String riseType								

	Variable /G gStart, gEnd, gBox, gBoxHW, gBoxRise, gBWid, gMinDur, gMaxDur, gMaxRise
	Variable /G gThresh, gPre, gPost, gBThresh, gPkAvg
	variable iErr, wid, bLooseBase
	variable bx = gBox	// could pass this in here instead and error check it
	
//abb added /d
	wave/D wstrt=$"M_strtX"
	wave/D wpkx=$"M_pkX"
	wave/D wpky=$"M_pkY"
	wave/D wrise=$"M_rise"
	wave/D wend=$"M_endX"
	wave/D wdk=$"M_decay"
	wave/D whw=$"M_HW"
	wave/D wHR=$"M_HR"
	wave/D war=$"M_area"


// ERROR CODES ARE:
//	-1:			Rise too long
//	-20:			peak too small, or rise too long
//	-3:			Return to baseline not found
//	-wid			mini  too long or too short
//				

	controlInfo /W=pandetectparams chkLooseBase
	bLooseBase = V_Value

	iErr = findMiniPeak (nxtChk, nFound, pol, bx, bDiagnose, riseType, w, wTInfo, wstrt, wpkx, wpky, wrise, wHW, wHR)
	if (bDiagnose)
		print "mini #", nFound,"start=", wstrt[nFound], "peak=", wpky[nFound], " at ", wpkx[nFound], "rise=", wrise[nFound]
		print "baseline=",wTInfo[0], "pol=",pol
	endif
	//Cursor /F B wf wpkx[nFound], 	wpky[nFound]	//praveen: cursor at the peak so that the user knows which peak is being considered 
	Cursor B wf,  wpkx[nFound] //Modif by Sandrine
	if (iErr< 0)
		if (bDiagnose)
			print "ierr from findminiPeak=",ierr
		endif
		Return -20+ierr
	endif
// Having found the start, update the baseline
	iErr = updateThresh (wstrt[nFound]-gBwid,wstrt[nFound], wTInfo[0], wf, wTInfo)

//
//	Now find the end of the mini
//
 	iErr = findMiniEnd (nFound, gBoxHW, pol, bDiagnose, bLooseBase, w, wTInfo,  wend, wpkx, wpky, wdk)
	if (iErr< 0)
		if (bDiagnose)
			print "ierr from findminiEnd=",ierr
		endif
		Return -3
	else
		wid = wend[nFound]-wstrt[nFound]
	endif
	if ((wid <= gMaxDur) %& (wid >= gMinDur))
		iErr = calcMiniArea (nFound, bx, w, wTInfo, wstrt, wend, war)
		Return wid
	else
		Return -wid
	endif

End	
//********************************************************************************
//
//********************************************************************************
Function findMiniPeak (nxtChk, nFound, pol, bx, bDiagnose, riseType, w, wTInfo, wstrt, wpkx, wpky, wrise, wHW, wHR)			
Variable nxtChk													// time of detected threshold crossing
Variable nFound, pol, bx, bDiagnose
String riseType
Wave /D w, wTInfo, wstrt, wpkx, wpky, wrise, wHW, wHR

	variable strtlook, endlook
	variable  riselvl1, riselvl2, rise1, rise2, hwlvl, hwx2
	variable /G gPkAvg, gPk_Value, gMaxDur, gMaxRise, gMinHW, gBoxRise, gBoxHW
	variable sampIntvl = deltax(w)

	strtlook = nxtChk-(bx*sampIntvl)
	endlook = nxtChk+(bx*sampIntvl)+gMaxdur
	if (bDiagnose)
		print ""
		print "nxtChk=",nxtChk, "nFound=", nFound, " bx=",bx, "start=",nxtChk-gMaxRise
		print "base=",wTInfo[0],"nFound=",nFound,"wTInfo[2,3]=",wTInfo[2],",",wTInfo[3]
	endif
	wpkX[nFound] = findMaxBox2(w,gpkAvg, pol, strtlook,endlook)	
//	wpkX[nFound] = findMaxBox2(w,gpkAvg, pol, strtlook,endlook-gMaxdur+gMaxRise)		//praveen
	
	wpky[nFound] = gPk_Value -wTInfo[0]
	if (bDiagnose)
		print "peak at",wpkx[nFound], "=",wpky[nFound]
	endif
	if (abs(wpky[nFound])< abs(wTInfo[0]-wTInfo[2]))			// recheck amplitude
	if (bDiagnose)
		print "rejecting, too small"
	endif
		return -1
	endif
	if (wpkx[nFound]==wpkx[nFound-1])		// Oops, refound same peak
		print "Rejecting, refound same peak!"	//praveen: print error text
		return -2
	endif								

// Now Find the start of the mini

//abb
//print "   wpkx[nFound]=",wpkx[nFound], "gMaxRise=", gMaxRise, "wTInfo[0]=", wTInfo[0]
	FindLevel /B=(bx) /Q /R=(wpkx[nFound],wpkx[nFound]-5*gMaxRise) w, wTInfo[0]		// start at pk and look bkwards
	if (V_Flag)  //didn't find
		if (bDiagnose)
			print "   didnt find it, using",wTInfo[0]
		endif
		FindLevel /B=(bx) /Q /R=(wpkx[nFound],wpkx[nFound]-5*gMaxRise) w, wTInfo[3]	// if not found, use looser criterion
		if (V_Flag)
			if (bDiagnose)
				print "   Cant find Mini Start looked from ",wpkx[nFound], " to ",wpkx[nFound]-5*gMaxRise," for",wTInfo[0], wTInfo[3]
			endif
		else
			wstrt[nFound] = V_LevelX
			if (bDiagnose)
				print "   found looser baseline at",wstrt[nFound] 
			endif
		endif
	else
		wstrt[nFound] = V_LevelX
		if (bDiagnose)
			print "   Found start at", wstrt[nFound]
		endif
	endif



// Now calculate rise time  
	if (cmpstr(riseType, "none")==0)
		wrise[nFound] = 0
		Return 0
	endif 
	if (cmpstr(riseType, "10-90")==0)
		riselvl1 = 0.1*(wpky[nFound]) + wTInfo[0]
		riselvl2 = 0.9*(wpky[nFound]) + wTInfo[0]
	endif
// Note!!! not using any box to avoid filtering effects on rise time
	if (cmpstr(riseType, "20-80")==0)
		riselvl1 = 0.2*(wpky[nFound]) + wTInfo[0]
		riselvl2 = 0.8*(wpky[nFound]) + wTInfo[0]
	endif 
//abb it depends on platform
//	if ( bIsOnMac() )
		FindLevel /B=(gBoxRise) /T=0.5/Q/R=(wpkx[nFound], wstrt[nFound] ) w, riselvl1
//	else
//		FindLevel  /Q/R=(wpkx[nFound], wstrt[nFound] ) w, riselvl1
//	endif
	
	if (V_Flag==0)
		rise1 = V_LevelX
	else
		if (bDiagnose)
			Print "   can't find beginning rise point using riselevls: ", riselvl1, riselvl2
			print "   looked from", wpkx[nFound], "to ",wstrt[nFound], "gboxrise=",gboxrise 
		endif
		return -3
//!!! else should do some error handling
	endif
	FindLevel /B=(gBoxRise)  /Q/R=(wpkx[nFound], wstrt[nFound]) /T=0.5 w, riselvl2
	
	if (V_Flag==0)
		rise2 = V_LevelX
	else
		if (bDiagnose)
			Print "   can't find ending rise point using riselevls: ", riselvl1, riselvl2
		endif
		return -4
//!!! else should do some error handling
	endif
	wrise[nFound] =rise2-rise1
	if (wrise[nFound]>gMaxRise)
		if (bDiagnose)
			print "   risetime=",wrise[nFound],"=",rise2,"-",rise1
			Print "Rejecting, Risetime too long"		//praveen: print error text
		endif
		return -5
	endif
	if (wrise[nFound] < sampIntvl)
		if (wrise[nFound] > 0)
			wrise[nFound] = sampIntvl
		else
			wrise[nFound] = NAN
		endif
	endif
//
// Now find the half width
//
	hwlvl = 0.5*(wpky[nFound]) + wTInfo[0]
//abb, did I add in this print ???
print "           ", gBoxHW, wpkx[nFound], wstrt[nFound], hwlvl
	FindLevel /B=(gBoxHW) /T=0.5/Q/R=(wpkx[nFound], wstrt[nFound] ) w, hwlvl
	if (V_Flag==0)
		wHR[nFound] = V_LevelX
	else
		if (bDiagnose)
			Print "   can't find beginning 1/2 point using levl=: ", hwlvl
		endif
		return -6
	endif
	FindLevel /B=(gBoxHW) /Q/R=(wpkx[nFound], wpkx[nFound]+gMaxDur) /T=0.5 w, hwlvl
	if (V_Flag==0)
		hwX2 = V_LevelX
	else
		if (bDiagnose)
			Print "   can't find ending 1/2 point using levl=: ", hwlvl
		endif
		return -7
//!!! else should do some error handling
	endif
	wHW[nFound] =hwx2-wHR[nFound]

	if (whw[nFound]<gMinHW)
		if (bDiagnose)
			Print " Rejecting,  HW=",whw[nFound]," too low"		//praveen: print error text
			
		endif
		return -8
	endif
	if (whw[nFound] >	gMaxDur)	
		if (bDiagnose)
			Print "  Rejecting, HW too long=",whw[nFound]		//praveen: print error text
		endif
		return -9
	endif
	Return 0
End
//********************************************************************************
//
//********************************************************************************
Function findMiniEnd (nFound, bx, pol, bDiagnose, bLooseBase, w, wTInfo,  wend, wpkx, wpky, wdk)			
Variable nFound, bx, pol, bDiagnose, bLooseBase
Wave /D w, wTInfo, wend, wpkx, wpky, wdk

	variable /G gMaxDur, gMaxRise, gBasefraction, gEnd
	variable iErr, findend, blvl, pkx=wpkx[nFound]

	findend = min(pkx+gMaxDur,gEnd)				// dont look past end of wave
	if (bDiagnose)
		print "looking for end from ",pkx,"-",findend,"=",wTInfo[0]
	endif
	FindLevel /B=(bx) /Q /R=(pkx,findend) w, wTInfo[0]			// start at pk and look forwards to maximum duration
	if (V_Flag)													//use return to actual baseline, unless cant find it 
		if (bDiagnose)
			print "didnt find it, using",wTInfo[3]
		endif
		FindLevel /B=(bx) /Q /R=(pkx,findend) w, wTInfo[3]		// if not found, use baseline +/- noise
		if (V_Flag)
			if(bLooseBase)
				blvl = wTInfo[0] + pol*gBasefraction*(wTInfo[0]-wpkY[nFound])
				FindLevel /B=(bx) /Q /R=(pkx,findend) w, blvl		// if not found, use percent criterion
				if (V_Flag)
					wend[nFound] = pkx+gMaxRise+gMaxDur
					print "cant find end in findminiend, setting end of mini ",nFound," to ",wend[nFound]
					Return -2										// can't find end
				else
					wend[nFound] = V_LevelX
					if (bDiagnose)
						print "found return to percent criterion=",blvl,"at",wend[nFound]
					endif
				endif
			else
				wend[nFound] = pkx+gMaxRise+gMaxDur
				print "cant find end in findminiend, setting end of mini ",nFound," to ",wend[nFound]
				Return -2											// can't find end
			endif
		else															// we did find it
			wend[nFound] = V_LevelX
			if (bDiagnose)
				print "found looser baseline at",wend[nFound] 
			endif
		endif
	else
		wend[nFound] = V_LevelX
		if (bDiagnose)
			print "found true baseline at",wend[nFound]
		endif 
	endif
	
//!!! need to check for overlapping mini's here. Probably need to no the same when looking for mini start
//!!! need to measure decay times	
	Return iErr
End



//********************************************************************************
//
//********************************************************************************
Function calcMiniArea (nFound, bx, w, wTInfo, wstrt, wend, war)			
Variable nFound, bx
Wave /D w, wTInfo, wstrt, wend, war

	variable iErr
	war[nFound] = area (w,wstrt[nFound],wend[nFound])- wTInfo[0]*(wend[nFound]-wstrt[nFound])
	if (war[nFound]==NaN)
		iErr = -1
	endif
	Return iErr
End

//	inputs are 
//	outputs are
//
//	M_file[i]					name of file in which mini was found
//	M_strtX[i]				Starting x coordinate of the minis
//	M_pkX[i]					x coordinate of peak center of mini
//	M_ rise[i]				10-90 or 20-80% rise time
//	M_pkY[i]					Mini peak amplitudes after baseline correction
//	M_base[i]					Amplitude of the baseline used (could modify to allow sloping of base during mini)
//	M_endX[i]					Ending x coordinate of the minis
//	M_decay[i]				Decay time constant (or 90-10 or 80-20 % fall)
//	M_HW [i]					Full Width at Half Maximums of the baseline-corrected minis
//	M_HR [i]					Half rise point
//	M_area[i]					Areas of the baseline-corrected minis
//********************************************************************************
//
//********************************************************************************
Function MakeMiniWave (bWriteMini, bDisplayMini, alignMode, nFound, delX, bDiagnose, miniBase, w, wHR, wTInfo, wpk, wstrt)
Variable bWriteMini, bDisplayMini, alignMode, nFound, delX, bDiagnose
String  miniBase
wave w, wHR
wave /D  wTInfo, wpk, wstrt

	Variable /G gPre, gPost
	Variable xstrt, xend
	String miniStr
//
//
//	Ignore mini spanning waves for now. Extract from pre before peak to post after end
//	Note we are extracting up to gPost after peak of mini
//

	miniStr = "M"+miniBase+"_"+num2digstr(4,nFound)					// we can only handle 9,999 minis per wave
	if (alignMode==1)
		xstrt = max(wpk[nFound]-gPre, leftX(w))
		xend = min(wpk[nFound]+gPost, pnt2x(w,numpnts(w)-1))
	else
		if (alignMode==0) 
			xstrt = max(wstrt[nFound]-gPre, leftX(w))
			xend = min(wstrt[nFound]+gPost, pnt2x(w,numpnts(w)-1))
		else
			xstrt = max(wHR[nFound]-gPre, leftX(w))
			xend = min(wHR[nFound]+gPost, pnt2x(w,numpnts(w)-1))
		endif
	endif	
//print "gPost=",gPost,"wpk=",wpk[nFound]
	if (bDiagnose)
		print "trying to duplicate from",xstrt," to ",xend
	endif
	Duplicate /O /R=(xstrt,xend) /D w, $miniStr
//!!! Need to add detection parameter info to wave note !!! Need to use execute, or do it all after finishing

//print "delx=",delx 
	SetScale/P x, 0, delX, $miniStr								// setscales so they all start at 0
	Note $miniStr "Start="+num2str(xstrt)
	if (bWriteMini)
//		Save /O/P=outpath $miniStr as miniStr					// NOTE: we are overwriting, if exists
//		Save /O/P=outpath $miniStr as miniStr+".ibw"			// praveen: added .ibw extension for windows. in fact, the miniwaves are
															// not overwritten so no need to save them
	endif
	if (bDisplayMini) 
		display $miniStr
	endif
End
//*******************************************************************
Function findMaxBox2(w,pkwid, pkmode, lcsr, rcsr)
//
//	Returns the BEGINNING of the maximum pkwid length period in w between lcsr and rcsr-pkwid.
//   This is identical to findMaxBox, which is in MeasureWaves 4.5
wave w
variable pkwid						// in sec, or msec appropriate to the scaling of w
variable pkmode, lcsr, rcsr			// lcsr and rcsr are also in X values

variable strtx, endx, pkloc
variable /G gPk_Value

	strtX = lcsr												
	endX = rcsr-pkwid
	Duplicate /D /O /R=(strtx,endX) w, wdup
	wdup = faverage(w,X,X+pkwid)
	WaveStats /Q  wdup
	if (pkmode==1)										// Positive peaks, look for max
		pkloc = V_MaxLoc
		gPk_Value = V_Max
	else														// Negative peaks, look for min
		pkloc = V_MinLoc
		gPk_Value = V_Min
	endif

Return pkloc

End

//********************************************************************************
//
//********************************************************************************
Window PanDetectParams() : Panel
	PauseUpdate; Silent 1		// building window...
	NewPanel /W=(135,165,682,585) as "Detection Paramaters"
	SetDrawLayer UserBack
	SetDrawEnv linebgc= (0,0,0),fillfgc= (49163,65535,32768),fillbgc= (0,65535,0)
	DrawRect -3,79,280,315
	DrawRect 216,171,218,171
	SetDrawEnv fillfgc= (49151,65535,57456),fillbgc= (65535,16385,16385)
	DrawRect -3,1,280,79
	DrawRect 228,51,228,45
	SetDrawEnv fillfgc= (16385,28398,65535)
	DrawRect 547,1,280,126
	SetDrawEnv fillfgc= (13102,26214,0)
	DrawRect -3,315,280,401
	SetDrawEnv fsize= 13,fstyle= 1
	DrawText 0,347,"Update Interval:"
	SetDrawEnv fsize= 13,fstyle= 1
	DrawText 1,369,"Extract from:"
	SetDrawEnv fillfgc= (0,6405,26214)
	DrawRect 547,126,280,237
	SetDrawEnv fillfgc= (13112,0,26214)
	DrawRect 547,236,280,400
	SetDrawEnv fstyle= 1,textrgb= (65535,65535,65535)
	DrawText 401,326,"(Fraction of pk)"
	SetDrawEnv fstyle= 1,textrgb= (65535,65535,65535)
	DrawText 402,306,"SD of noise OR"
	SetDrawEnv linebgc= (65535,0,0),fillfgc= (65535,0,26214)
	DrawRect 457,353,536,399
	SetDrawEnv fstyle= 1
	DrawText 10,117,"Write"
	SetDrawEnv fstyle= 1
	DrawText 3,143,"Display"
	SetVariable setboxdetect,pos={303,65},size={216,17},title="Box width (detection; #pts)"
	SetVariable setboxdetect,limits={1,25,2},value= gBox
	PopupMenu popPolarity,pos={36,53},size={142,19},title="Polarity"
	PopupMenu popPolarity,mode=1,value= #"\"Negative;Positive\""
	Button btnDoIt,pos={471,363},size={52,25},proc=ButDoDetectProc,title="Do it"
	CheckBox chkWriteMinis,pos={61,97},size={95,24},title="Mini Waves",value=0
	CheckBox chkInteractive,pos={61,181},size={168,25},title="Interactive Mode",value=1
	SetVariable setStart,pos={4,4},size={116,17},title="Start (ms)",format="%2.1f"
	SetVariable setStart,frame=0,limits={0,1e+06,0},value= gStart
	SetVariable setEnd,pos={126,4},size={128,17},title="End (ms)",format="%2.1f"
	SetVariable setEnd,limits={0,1e+06,0},value= gEnd
	SetVariable setPre,pos={103,351},size={59,17},title="Pre",format="%2.1f",frame=0
	SetVariable setPre,limits={0,1e+06,0},value= gPre
	SetVariable setPost,pos={163,351},size={115,17},title="to Post (ms)"
	SetVariable setPost,format="%2.1f",frame=0,limits={0,1e+06,0},value= gPost
	SetVariable setThresh,pos={411,151},size={101,17},title="Relative"
	SetVariable setThresh,format="%2.1f",limits={0,1000,0},value= gThresh
	SetVariable setBaseWidth,pos={294,249},size={193,17},title="Detection Baseline (ms)"
	SetVariable setBaseWidth,format="%2.1f",limits={0,1e+06,0},value= gBWid
	SetVariable setMinDur,pos={440,172},size={86,17},title="Min (ms)",format="%2.1f"
	SetVariable setMinDur,limits={0,1e+06,0},value= gMinDur
	SetVariable setMaxDur,pos={306,173},size={131,17},title="Duration: Max"
	SetVariable setMaxDur,format="%2.1f",limits={0,1e+06,0},value= gMaxDur
	SetVariable setBThresh,pos={303,290},size={92,17},title="Return to"
	SetVariable setBThresh,format="%2.1f",limits={-1e+06,1e+06,0},value= gBThresh
	SetVariable setfwid,pos={303,25},size={215,17},title="Filter width (pre; mS)           "
	SetVariable setfwid,format="%2.1f",limits={0,1000,0},value= gFiltWid
	SetVariable setboxPk,pos={303,45},size={215,17},title="width for pk measure (ms)    "
	SetVariable setboxPk,limits={0,25,0},value= gPkAvg
	PopupMenu popfilter,pos={328,3},size={173,19},title="Filter: "
	PopupMenu popfilter,mode=2,value= #"\"None; Detection only;Detection and Output\""
	SetVariable setMinUpdate,pos={125,330},size={69,17},title="min",format="%2.1f"
	SetVariable setMinUpdate,frame=0,limits={0,1e+06,0},value= gMinUpdate
	SetVariable setMaxUpdate,pos={196,330},size={81,17},title="Max",format="%2.1f"
	SetVariable setMaxUpdate,frame=0,limits={0,1e+06,0},value= gMaxUpdate
	PopupMenu poprise,pos={148,373},size={131,19},title="Rise time"
	PopupMenu poprise,mode=2,value= #"\"10-90;20-80;none\""
	SetVariable setMaxRise,pos={326,195},size={156,17},title="Max Rise Time (ms)"
	SetVariable setMaxRise,format="%2.1f",limits={0,1e+06,0},value= gMaxRise
	SetVariable setBaseWidth2,pos={296,269},size={212,17},title="Measurement Baseline (ms)"
	SetVariable setBaseWidth2,format="%2.1f",limits={0,1e+06,0},value= gBPreWid
	CheckBox chkWriteSource,pos={163,97},size={104,23},title="Source Wave",value=0
	SetVariable setMinThresh,pos={308,151},size={90,17},title="absolute"
	SetVariable setMinThresh,format="%2.1f",limits={0,1000,0},value= gMinThresh
	CheckBox ChkDisplayExtracted,pos={61,124},size={95,24},title="Mini Waves",value=0
	CheckBox chkWholeWave,pos={26,25},size={168,25},title="Or, Use Whole Wave",value=0
	CheckBox chkDisplayDetected,pos={163,124},size={103,23},title="Detection",value=1
	CheckBox chkMeasure,pos={61,152},size={168,25},title="Measure as detected",value=1
	PopupMenu popalign,pos={0,374},size={146,19},title="Align on: "
	PopupMenu popalign,mode=1,value= #"\"Mid Rise;Start;Peak\""
	SetVariable setbasefraction,pos={341,310},size={54,17},title=" ",format="%3.2f"
	SetVariable setbasefraction,limits={0,1,0.01},value= gBasefraction
	SetVariable setMinHW,pos={331,217},size={156,17},title="Min Half Width (ms)"
	SetVariable setMinHW,format="%2.1f",limits={0,1e+06,0},value= gMinHW
	SetVariable setboxHW,pos={303,85},size={216,17},title="Box width (HW, End)            "
	SetVariable setboxHW,limits={1,25,2},value= gBoxHW
	SetVariable setboxRise,pos={303,105},size={216,17},title="Box width (Rise)                 "
	SetVariable setboxRise,limits={1,25,2},value= gBoxRise
	PopupMenu popThreshMode,pos={309,129},size={182,19},title="Threshold: use"
	PopupMenu popThreshMode,mode=1,value= #"\"Absolute;Relative to noise;greater;lesser\""
	CheckBox chkDiagnostics,pos={61,212},size={168,25},title="Print Diagnostics",value=1
	CheckBox ChkLooseBase,pos={284,310},size={52,17},title="Or to",value=1
	SetVariable setMiniBase,pos={11,244},size={136,17},title="Mini Basename"
	SetVariable setMiniBase,limits={-INF,INF,1},value= gResName
	SetVariable setFile,pos={11,266},size={216,17},title="Folder"
	SetVariable setFile,limits={-INF,INF,1},value= gResPath
	Button btnSetPath,pos={233,263},size={37,21},proc=setResPath,title="Set"
EndMacro

//********************************************************************************
//
//********************************************************************************
Proc ButDoDetectProc(ctrlName) : ButtonControl
	String ctrlName
// Get the current info from the panel and call DetectMinis
	variable bInter, bWriteMini, bDisplayExt, bDisplayDet		// 0=no, 1=yes
	variable bWriteSrc, bwholeW, bmeasure
	variable pol 												//-1 = neg, +1 = pos
	variable alignMode											// 0 = , 1 = ,2 = 
	String riseType, doFilter
	
	if (cmpstr(ctrlName,"btnSelectFile")==0)
		doWindow /F panselectwaves
		return 
	endif
	
	silent 1
	ControlInfo /W=PanDetectParams chkInteractive
	bInter=V_value 
	ControlInfo /W=PanDetectParams  chkWriteMinis
	bWriteMini=V_value 
	ControlInfo /W=PanDetectParams  chkDisplayExtracted
	bDisplayExt=V_value 
	ControlInfo /W=PanDetectParams  chkDisplayDetected
	bDisplayDet=V_value 
	ControlInfo /W=PanDetectParams  chkWriteSource
	bWriteSrc=V_value 
	ControlInfo /W=PanDetectParams  chkDisplayDetected
	bDisplayDet=V_value 
	ControlInfo /W=PanDetectParams  chkMeasure
	bMeasure=V_value 
	ControlInfo /W=PanDetectParams popPolarity			// using the string instead of the index is more robust to future changes in the panel
	if (cmpstr(S_value,"positive")==0)
		pol = 1
	else
		pol = -1
	endif 
	ControlInfo /W=PanDetectParams popAlign			// Determines alignment of extracted minis
	if (cmpstr(S_value,"Peak")==0)
		alignMode = 1
print "aligning on peak"
	else
		if (cmpstr(S_value,"Start")==0)		
			alignMode = 0
print "aligning on start"
		else
			alignMode = 2
print "aligning on mid Rise"
		endif
	endif 
	ControlInfo /W=PanDetectParams popRise
	riseType=S_value 
	ControlInfo /W=PanDetectParams popFilter
	doFilter=S_value 

	if (cmpstr(wlist,"")==0)
		doWindow PanSelectWaves
	else
		DetectMinis(bInter,bWriteMini,bDisplayExt, bDisplayDet,bWriteSrc, bmeasure, alignMode, pol, riseType, doFilter)
	endif
End

//********************************************************************************
//
//********************************************************************************
Function RemoveWfromList(ctrlName,popNum,wavStr) : PopupMenuControl
	String ctrlName
	Variable popNum
	String wavStr

	String /G wlist						// this contains the list of waves
	variable offset
	offset = FindListItem(wavStr,wlist, ";", 0)
	if (offset >= 0)
		wlist[offset, offset+strlen(wavStr)] = ""		// excise "wavStr;"
	endif
	doUpdate
End

//********************************************************************************
//
//********************************************************************************
Function AddWave2List(ctrlName,popNum,wavStr) : PopupMenuControl
	String ctrlName
	Variable popNum
	String wavStr
	String/G wlist						// this contains the list of waves
	doUpdate
	wlist += wavStr
	wlist += ";"
	doUpdate
End
//********************************************************************************
//
//********************************************************************************
Function AddAllWaves2List (ctrlName) : ButtonControl
	String ctrlName

	string/G wlist = WaveList("*", ";", "")
End
//********************************************************************************
// This routine is called when the user presses the "done" button on the resize panel
//********************************************************************************
Function resize ()
	NVAR xstart= gXstart
	NVAR xend= gXend
	NVAR ystart= gYstart
	NVAR yend= gYend

	SetAxis bottom, Xstart, Xend
	SetAxis left, Ystart, Yend
End
//********************************************************************************
//
//********************************************************************************
Window PanSelectWaves() : Panel
	PauseUpdate; Silent 1		// building window...
//abb put it in nicer location
//	NewPanel /W=(20,110,641,223)
	NewPanel /W=(602.25,59,1223.25,172.25)
	ShowTools
	Button butDone,pos={413,69},size={135,30},proc=ChangeWindows,title="Done"
	PopupMenu popWaveAdd,pos={0,12},size={208,19},proc=AddWave2List,title="Add a Wave"
	PopupMenu popWaveAdd,mode=2,value= #"WaveList(\"*\", \";\", \"\")"
	PopupMenu popWaveRemove,pos={304,11},size={129,19},proc=RemoveWfromList,title="Remove a Wave"
	PopupMenu popWaveRemove,mode=4,value= #"wlist"
	SetVariable setWList,pos={3,45},size={547,17},title="Wave List"
	SetVariable setWList,limits={-INF,INF,0},value= wlist
	Button butcvtET,pos={217,69},size={135,30},proc=ChangeWindows,title="Convert ET File"
	Button butLoad,pos={3,69},size={186,29},proc=LoadIgorBin,title="Load Waves from folder"
EndMacro
//********************************************************************************
//
//********************************************************************************
Window cvtETpanel() : Panel
	PauseUpdate; Silent 1		// building window...
	NewPanel /W=(221,82,578,254)
	ShowTools
	SetDrawLayer UserBack
	DrawText 16,149,"(* to use ET file name)"
	Button button0,pos={6,77},size={124,24},proc=CvtET,title="Convert ET File"
	PopupMenu popmode,pos={7,7},size={104,19}
	PopupMenu popmode,mode=1,value= #"\"Single Wave;One Wave per Record; Multiple Waves\""
	SetVariable setETstrt,pos={161,6},size={86,17},title="Start"
	SetVariable setETstrt,limits={-INF,INF,0},value=gStart
	SetVariable setETEnd,pos={163,28},size={86,17},title="End"
	SetVariable setETEnd,limits={-INF,INF,0},value=gEnd
	SetVariable setWaveDur,pos={105,52},size={176,17},title="Wave Duration (ms)"
	SetVariable setWaveDur,limits={-INF,INF,0},value=gWavePts
	SetVariable setbase,pos={6,112},size={203,17},title="base name"
	SetVariable setbase,limits={-INF,INF,0},value=gETBase
	CheckBox chkWhole,pos={7,28},size={130,20},title="Use Whole ET File",value=1
EndMacro
//********************************************************************************
//
//********************************************************************************
Window resizepanel() : Panel
	PauseUpdate; Silent 1		// building window...
	NewPanel /W=(289,103,553,212)
	ShowTools
	SetDrawLayer UserBack
	SetDrawEnv fsize= 14,fstyle= 1
	DrawText 46,17,"Resize Display"
	Button button0,pos={72,67},size={50,20},proc=resize,title="DONE"
	SetVariable setXstart,pos={9,23},size={81,16},title="X axis"
	SetVariable setXstart,limits={-inf,inf,0},value= gXstart
	SetVariable setXend,pos={95,23},size={81,16},title=" "
	SetVariable setXend,limits={-inf,inf,0},value= gXend
	SetVariable setYstart,pos={8,44},size={81,16},title="Y axis"
	SetVariable setYstart,limits={-inf,inf,0},value= gYstart
	SetVariable setYend,pos={94,44},size={81,16},title=" "
	SetVariable setYend,limits={-inf,inf,0},value= gYend
EndMacro
//********************************************************************************
//
//********************************************************************************
Function CvtET(ctrlName) : ButtonControl
	String ctrlName

	String /G gETBase
	string fList, modestr, filpath, startstr, endstr, basestr, cmdstr, cmdtypestr
	variable filenum=0, istrt = 0, fvar, ipos, endfile
	wave info_duration= $"info_duration"
	wave info_start=$"info_start"

 	Open /D/M="Select an ET file"  /R/T="CFDF"  fvar 
	filpath = S_fileName 
//print filpath
	do
		ipos = strsearch(filpath,":",istrt)
		if (ipos != -1)
			istrt = ipos+1
		else
			break
		endif
	while (1)				// istrt currently points to character after last colon
	if (cmpstr(gETbase,"*")==0) 
		basestr = addquotes("E"+filpath[istrt+2,strlen(filpath)])
	else
		basestr = addquotes(gETbase)	
	endif
//
//	Get rid of all periods
//
	do				
		ipos = strsearch(basestr,".",0)
		if (ipos != -1)
			basestr[ipos,ipos] = ""
		else
			break
		endif
	while (1)

	controlInfo chkWhole
	if (V_Value) 							// we wish to use the whole file
		startstr = "0"
		cmdstr =  "let "+ addquotes("info")+"," + addquotes(filpath)
		print cmdstr
		Execute cmdstr
		endfile = info_duration[0]*numpnts(info_duration)
print "endfile=",endfile,"= dur:",info_duration[0],"num",numpnts(info_duration)
		endstr = num2str (endfile)
//		killwaves  $"info_start",$"info_duration"
	else											// use a portion of the file
		controlinfo setETstrt
		startstr =num2str (V_value)
		controlinfo setETEnd
		endstr = num2str (V_value)
	endif
	controlinfo popmode
	modestr = S_Value
	if (cmpstr(modestr,"single wave")==0)
		cmdtypestr = addquotes("fet")
	endif
	if (cmpstr(modestr,"one wave per record")==0)
		cmdtypestr = addquotes("betw")
	endif
	cmdstr =  "fet " + basestr + ","+ addquotes(filpath[istrt,strlen(filpath)])+ ","  + startstr+ "," + endstr
	print cmdstr
	Execute cmdstr
End

//********************************************************************************
//
//
//
//********************************************************************************
Function ChangeWindows (ctrlName) : ButtonControl
	String ctrlName

	if (cmpstr(ctrlname,"butdone")==0)
		DoWindow /F PanDetectParams
	endif
	if (cmpstr(ctrlname,"butcvtet")==0)
		DoWindow /F CvtETPanel
	endif
	
End

//********************************************************************************
// InitWaves - Initialize the wave that holds the info about each psp
// 	Input:
//		maxEvnts - Maximum number of psp's the program can handle
//********************************************************************************
Macro InitWaves (maxEvnts, bReinit)
	Variable maxEvnts, bReinit

	if (bReinit)
		Redimension  /N=(maxEvnts) M_file
		Redimension /D /N=(maxEvnts) M_strtX, M_pkX, M_rise, M_pkY, M_base, M_endX, M_decay, M_HW, M_HR, M_area
	else
		Make /T /N=(maxEvnts) /O M_file
		Make /D /N=(maxEvnts) /O  M_strtX, M_pkX, M_rise, M_pkY, M_base, M_endX, M_decay, M_HW, M_HR,M_area
	endif

EndMacro

//********************************************************************************
//********************************************************************************
//********************************************************************************
//********************************************************************************
//********************************************************************************
//********************************************************************************
//********************************************************************************
//********************************************************************************
