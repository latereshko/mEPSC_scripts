#pragma rtGlobals=1		// Use modern global access method.

//********************************************************************************
//	NoteUtils a collection of notebook utility functions
//	Notes are entered into forms which are stored on disk and which contain text to prompt user
//	entries. Each form contains info related to an experimental event (e.g. new cell, new test new condition
//	etc. These forms are generic but may be modified by individual users.
//
//
//********************************************************************************

Macro setNotebook(ctrlName, popNum, popStr) : PopupControl
	String ctrlName
	variable popNum
	string popStr
	
	silent 1
	string textstr
	variable modeVal
	String notebkStr= UniqueName("NoteBook", 10, 0)		// unique name allows multiple open notebooks
	
	if (cmpstr(popStr,"OPEN EXISTING")==0)
		openNotebook /M="Select an existing notebook" /N=notebkStr
		PopupMenu ctrlName value=WinList("*", ";","WIN: 16")+";NONE;OPEN EXISTING;OPEN NEW"
		PopupMenu ctrlName mode=FindItemIndexInList (notebkStr,WinList("*", ";","WIN: 16"), ";")
	endif
	if (cmpstr(popStr,"OPEN NEW")==0)
		newNotebook /N=$notebkStr
		Notebook $notebkStr newRuler=Title, justification=1, rulerDefaults={"Times", 16, 1, (0, 0, 0)}
		sprintf textstr, "New Analysis Notebook, %s, %s\r", Secs2Date(datetime, 0), time()
		Notebook $notebkStr ruler=Title, text=textstr
		Notebook $notebkStr ruler=Normal; Notebook $notebkStr margins={18,18,504}, tabs={63 + 3*8192}
		PopupMenu $ctrlName value=WinList("*", ";","WIN: 16")+";NONE;OPEN EXISTING;OPEN NEW"
		modeVal = FindItemIndexInList (notebkStr,WinList("*", ";","WIN: 16"), ";")
		PopupMenu $ctrlName mode=modeVal
	endif
End
|*******************************************************************
Function Notes(notebkStr,noteTypeStr,noteStr)
	String notebkStr,noteTypeStr,noteStr
	variable status
//	For text, noteStr contains the text. For graphics, notestr contains the name of the object (layout or graph)
//	For tables, notestr should contain the delimited list of waves 

	silent 1
//print notebkStr
do																// select type of note
if (cmpstr(noteTypeStr,"title")==0)
	Notebook $notebkStr ruler=Title, text="\r"+noteStr
	break
endif
if (cmpstr(noteTypeStr,"list")==0)
	Notebook $notebkStr ruler=Normal, text="\r"+noteStr
	break
endif
if (cmpstr(noteTypeStr,"table")==0)						// 
	break
endif
if (cmpstr(noteTypeStr,"text")==0)
	Notebook $notebkStr ruler=Normal, text="\r"+noteStr
	break
endif
if (cmpstr(noteTypeStr,"time")==0)
	break
endif
if (cmpstr(noteTypeStr,"date")==0)
	break
endif
if (cmpstr(noteTypeStr,"object")==0)
	Notebook $notebkStr picture = {$noteStr,1,0}			//mode 1 = regular pict, flag 0 = B&W
	break
endif

while (1)
return status
End

|*******************************************************************
|*******************************************************************
|*******************************************************************
|*******************************************************************
|*******************************************************************
|*******************************************************************
|*******************************************************************
|*******************************************************************

// insert title
	Notebook Report newRuler=Title, justification=1, rulerDefaults={"Times", 16, 1, (0, 0, 0)}
	sprintf str, "Curve fit, %s, %s\r", Secs2Date(datetime, 0), time()
	Notebook Report ruler=Title, text=str
	
// insert fit results
	sprintf str, "K0 = %g±%g\rK1 = %g±%g\rK2 = %g±%g\r", K0, sig0 , K1, sig1, K2, sig2
	
// insert graphs
	Notebook Report picture={ExpFitGraph, 0, 0}, text="\r"
	Notebook Report picture={ResidualsGraph, 0, 0}, text="\r"
