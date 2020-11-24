//********************************************************************************
//
//	SNUtilities 2.3 - this file should be placed in Igor Pro Folder: User Procedures
//	it is req	uired to compile MeasureWaves and DetectMini
//   version 2.2--added capacity to handle quoted wave names, fixed a bug in NumItemsInList and FindItemIndexInList
//	version 2.3--replaced getStrFromList with built in function stringFromList 
//
//********************************************************************************

|********************************************************************************
Function/S ReplaceListSeparator(list, oldSeparator,newSeparator)
	String list, oldSeparator, newSeparator

//	this assumes separators are both only 1 character.
	Variable offset = 0

	do
		offset = strsearch(list,oldSeparator,offset)	// e.g. search for ";"
		if (offset >= 0)
			list[offset, offset] =  newSeparator		// replace 
		else
			break
		endif
	while (1)

	return list
End
|********************************************************************************
Function NumItemsInList (list, Separator)
	String list, Separator
// returns the zero-based number of items, note: an item with no separator will be counted as one
	Variable offset = 0, num = 0, lastChar =  strlen(list)-1
//print "from numitems"
	do
		offset = strsearch(list,Separator,0)				//e.g. search for ";"
//print "offset=",offset
		if (offset < 0)									// if none found, we're done
			if (lastchar > 0)							// if there's an item left count it
				num += 1
			endif
			return num
		endif
		if (offset==0)										// remove any initial separator
			list = list[1,lastchar]
		else													// there is a separator and its not the initial character
			if (offset < lastChar)							// if this isnt the last character
				num += 1									// increment the counter
				list = list[offset+1,lastchar]				// strip the item and continue
			else												// offset must = lastchar, so count the item and leave
				num+= 1
				return num
			endif
		endif
		lastChar =  strlen(list)-1
//print list, "lastchar=",lastchar
	while (1)

	return num
End
|********************************************************************************
Function FindItemIndexInList  (item,list, separator)
//* returns the zero-based index of item in list, if it exists, or -1 if it does not.
	String item,list, separator

	Variable offset = 0, num = 0	
	if (cmpstr(list[strlen(list)-1], separator)!=0)		// need a trailin separator for this to work
		list = list+separator
	endif
	offset = strsearch(separator+list,separator+item+separator,offset)		// search for ";item;" to avoid finding items within other items
	if (offset==-1)			// not found, return -1
		return offset
	endif
	list = list[0,offset-1]				// throw away everything after the last char before item was found
//print "trunc list=",list
	num = NumItemsInList (list, Separator)
	return num
End
//*******************************************************************
Function /S ChunkList(bigstr,sep, chunksize)
String bigstr, sep
variable chunksize

//  Used to divide a list string into chunks, without chopping items in half. 
// Returns the next chunk string, so must be called iteratively.

Variable nxtsep, lastsep= -1
String chunkstr

	do													// Loop through the items in the chunk
		nxtsep = StrSearch(bigstr, sep , lastsep+1)
		if (nxtsep > chunksize)							// we're done with this chunk
			chunkstr = bigstr[0,lastsep]
			break
		endif
		if (nxtsep >= 0)
			lastsep = nxtsep
		endif
		if (nxtsep == -1)								// we're done with the big string
			chunkstr = bigstr[0,strlen(bigstr)]
			break
		endif
	while (1)
	return chunkstr

End

//********************************************************************************
Function /S num2digstr (digits,num)
	variable digits, num
	
//
//	This function returns a string representing a number padded with zeros, so that the number of character
//	= digits. If num occupies more digits than requested, the excess low digits of the number are truncated. 
// 	e.g. calling num2digstr (3,1234) returns "123", while  calling num2digstr (6,1234) returns "001234"
//
	String outstr, zerostr="000000000000", numstr = num2istr(num)
	variable i=1
	
	if (strlen(numstr) <= digits) 
		outstr = zerostr[0,digits-1]		
		outstr[digits-strlen(numstr),digits-1] = numstr
	else
		outstr = numstr[0,digits-1]
	endif
	
	return outstr
End
//********************************************************************************
Function /S addquotes (instr)
	String instr
	String outstr
	
	outstr = "\"" + instr +  "\""
	return outstr
End
//********************************************************************************
//Function /S RemoveQuotesFromList (list, separator)
//	Input is a list of names that may contain quoted liberal names.
//	Returns the list with liberal names quotes removed.
//	Example:
//		Input:		"wave0;'wave 1';"
//		Output:		"wave0;wave 1;"
//	The list is expected to be a standard separated list, like "wave0;wave1;wave2;".
Function /S RemoveQuotesFromList (list, separator)
	String list
	String separator
	
	Variable startIndex, endIndex
	String outputList = "", item
	startIndex = 0; endIndex = strlen(list)
	do
		if (startIndex >= endIndex)			// no more items?
			break
		endif
		item = stringFromList(0, list[startIndex, endIndex], separator)
		outputList += RemoveQuotesFromName(item) + separator
		startIndex += strlen(item) + 1		// skip items we've already extracted
	while(1)

	return outputList
End
//********************************************************************************
Function /S RemoveQuotesFromName (item)
	String item
	String outItem
	
	if ((cmpstr(item[0],"\'")==0)%& (cmpstr(item[strlen(item)-1],"\'")==0))
		outitem = item[1,strlen(item)-2]
	else
		outitem = item
	endif
	
	return outitem
End

// a function to add all waves matching MatchStrW to wavelist (praveen)
Function pt_AddMatchStrWToList(MatchStrW)
// Example: pt_AddMatchStrWToList("Cell_002135_*")
String MatchStrW
String /G wlist
String WNameStr
Variable N,i
wlist = wavelist(MatchStrW,";","")
End

// a function to scale waves (praveen)
Function pt_ScaleWaves(MatchStrW, MultiplicFactor)
//Example: pt_ScaleWaves("Cell_002138_*", 1e12)
String MatchStrW
Variable MultiplicFactor
String WList, WNameStr
Variable i, N
WList=wavelist(MatchStrW,";","")
N=ItemsInList(wList,";")
For (i=0; i<N; i+=1)
	WNameStr=StringFromList(i, Wlist, ";")
	wave w=$WNameStr
	w=w*MultiplicFactor
EndFor
Print "Multiplied N=",N,"waves with",MultiplicFactor
End