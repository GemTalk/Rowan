Extension { #name : 'RwAbstractTool' }

{ #category : '*rowan-tools-gemstone' }
RwAbstractTool >> _log: aString [

"
If the client is a topaz process, the default logger writes to the 
 current output file as controlled by topaz OUTPUT PUSH statements, 
 else to stdout.
"
	GsFile gciLogClient: aString
]
