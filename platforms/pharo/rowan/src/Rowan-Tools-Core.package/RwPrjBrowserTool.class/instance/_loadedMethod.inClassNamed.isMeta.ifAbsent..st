private
_loadedMethod: methodSelector inClassNamed: className isMeta: isMeta ifAbsent: absentBlock

	^ Rowan image
		loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | ^ loadedMethod ]
		ifAbsent: absentBlock