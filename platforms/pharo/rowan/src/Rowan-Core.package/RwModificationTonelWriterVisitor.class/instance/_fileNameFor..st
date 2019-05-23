class writing
_fileNameFor: aClassDefinition
	^ String streamContents: [ :stream | 
		stream 
			<< aClassDefinition name
			<< '.' << (self _typeOfClass: aClassDefinition) asLowercase
			<< '.st'  ]