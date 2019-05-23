private
_errorMessage

	^ 'The method protocol ', 
		self methodDefinition protocol printString, 
		' for the method ', 
		self _methodPrintString printString, 
		' in the package ', 
		self packageName printString,
		' violates the package convention ', 
		self packageConvention printString, 
		'. '