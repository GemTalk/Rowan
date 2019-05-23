private
_errorMessage

	^ 'The class category ', 
		self classDefinition category printString, 
		' for the class ', 
		self classDefinition name printString, 
		' violates the package convention ', 
		self packageConvention printString