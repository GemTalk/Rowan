class writing
_commentOf: aClassDefinition
	^ (aClassDefinition comment 
		copyReplaceAll: '"' 
		with: '""')
		withLineEndings: self _newLine