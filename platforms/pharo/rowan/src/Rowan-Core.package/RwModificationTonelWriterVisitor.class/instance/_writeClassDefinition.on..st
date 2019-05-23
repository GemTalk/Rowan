class writing
_writeClassDefinition: aClassDefinition on: aStream
	| nl |
	nl := self _newLine.
	
	(self _hasCommentOf: aClassDefinition) 
		ifTrue: [ 
			aStream 
				<< '"' << nl
				<< (self _commentOf: aClassDefinition) << nl
				<< '"' << nl ].
	aStream
		<< (self _typeOfClass: aClassDefinition) 
		<< ' ' << (self _typeClassDefinitionOf: aClassDefinition ) << nl