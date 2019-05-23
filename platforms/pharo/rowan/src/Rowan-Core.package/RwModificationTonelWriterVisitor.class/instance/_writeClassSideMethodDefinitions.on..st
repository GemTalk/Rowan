class writing
_writeClassSideMethodDefinitions: aClassDefinition on: aStream
	((aClassDefinition classMethodDefinitions values )
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self _writeMethodDefinition: each classDefinition: aClassDefinition isMeta: true on: aStream ]