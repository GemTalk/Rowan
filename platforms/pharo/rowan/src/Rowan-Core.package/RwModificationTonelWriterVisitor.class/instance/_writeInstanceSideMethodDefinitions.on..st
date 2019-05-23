class writing
_writeInstanceSideMethodDefinitions: aClassDefinition on: aStream
	((aClassDefinition instanceMethodDefinitions values )
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self _writeMethodDefinition: each classDefinition: aClassDefinition isMeta: false on: aStream ]