class writing
processClass: aClassModification

	"write out the class definition"

	self _classSourceFile
		writeStreamDo: [:aStream |
			self _writeClassDefinition: currentClassDefinition on: aStream.
			self _writeClassSideMethodDefinitions: currentClassDefinition on: aStream.
			self _writeInstanceSideMethodDefinitions: currentClassDefinition on: aStream ]