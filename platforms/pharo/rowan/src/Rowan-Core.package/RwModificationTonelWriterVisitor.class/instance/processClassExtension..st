class extension writing
processClassExtension: aClassExtensionModification

	"write out the class definition"

	(currentClassExtension instanceMethodDefinitions isEmpty and: [ currentClassExtension classMethodDefinitions isEmpty ])
		ifTrue: [ self _classExtensionSourceFile ensureDelete ]
		ifFalse: [ 
			self _classExtensionSourceFile
				writeStreamDo: [:aStream |
					self _writeClassExtension: currentClassExtension on: aStream.
					self _writeClassSideMethodDefinitions: currentClassExtension on: aStream.
					self _writeInstanceSideMethodDefinitions: currentClassExtension on: aStream ] ]