class writing
processClass: aClassModification

	"create class directory and class defintion structure"

	self _classSourceDir ensureCreateDirectory.

	(self _classSourceDir /  'README.md')
		writeStreamDo: [:aStream |
		aStream 
			<< (self _commentOf: self currentClassDefinition) ].

	(self _classSourceDir /  'properties.json')
		writeStreamDo: [:aStream |
			aStream 
				<< (self _typeClassDefinitionOf: self currentClassDefinition)
				<< self _newLine ].

	instanceFileNameMap  := self _createFileNameMapForMethods: aClassModification after instanceMethodDefinitions.
	classFileNameMap := self _createFileNameMapForMethods: aClassModification after classMethodDefinitions.
	instanceBeforeFileNameMap  := self _createFileNameMapForMethods: aClassModification before instanceMethodDefinitions.
	classBeforeFileNameMap := self _createFileNameMapForMethods: aClassModification before classMethodDefinitions.

	currentClassDefinition instanceMethodDefinitions isEmpty
		ifTrue: [  self _methodInstanceDir ensureDeleteAll ]
		ifFalse: [ aClassModification instanceMethodsModification acceptVisitor: self ].
	currentClassDefinition classMethodDefinitions isEmpty
		ifTrue: [  self _methodClassDir ensureDeleteAll ]
		ifFalse: [ aClassModification classMethodsModification acceptVisitor: self ]