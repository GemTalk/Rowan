class writing
processClassExtension: aClassExtensionModification

	"create class extension directory and defintion structure"

	(currentClassExtension instanceMethodDefinitions isEmpty and: [ currentClassExtension classMethodDefinitions isEmpty ])
		ifTrue: [ self _classExtensionSourceDir ensureDeleteAll ]
		ifFalse: [ 
			self _classExtensionSourceDir ensureCreateDirectory.
			(self _classExtensionSourceDir /  'properties.json')
				writeStreamDo: [:aStream |
					aStream 
						<< (self _classExtensionOf: self currentClassExtension)
						<< self _newLine ].

			instanceFileNameMap  := self _createFileNameMapForMethods: aClassExtensionModification after instanceMethodDefinitions.
			classFileNameMap := self _createFileNameMapForMethods: aClassExtensionModification after classMethodDefinitions.
			instanceBeforeFileNameMap  := self _createFileNameMapForMethods: aClassExtensionModification before instanceMethodDefinitions.
			classBeforeFileNameMap := self _createFileNameMapForMethods: aClassExtensionModification before classMethodDefinitions.

			aClassExtensionModification instanceMethodsModification acceptVisitor: self.
			aClassExtensionModification classMethodsModification acceptVisitor: self ]