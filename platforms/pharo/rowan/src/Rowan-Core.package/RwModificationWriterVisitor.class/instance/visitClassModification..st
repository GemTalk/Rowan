visiting
visitClassModification: aClassModification

	aClassModification isAddition
		ifTrue: [ ^ self addedClass: aClassModification ] .
	aClassModification isDeletion
		ifTrue: [ ^ self deletedClass: aClassModification ].
(aClassModification instanceMethodsModification isEmpty and: [ aClassModification classMethodsModification isEmpty ])
	ifTrue: [
		"no instance or class modifications, check to see if the only Property modification is for 'gs_SymbolDictionary' "
		| propertiesModification elementsModified |
		propertiesModification := aClassModification propertiesModification.
		elementsModified := propertiesModification elementsModified.
		(elementsModified size = 1 and: [ (elementsModified at: 'gs_SymbolDictionary' ifAbsent: []) notNil])
			ifTrue: [
				"'gs_symbolDictionary' property is not written to disk, so we can skip this class"
				^ self ]].
	^ self changedClass: aClassModification
