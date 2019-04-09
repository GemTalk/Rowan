Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-35x' }
Class >> _equivalentSubclass: oldClass superCls: actualSelf name: aString newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars inDict: aDictionary constraints: aConstraint isKernel: isKernelBool [

	 self _equivalentSubclass: oldClass superCls: actualSelf name: aString newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars inDict: aDictionary isKernel: isKernelBool
]

{ #category : '*rowan-gemstone-35x' }
Class >> _subclass: className instVarNames: anArrayOfInstvarNames format: theFormat constraints: theConstraints classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary inClassHistory: aClassHistory description: aDescription options: optionsArray [
  ^ self _subclass: className instVarNames: anArrayOfInstvarNames format: theFormat classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary inClassHistory: aClassHistory description: aDescription options: optionsArray
]

{ #category : '*rowan-gemstone-35x' }
Class >> indexableSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDict inDictionary: aDictionary newVersionOf: oldClass description: aDescription constraints: constraintsArray options: optionsArray [
	^ self indexableSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDict inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray
]

{ #category : '*rowan-gemstone-35x' }
Class >> subclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription constraints: theConstraints options: optionsArray [
	^ self subclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray
]
