Extension { #name : 'RwGsSymbolDictionaryRegistry' }

{ #category : '*rowan-gemstone-loader' }
RwGsSymbolDictionaryRegistry class >> _classAdditionPatchClass [

	^ RwGsClassAdditionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader' }
RwGsSymbolDictionaryRegistry class >> _classDeletionPatchClass [

	^ RwGsClassDeletionSymbolDictPatch 
]

{ #category : '*rowan-gemstone-loader' }
RwGsSymbolDictionaryRegistry class >> _classExtensionPatchClass [

	^ RwGsClassExtensionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader' }
RwGsSymbolDictionaryRegistry class >> _classPropertiesPatchClass [

	^ RwGsClassPropertiesSymDictPatch
]

{ #category : '*rowan-gemstone-loader' }
RwGsSymbolDictionaryRegistry class >> _classVariablePatchClass [

	^ RwGsClassVariableChangeSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader' }
RwGsSymbolDictionaryRegistry class >> _classVersioningPatchClass [

	^ RwGsClassVersioningSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodAdditionPatchClass [

	^ RwGsMethodAdditionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodDeletionPatchClass [

	^ RwGsMethodDeletionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionDeletionPatchClass [

	^ RwGsMethodDeletionExtensionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionPatchClass [

	^ RwGsMethodExtensionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodsDeletionPatchClass [

	^ RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodSourcePatchClass [

	^ RwGsMethodExtensionSessionMethodSourceSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodsPatchClass [

	^ RwGsMethodExtensionSessionMethodSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodsPropertiesPatchClass [

	^ RwGsMethodExtensionSessionMethodPropertiesSymDictPatch

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodPropertiesPatchClass [

	^ RwGsMethodPropertiesSymDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _methodSourcePatchClass [

	^ RwGsMethodSourceSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _packageAdditionPatchClass [

	^ RwGsPackageAdditionSymbolDictPatch
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsSymbolDictionaryRegistry class >> _packageDeletionPatchClass [

	^ RwGsPackageDeletionSymbolDictPatch
]


