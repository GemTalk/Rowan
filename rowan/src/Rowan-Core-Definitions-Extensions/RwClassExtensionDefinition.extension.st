Extension { #name : 'RwClassExtensionDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwClassExtensionDefinition >> _methodsModificationClass [

	^ RwExtensionMethodsModification
]

{ #category : '*rowan-core-definitions-extensions' }
RwClassExtensionDefinition >> _modificationClass [

	^ RwClassExtensionModification
]

{ #category : '*rowan-core-definitions-extensions' }
RwClassExtensionDefinition >> comparePropertiesAgainstBase: aDefinition [
	"property differences aren't of interest for a class extension ..."

	^ RwPropertiesModification new
]
