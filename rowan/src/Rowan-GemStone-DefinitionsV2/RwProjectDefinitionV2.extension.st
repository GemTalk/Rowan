Extension { #name : 'RwProjectDefinitionV2' }

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> _gemstoneAllUsersName [
	^ RwLoadSpecificationV2 _gemstoneAllUsersName
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	^ self projectSpecification gemstoneDefaultSymbolDictNameForUser: userId
]

{ #category : '*rowan-gemstone-definitionsv2' }
RwProjectDefinitionV2 >> gemstoneSymbolDictNameForPackageNamed: packageName forUser: userId ifAbsent: absentBlock [
	"this method must be preserved until we are no longer loading this code into an image that WAS NOT bootstrapped from .gs files ... it is used during load"

	^ self components
		gemstoneSymbolDictNameForPackageNamed: packageName
		forUser: userId
		ifAbsent: absentBlock
]
