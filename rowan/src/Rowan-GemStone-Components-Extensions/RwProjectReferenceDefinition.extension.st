Extension { #name : 'RwProjectReferenceDefinition' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> packageNameToPlatformPropertiesMap [

	^ self properties at: 'packageNameToPlatformPropertiesMap' ifAbsentPut: [ Dictionary new ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	| packageProperties packageNameToPlatformPropertiesMap |
	packageNameToPlatformPropertiesMap := self packageNameToPlatformPropertiesMap.
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	symbolDictName 
		ifNil: [
			"remove the entry if it exists"
			packageProperties removeKey: 'symbolDictName' ifAbsent: [].
			^self ].
	packageProperties at: 'symbolDictName' put: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> symbolDictNameForPackageNamed: packageName [
	| packageProperties  |
self deprecated: 'temporary patch .. see RwGemStoneSpecification>>symbolDictNameForPackageNamed: for full implementation'.
	packageProperties :=  self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^  'UserGlobals' ].
	self halt.

]
