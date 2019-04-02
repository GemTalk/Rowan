Extension { #name : 'RwProjectReferenceDefinition' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> defaultMethodEnv [

self deprecated: 'temporary patch .. this is probably the right spot, but it is worth double checking'.
	^ 0
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> defaultSymbolDictName [

self deprecated: 'temporary patch .. this is probably the right spot, but it is worth double checking'.
	^ 'UserGlobals'
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> methodEnvForPackageNamed: packageName [

	| packageProperties |
self deprecated: 'temporary patch .. see RwGemStoneSpecification>>methodEnvForPackageNamed: for full implementation'.
	packageProperties :=self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultMethodEnv ].
	^ packageProperties at: 'methodEnv' ifAbsent: [ self defaultMethodEnv ]
]

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

	| packageProperties |
self deprecated: 'temporary patch .. see RwGemStoneSpecification>>symbolDictNameForPackageNamed: for full implementation'.
	packageProperties := self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ self defaultSymbolDictName ].
	^ packageProperties
		at: 'symbolDictName'
		ifAbsent: [ ^ self defaultSymbolDictName ]
]
