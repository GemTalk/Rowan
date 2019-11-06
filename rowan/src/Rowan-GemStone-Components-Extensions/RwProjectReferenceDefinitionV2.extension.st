Extension { #name : 'RwProjectReferenceDefinitionV2' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> defaultMethodEnv [

	self halt
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> defaultSymbolDictName [

	self halt.
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> defaultUseSessionMethodsForExtensions [

	self halt
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> methodEnvForPackageNamed: packageName [

	| packageProperties |
	packageProperties :=self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultMethodEnv ].
	^ packageProperties at: 'methodEnv' ifAbsent: [ self defaultMethodEnv ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

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
RwProjectReferenceDefinitionV2 >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	| packageProperties |
	packageProperties := self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ self packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	aBool 
		ifNil: [
			"remove the entry if it exists"
			packageProperties removeKey: 'useSessionMethodsForExtensions' ifAbsent: [].
			^self ].
	aBool ifTrue: [ Rowan image ensureSessionMethodsEnabled ].
	packageProperties at: 'useSessionMethodsForExtensions' put: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> symbolDictNameForPackageNamed: packageName [

	| packageProperties |
	packageProperties := self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultSymbolDictName ].
	^ packageProperties
		at: 'symbolDictName'
		ifAbsent: [ ^ self defaultSymbolDictName ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	| packageProperties |
	packageProperties := self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultUseSessionMethodsForExtensions ].
	^ packageProperties
		at: 'useSessionMethodsForExtensions'
		ifAbsent: [ ^ self defaultUseSessionMethodsForExtensions ]
]
