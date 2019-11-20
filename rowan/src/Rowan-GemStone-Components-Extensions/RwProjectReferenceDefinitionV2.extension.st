Extension { #name : 'RwProjectReferenceDefinitionV2' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> defaultMethodEnv [
	^ self projectLoadSpecification gemstoneDefaultMethodEnvForUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> defaultSymbolDictName [
	^ self projectLoadSpecification
		gemstoneDefaultSymbolDictNameForUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> defaultUseSessionMethodsForExtensions [
	^ self projectLoadSpecification gemstoneDefaultUseSessionMethodsForExtensionsForUser: Rowan image currentUserId
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetDefaultMethodEnvForUser: userId to: aBool [
	^ self projectLoadSpecification
		gemstoneSetDefaultMethodEnvForUser: userId
		to: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetDefaultMethodEnvTo: aBool [
	^ self projectLoadSpecification gemstoneSetDefaultMethodEnvTo: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [
	^ self projectLoadSpecification gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	^ self projectLoadSpecification gemstoneSetDefaultSymbolDictNameTo: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool [
	^ self projectLoadSpecification gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool [
	^ self projectLoadSpecification gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetMethodEnv: env forPackageNamed: packageName [

	| packageProperties packageNameToPlatformPropertiesMap |
	packageNameToPlatformPropertiesMap := self packageNameToPlatformPropertiesMap.
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	env 
		ifNil: [
			"remove the entry if it exists"
			packageProperties removeKey: 'methodEnv' ifAbsent: [].
			^self ].
	packageProperties at: 'methodEnv' put: env
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinitionV2 >> gemstoneSetUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

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
RwProjectReferenceDefinitionV2 >> methodEnvForPackageNamed: packageName [

	| packageProperties |
	packageProperties := self packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultMethodEnv ].
	^ packageProperties
		at: 'methodEnv'
		ifAbsent: [ ^ self defaultMethodEnv ]
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
