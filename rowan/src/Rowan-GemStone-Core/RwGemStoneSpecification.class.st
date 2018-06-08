Class {
	#name : 'RwGemStoneSpecification',
	#superclass : 'RwPlatformSpecification',
	#instVars : [
		'defaultMethodEnv',
		'defaultSymbolDictName',
		'defaultUserId',
		'defaultUseSessionMethodsForExtensions',
		'packageNameToPlatformPropertiesMap',
		'projectOwnerId'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultMethodEnv [

	^ defaultMethodEnv ifNil: [ defaultMethodEnv := 0 ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultMethodEnv: anInteger [

	defaultMethodEnv := anInteger
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultSymbolDictName [

	^ defaultSymbolDictName ifNil: [ 'UserGlobals' ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultSymbolDictName: aString [

	defaultSymbolDictName := aString
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultUserId [

	^ defaultUserId ifNil: [ defaultUserId := Rowan image currentUserId ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultUserId: aString [

	defaultUserId := aString
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultUseSessionMethodsForExtensions [

	^ defaultUseSessionMethodsForExtensions
		ifNil: [ defaultUseSessionMethodsForExtensions := false ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultUseSessionMethodsForExtensions: aBool [

	defaultUseSessionMethodsForExtensions := aBool
]

{ #category : 'initialization' }
RwGemStoneSpecification >> initialize [

	super initialize.
	defaultSymbolDictName := 'UserGlobals'.
	packageNameToPlatformPropertiesMap := Dictionary new
]

{ #category : 'accessing' }
RwGemStoneSpecification >> methodEnv: methodEnv forPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	packageProperties at: 'methodEnv' put: methodEnv
]

{ #category : 'accessing' }
RwGemStoneSpecification >> methodEnvForPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultMethodEnv ].
	^ packageProperties at: 'methodEnv' ifAbsent: [ ^ self defaultMethodEnv ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> packageNameToPlatformPropertiesMap [

	^ packageNameToPlatformPropertiesMap
]

{ #category : 'accessing' }
RwGemStoneSpecification >> packageNameToPlatformPropertiesMap: aDictionary [

	packageNameToPlatformPropertiesMap := aDictionary

]

{ #category : 'copying' }
RwGemStoneSpecification >> postCopy [

	"don't forget to copy any compound state objects"

	super postCopy.
	packageNameToPlatformPropertiesMap := packageNameToPlatformPropertiesMap copy.
	packageNameToPlatformPropertiesMap
		keysAndValuesDo: [ :key :value | packageNameToPlatformPropertiesMap at: key put: value copy ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> projectOwnerId [

	^ projectOwnerId ifNil: [ Rowan image currentUserId ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> projectOwnerId: anObject [

   projectOwnerId := anObject

]

{ #category : 'accessing' }
RwGemStoneSpecification >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	| packageProperties |
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

{ #category : 'accessing' }
RwGemStoneSpecification >> setUseSessionMethodsForExtensions: aBool forPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	aBool 
		ifNil: [
			"remove the entry if it exists"
			packageProperties removeKey: 'useSessionMethodsForExtensions' ifAbsent: [].
			^self ].
	packageProperties at: 'useSessionMethodsForExtensions' put: aBool

]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictNameForPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultSymbolDictName ].
	^ packageProperties
		at: 'symbolDictName'
		ifAbsent: [ ^ self defaultSymbolDictName ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> userId: userId forPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	packageProperties at: 'userId' put: userId
]

{ #category : 'accessing' }
RwGemStoneSpecification >> userIdForPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultUserId ].
	^ packageProperties at: 'userId' ifAbsent: [ ^ self defaultUserId ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> useSessionMethodsForExtensionsForPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ ^ self defaultUseSessionMethodsForExtensions ].
	^ packageProperties
		at: 'useSessionMethodsForExtensions'
		ifAbsent: [ ^ self defaultUseSessionMethodsForExtensions ]
]
