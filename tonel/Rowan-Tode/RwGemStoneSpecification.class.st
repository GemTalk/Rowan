Class {
	#name : 'RwGemStoneSpecification',
	#superclass : 'RwPlatformSpecification',
	#instVars : [
		'defaultMethodEnv',
		'defaultSymbolDictName',
		'defaultUserId',
		'packageNameToPlatformPropertiesMap'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultMethodEnv [

	^ defaultMethodEnv ifNil: [ defaultMethodEnv := 0 ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultMethodEnv: anObject [

	defaultMethodEnv := anObject
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultSymbolDictName [

	^ defaultSymbolDictName ifNil: [ 'UserGlobals' ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultSymbolDictName: anObject [

	defaultSymbolDictName := anObject
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultUserId [

	^ defaultUserId ifNil: [ defaultUserId := RwGsImage currentUserId ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultUserId: anObject [

	defaultUserId := anObject
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

{ #category : 'copying' }
RwGemStoneSpecification >> postCopy [

	"don't forget to copy any compound state objects"

	super postCopy.
	packageNameToPlatformPropertiesMap := packageNameToPlatformPropertiesMap copy.
	packageNameToPlatformPropertiesMap
		keysAndValuesDo: [ :key :value | packageNameToPlatformPropertiesMap at: key put: value copy ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictName: symbolDictName forPackageNamed: packageName [

	| packageProperties |
	packageProperties := packageNameToPlatformPropertiesMap
		at: packageName
		ifAbsent: [ packageNameToPlatformPropertiesMap at: packageName put: Dictionary new ].
	packageProperties at: 'symbolDictName' put: symbolDictName
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
