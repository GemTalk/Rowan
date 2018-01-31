Class {
	#name : 'RwGemStoneSpecification',
	#superclass : 'RwPlatformSpecification',
	#instVars : [
		'defaultSymbolDictName',
		'packageNameToSymbolDictNameMap',
		'userId',
		'methodEnv'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultSymbolDictName [

	^ defaultSymbolDictName
]

{ #category : 'accessing' }
RwGemStoneSpecification >> defaultSymbolDictName: anObject [

	defaultSymbolDictName := anObject
]

{ #category : 'initialization' }
RwGemStoneSpecification >> initialize [

	super initialize.
	defaultSymbolDictName := 'UserGlobals'.
	packageNameToSymbolDictNameMap := Dictionary new
]

{ #category : 'accessing' }
RwGemStoneSpecification >> methodEnv [

   ^methodEnv ifNil: [ methodEnv := 0 ]

]

{ #category : 'accessing' }
RwGemStoneSpecification >> methodEnv: anObject [

   methodEnv := anObject

]

{ #category : 'accessing' }
RwGemStoneSpecification >> packageNameToSymbolDictNameMap [

	^ packageNameToSymbolDictNameMap
]

{ #category : 'copying' }
RwGemStoneSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictName [

   ^symbolDictName

]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictName: anObject [

   symbolDictName := anObject

]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictName: symbDictName forPackageNamed: packageName [

	packageNameToSymbolDictNameMap at: packageName put: symbDictName
]

{ #category : 'accessing' }
RwGemStoneSpecification >> symbolDictNameForPackageNamed: packageName [

	^ packageNameToSymbolDictNameMap
		at: packageName
		ifAbsent: [ self defaultSymbolDictName ]
]

{ #category : 'accessing' }
RwGemStoneSpecification >> userId [

   ^userId

]

{ #category : 'accessing' }
RwGemStoneSpecification >> userId: anObject [

   userId := anObject

]
