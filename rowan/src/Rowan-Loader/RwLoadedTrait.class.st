Class {
	#name : 'RwLoadedTrait',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'definitions' }
RwLoadedTrait >> asDefinition [
	"Create a Definition corresponding to what I define in the image."
	^RwClassDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions
]

{ #category : 'accessing' }
RwLoadedTrait >> classCategory [

	^self propertyAt: #'category' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwLoadedTrait >> classInstVarNames [

	^self propertyAt: #'instvars'

]

{ #category : 'private' }
RwLoadedTrait >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'accessing' }
RwLoadedTrait >> classSymbolDictionaryName [

	^self propertyAt: #'gs_SymbolDictionary'

]

{ #category : 'private' }
RwLoadedTrait >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'comparing' }
RwLoadedTrait >> instanceMethodDefinitionsForCompare [

	^self loadedInstanceMethods
]

{ #category : 'testing' }
RwLoadedTrait >> isEmpty [

	^loadedInstanceMethods isEmpty and: [loadedClassMethods isEmpty]
]

{ #category : 'accessing' }
RwLoadedTrait >> loadedClassMethods [

	^ loadedClassMethods

]

{ #category : 'accessing' }
RwLoadedTrait >> loadedInstanceMethods [ 

	^ loadedInstanceMethods

]

{ #category : 'testing' }
RwLoadedTrait >> loadedMethodAt: aSelector isMeta: isMeta [

	^ isMeta
		ifTrue: [ loadedClassMethods at: aSelector ifAbsent: [nil]]
		ifFalse: [ loadedInstanceMethods at: aSelector ifAbsent: [nil] ]

]

{ #category : 'accessing' }
RwLoadedTrait >> loadedPackage [

	^loadedPackage
]

{ #category : 'accessing' }
RwLoadedTrait >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'accessing' }
RwLoadedTrait >> packageName [

	^loadedPackage name
]
