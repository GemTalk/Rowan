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

{ #category : 'private' }
RwLoadedTrait >> _traitBasedProperties [
	"Answer the list of properties that are derived from the class instance itself"

	^ #(#'name')
]

{ #category : 'definitions' }
RwLoadedTrait >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^ RwTraitDefinition
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

	^self propertyAt: #'classinstvars'
]

{ #category : 'private' }
RwLoadedTrait >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'accessing' }
RwLoadedTrait >> classSymbolDictionaryName [

	^self propertyAt: #'gs_SymbolDictionary'

]

{ #category : 'accessing' }
RwLoadedTrait >> classVarNames [

	^self propertyAt: #'classvars'
]

{ #category : 'private' }
RwLoadedTrait >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'comparing' }
RwLoadedTrait >> instanceMethodDefinitionsForCompare [

	^self loadedInstanceMethods
]

{ #category : 'accessing' }
RwLoadedTrait >> instVarNames [

	^self propertyAt: #'instvars'
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

{ #category : 'private' }
RwLoadedTrait >> markPackageDirty [

	loadedPackage markDirty
]

{ #category : 'accessing' }
RwLoadedTrait >> packageName [

	^loadedPackage name
]

{ #category : 'properties' }
RwLoadedTrait >> updatePropertiesFromTraitDefinition: traitDefinition [
	"Copy all properties to the receiver's property dictionary that are not recorded in the class directly"

	| recordedPropertyNames |
	recordedPropertyNames := self _traitBasedProperties . "a small Array of Symbols"
	traitDefinition properties keysAndValuesDo: [:propertyName :propertyValue |
		(recordedPropertyNames includesIdentical: propertyName)
			ifFalse: [ self propertyAt: propertyName put: propertyValue ] ]
]
