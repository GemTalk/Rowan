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
	"Answer the list of properties that are derived from the trait instance itself"

	self halt.
	^ #(#'name' #'instvars' #'classinstvars' #'classvars' #'comment' #'gs_constraints' #'gs_SymbolDictionary')
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
RwLoadedTrait >> classInstVarNames [

	^self propertyAt: #'classinstvars'
]

{ #category : 'private' }
RwLoadedTrait >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
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

{ #category : 'testing' }
RwLoadedTrait >> isLoadedClass [

	^ false
]

{ #category : 'testing' }
RwLoadedTrait >> isLoadedClassExtension [

	^ false
]

{ #category : 'testing' }
RwLoadedTrait >> isLoadedTrait [

	^ true
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

{ #category : 'accessing' }
RwLoadedTrait >> traitCategory [

	^self propertyAt: #'category' ifAbsent: [ '' ]
]

{ #category : 'properties' }
RwLoadedTrait >> updatePropertiesFromTraitDefinition: traitDefinition [
	"Copy all properties to the receiver's property dictionary that are not recorded in the class directly"

	| recordedPropertyNames |
self halt.
	recordedPropertyNames := self _traitBasedProperties . "a small Array of Symbols"
	traitDefinition properties keysAndValuesDo: [:propertyName :propertyValue |
		(recordedPropertyNames includesIdentical: propertyName)
			ifFalse: [ self propertyAt: propertyName put: propertyValue ] ]
]
