"
No class-specific documentation for CypLoadedMethod, hierarchy is: 
Object
  CypLoadedThing( properties name handle)
    CypLoadedMethod

"
Class {
	#name : 'RwLoadedMethod',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedClass',
		'classIsMeta'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'comparing' }
RwLoadedMethod >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	( #( #'_gsFileOffset' #'_gsFileName' ) includesIdentical: propertyKey)
		ifTrue: [ 
		"_gsFileOffset' '_gsFileName' are informational properties so any differences can be ignored "
		^ true ].
	^ super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue
]

{ #category : 'definitions' }
RwLoadedMethod >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^RwMethodDefinition withProperties: self propertiesForDefinition
		source: self source
]

{ #category : 'accessing' }
RwLoadedMethod >> classIsMeta [

	^classIsMeta
]

{ #category : 'comparing' }
RwLoadedMethod >> compareAgainstBase: aDefinition [ 

	| modification |
	modification := RwMethodModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition);
		sourceModification: (self compareSourceAgainstBase: aDefinition).
	^modification
]

{ #category : 'comparing' }
RwLoadedMethod >> isEmpty [

	^super isEmpty & self source isNil
]

{ #category : 'queries' }
RwLoadedMethod >> loadedClass [

	^loadedClass
]

{ #category : 'accessing' }
RwLoadedMethod >> loadedClass: aLoadedClass [

	loadedClass := aLoadedClass
]

{ #category : 'queries' }
RwLoadedMethod >> loadedPackage [

	^self loadedClass loadedPackage
]

{ #category : 'private' }
RwLoadedMethod >> markPackageDirty [
	"LoadedClass may still be nil if we're creating a new LoadedMethod."

	loadedClass ifNotNil: [loadedClass markPackageDirty]
]

{ #category : 'accessing' }
RwLoadedMethod >> methodCategory [

	^ properties at: #'protocol'

]

{ #category : 'definitions' }
RwLoadedMethod >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: #'selector' put: name.
	^props
]

{ #category : 'accessing' }
RwLoadedMethod >> selector [

	^ properties at: #'selector'
]

{ #category : 'accessing' }
RwLoadedMethod >> source [

	self subclassResponsibility
]
