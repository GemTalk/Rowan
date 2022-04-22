"
No class-specific documentation for CypLoadedClassExtension, hierarchy is: 
Object
  CypLoadedThing( properties name handle)
    CypLoadedClassExtension( loadedPackage loadedMethods)

"
Class {
	#name : 'RwLoadedClassExtension',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'definitions' }
RwLoadedClassExtension >> asDefinition [
	"Create a Definition corresponding to what I define in the image."
	^RwClassExtensionDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions
]

{ #category : 'private' }
RwLoadedClassExtension >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'private' }
RwLoadedClassExtension >> classMethodDefinitionsForCompare [
	^ self loadedClassMethods
]

{ #category : 'testing' }
RwLoadedClassExtension >> includesSelector: aSelector isMeta: isMeta [

	^ isMeta
		ifTrue: [ loadedClassMethods includesKey: aSelector ]
		ifFalse: [ loadedInstanceMethods includesKey: aSelector ]
]

{ #category : 'private' }
RwLoadedClassExtension >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'private' }
RwLoadedClassExtension >> instanceMethodDefinitionsForCompare [
	^ self loadedInstanceMethods
]

{ #category : 'testing' }
RwLoadedClassExtension >> isClassExtension [

	^ true
]

{ #category : 'testing' }
RwLoadedClassExtension >> isEmpty [

	^loadedInstanceMethods isEmpty and: [loadedClassMethods isEmpty]
]

{ #category : 'testing' }
RwLoadedClassExtension >> isLoadedClass [

	^ false
]

{ #category : 'testing' }
RwLoadedClassExtension >> isLoadedClassExtension [

	^ true
]

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedClassMethods [

	^ loadedClassMethods

]

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedInstanceMethods [ 

	^ loadedInstanceMethods

]

{ #category : 'enumeration' }
RwLoadedClassExtension >> loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock [
	loadedInstanceMethods
		valuesDo: [ :loadedMethod | 
			loadedInstanceMethodBlock
				cull: self loadedProject
				cull: self loadedPackage
				cull: self
				cull: loadedMethod ].
	loadedClassMethods
		valuesDo: [ :loadedMethod | 
			loadedClassMethodBlock
				cull: self loadedProject
				cull: self loadedPackage
				cull: self
				cull: loadedMethod ]
]

{ #category : 'private' }
RwLoadedClassExtension >> loadedMethodAt: aSelector isMeta: isMeta [

	^ isMeta
		ifTrue: [ loadedClassMethods at: aSelector ifAbsent: [nil]]
		ifFalse: [ loadedInstanceMethods at: aSelector ifAbsent: [nil] ]
]

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedPackage [

	^loadedPackage
]

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
RwLoadedClassExtension >> markPackageDirty [

	loadedPackage markDirty
]

{ #category : 'accessing' }
RwLoadedClassExtension >> packageName [

	^loadedPackage name
]
