"
package: the CypLoadedPackage that defines me and all my methods that are not extension methods.
extensions: a collection of CypLoadedExtendedClasses for this class. From these we can tell which methods don't belong to our default package.
"
Class {
	#name : 'RwLoadedClass',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Rowan-Loader'
}

{ #category : 'private' }
RwLoadedClass >> _classBasedProperties [

	"Answer the list of properties that are derived from the class instance itself"

	^ #()
]

{ #category : 'comparing' }
RwLoadedClass >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [
	propertyKey == #'comment'
		ifTrue: [ 
			propertyValue = baseValue
				ifTrue: [ ^ true ]
				ifFalse: [ 
					"empty or nil comments need to compare equal in GemStone"
					^ (propertyValue == nil or: [ propertyValue isEmpty ])
						and: [ baseValue == nil or: [ baseValue isEmpty ] ] ] ].
	propertyKey == #'classvars'
		ifTrue: [ 
			propertyValue = baseValue
				ifTrue: [ ^ true ]
				ifFalse: [ 
					"order is not relevant when comparing class vars"
					(propertyValue _isArray and: [ baseValue _isArray ])
						ifTrue: [ ^ propertyValue sort = baseValue sort ] ] ].
	^ super
		_compareProperty: propertyKey
		propertyVaue: propertyValue
		againstBaseValue: baseValue
]

{ #category : 'comparing' }
RwLoadedClass >> _modificationClass [

	^ RwClassModification
]

{ #category : 'definitions' }
RwLoadedClass >> asDefinition [
	"Create a Definition corresponding to what I define in the image."
	^RwClassDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions
]

{ #category : 'accessing' }
RwLoadedClass >> classCategory [

	^self propertyAt: #'category' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwLoadedClass >> classClassVarNames [

	^self propertyAt: #'classvars'

]

{ #category : 'accessing' }
RwLoadedClass >> classComment [

	^self propertyAt: #'comment' ifAbsent: ['']

]

{ #category : 'accessing' }
RwLoadedClass >> classInstVarNames [

	^self propertyAt: #'instvars'

]

{ #category : 'private' }
RwLoadedClass >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'comparing' }
RwLoadedClass >> classMethodDefinitionsForCompare [
	^ self loadedClassMethods
]

{ #category : 'accessing' }
RwLoadedClass >> classPoolDictionaries [

	^self propertyAt: #'pools'

]

{ #category : 'accessing' }
RwLoadedClass >> classSuperclass [

	^self propertyAt: #'superclass'

]

{ #category : 'accessing' }
RwLoadedClass >> classSymbolDictionaryName [

	^self propertyAt: #'gs_SymbolDictionary'

]

{ #category : 'testing' }
RwLoadedClass >> includesSelector: aSelector isMeta: isMeta [

	^ isMeta
		ifTrue: [ loadedClassMethods includesKey: aSelector ]
		ifFalse: [ loadedInstanceMethods includesKey: aSelector ]
]

{ #category : 'private' }
RwLoadedClass >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'comparing' }
RwLoadedClass >> instanceMethodDefinitionsForCompare [

	^self loadedInstanceMethods
]

{ #category : 'testing' }
RwLoadedClass >> isClassExtension [

	^ false
]

{ #category : 'testing' }
RwLoadedClass >> isEmpty [

	^loadedInstanceMethods isEmpty and: [loadedClassMethods isEmpty]
]

{ #category : 'testing' }
RwLoadedClass >> isLoadedClass [

	^ true
]

{ #category : 'testing' }
RwLoadedClass >> isLoadedClassExtension [

	^ false
]

{ #category : 'accessing' }
RwLoadedClass >> loadedClassMethods [

	^ loadedClassMethods

]

{ #category : 'accessing' }
RwLoadedClass >> loadedInstanceMethods [ 

	^ loadedInstanceMethods

]

{ #category : 'enumeration' }
RwLoadedClass >> loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock [
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

{ #category : 'testing' }
RwLoadedClass >> loadedMethodAt: aSelector isMeta: isMeta [

	^ isMeta
		ifTrue: [ loadedClassMethods at: aSelector ifAbsent: [nil]]
		ifFalse: [ loadedInstanceMethods at: aSelector ifAbsent: [nil] ]

]

{ #category : 'accessing' }
RwLoadedClass >> loadedPackage [

	^loadedPackage
]

{ #category : 'accessing' }
RwLoadedClass >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
RwLoadedClass >> markPackageDirty [

	loadedPackage markDirty
]

{ #category : 'accessing' }
RwLoadedClass >> packageName [

	^loadedPackage name
]

{ #category : 'other' }
RwLoadedClass >> prettyName [

	^' class ', self name

]

{ #category : 'properties' }
RwLoadedClass >> updatePropertiesFromClassDefinition: classDefinition [
	"Copy all properties to the receiver's property dictionary that are not recorded in the class directly"

	| recordedPropertyNames |
	recordedPropertyNames := self _classBasedProperties . "a small Array of Symbols"
	classDefinition properties keysAndValuesDo: [:propertyName :propertyValue |
		(recordedPropertyNames includesIdentical: propertyName)
			ifFalse: [ self propertyAt: propertyName put: propertyValue ] ]
]
