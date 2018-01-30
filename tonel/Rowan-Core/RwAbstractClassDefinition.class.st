Class {
	#name : 'RwAbstractClassDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'classMethodDefinitions',
		'instanceMethodDefinitions'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwAbstractClassDefinition class >> withProperties: propertiesDictionary classMethodDefinitions: classMethodDefinitions instanceMethodDefinitions: instanceMethodDefinitions [

	^(self basicNew)
		properties: propertiesDictionary;
		classMethodDefinitions: classMethodDefinitions;
		instanceMethodDefinitions: instanceMethodDefinitions;
		yourself
]

{ #category : 'private' }
RwAbstractClassDefinition >> _modificationClass [

	self subclassResponsibility: #'_modificationClass'
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> addClassMethodDefinition: aMethodDefinition [

	self addDefinition: aMethodDefinition to: classMethodDefinitions
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> addInstanceMethodDefinition: aMethodDefinition [

	self addDefinition: aMethodDefinition to: instanceMethodDefinitions
]

{ #category : 'private' }
RwAbstractClassDefinition >> addMethodDefinition: aMethodDefinition to: aDictionary [

	| selector |
	selector := aMethodDefinition propertyAt: 'selector'.
	(aDictionary includesKey: selector)
		ifTrue: [self error: 'Duplicate selector in behavior'].
	aDictionary at: selector put: aMethodDefinition
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> classMethodDefinitions [

	^classMethodDefinitions
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> classMethodDefinitions: dictionaryOfDefinitions [

	classMethodDefinitions := dictionaryOfDefinitions
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBase: aDefinition [

	| modification instanceMethodsModification classMethodsModification |
	modification := self _modificationClass before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	instanceMethodsModification := RwMethodsModification new.
	classMethodsModification := RwMethodsModification new.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionary: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: RwMethodDefinition
		isMeta: false.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionary: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: RwMethodDefinition
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^modification
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBaseForNewClassVersion: aDefinition [

	"all unchanged and unremoved methods need to be applied to the patch"

	| modification instanceMethodsModification classMethodsModification |
	modification := self _modificationClass before: aDefinition after: self.
	instanceMethodsModification := RwMethodsModification new.
	classMethodsModification := RwMethodsModification new.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: RwMethodDefinition
		isMeta: false.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: RwMethodDefinition
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta [

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after modification |
			before := baseDictionary at: key ifAbsent: [elementClass new].
			after := myDictionary at: key ifAbsent: [elementClass new].
			modification := after compareAgainstBase: before.
			modification 
				isMeta: isClassMeta;
				classDefinition: self.
			modification isEmpty
				ifFalse: [anElementsModification addElementModification: modification]]
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareDictionary: myDictionary againstBaseDictionaryForNewClassVersion: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta [

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys
		do: [ :key | 
			(baseDictionary
				at: key
				ifAbsent: [ 
					"additions do not apply for new class version - the original modification should not be changed"
					 ])
				ifNotNil: [ :before | 
					(myDictionary
						at: key
						ifAbsent: [ 
							"deletions do not apply for new class version - the original modification should not be changed"
							 ])
						ifNotNil: [ :after | 
							| modification |
							modification := after compareAgainstBaseForNewClassVersion: before.
							modification
								isMeta: isClassMeta;
								classDefinition: self.
							modification isEmpty
								ifFalse: [ anElementsModification addElementModification: modification ] ] ] ]
]

{ #category : 'initialization' }
RwAbstractClassDefinition >> initialize [

	super initialize.
	classMethodDefinitions := Dictionary new.
	instanceMethodDefinitions := Dictionary new
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> instanceMethodDefinitions [

	^instanceMethodDefinitions
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> instanceMethodDefinitions: dictionaryOfDefinitions [

	instanceMethodDefinitions := dictionaryOfDefinitions
]

{ #category : 'testing' }
RwAbstractClassDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & classMethodDefinitions isEmpty
		& instanceMethodDefinitions isEmpty
]

{ #category : 'private' }
RwAbstractClassDefinition >> key [

	^properties at: 'name' ifAbsent: [nil]
]
