Class {
	#name : 'RwAbstractClassDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'classMethodDefinitions',
		'instanceMethodDefinitions'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwAbstractClassDefinition class >> withProperties: propertiesDictionary classMethodDefinitions: classMethodDefinitions instanceMethodDefinitions: instanceMethodDefinitions [
  | res |
  propertiesDictionary _validateClass: SymbolDictionary .
	(res := self basicNew)
		properties: propertiesDictionary;
		classMethodDefinitions: classMethodDefinitions;
		instanceMethodDefinitions: instanceMethodDefinitions .
  ^ res
]

{ #category : 'private' }
RwAbstractClassDefinition >> _classNameForCompare: aDefinition [

	^ self key ifNil: [ aDefinition key ]

]

{ #category : 'private' }
RwAbstractClassDefinition >> _methodDefinitionClass [
	^ RwMethodDefinition
]

{ #category : 'private' }
RwAbstractClassDefinition >> _methodsModificationClass [

	self subclassResponsibility: #'__methodsModificationClass'
]

{ #category : 'private' }
RwAbstractClassDefinition >> _modificationClass [

	self subclassResponsibility: #'_modificationClass'
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> addClassMethod: methodSource protocol: protocol [

	^self addClassMethodDefinition: (self _methodDefinitionClass newForSource: methodSource protocol: protocol)
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> addClassMethodDefinition: aMethodDefinition [

	^ self addDefinition: aMethodDefinition to: classMethodDefinitions
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> addInstanceMethod: methodSource protocol: protocol [

	^self addInstanceMethodDefinition: (self _methodDefinitionClass newForSource: methodSource protocol: protocol)
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> addInstanceMethodDefinition: aMethodDefinition [

	^ self addDefinition: aMethodDefinition to: instanceMethodDefinitions
]

{ #category : 'private' }
RwAbstractClassDefinition >> addMethodDefinition: aMethodDefinition to: aDictionary [

	| selector |
	selector := aMethodDefinition propertyAt: #'selector'.
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

{ #category : 'accessing' }
RwAbstractClassDefinition >> classMethodDefinitionsForCompare [
	^ self classMethodDefinitions
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBase: aDefinition [

	^self compareAgainstBase: aDefinition using:  self _modificationClass
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBase: aDefinition using: aModificationClass [

	| modification instanceMethodsModification classMethodsModification className |
	modification := aModificationClass before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	className := self _classNameForCompare: aDefinition.
	instanceMethodsModification := self _methodsModificationClass
		extendedClassName: className.
	classMethodsModification := self _methodsModificationClass
		extendedClassName: className.
	self
		compareDictionary: self instanceMethodDefinitionsForCompare
		againstBaseDictionary: aDefinition instanceMethodDefinitionsForCompare
		into: instanceMethodsModification
		elementClass: self _methodDefinitionClass
		isMeta: false.
	self
		compareDictionary: self classMethodDefinitionsForCompare
		againstBaseDictionary: aDefinition classMethodDefinitionsForCompare
		into: classMethodsModification
		elementClass: self _methodDefinitionClass
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBaseForNewClassUnmanagedVersion: aDefinition [
	"all unchanged and unremoved methods need to be applied to the patch"

	^ self
		compareAgainstBase: aDefinition
		using: RwClassUnmanagedModification
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBaseForNewClassVersion: aDefinition [
	"all unchanged and unremoved methods need to be applied to the patch"

	^ self
		compareAgainstBaseForNewClassVersion: aDefinition
		using: self _modificationClass
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareAgainstBaseForNewClassVersion: aDefinition using: modificationClass [

	"all unchanged and unremoved methods need to be applied to the patch"

	| modification instanceMethodsModification classMethodsModification |
	modification := modificationClass before: aDefinition after: self.
	instanceMethodsModification := self _methodsModificationClass
		extendedClassName: self key.
	classMethodsModification := self _methodsModificationClass
		extendedClassName: self key.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: self _methodDefinitionClass
		isMeta: false.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionaryForNewClassVersion: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: self _methodDefinitionClass
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification
]

{ #category : 'comparing' }
RwAbstractClassDefinition >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta [

	| keys |
	keys := myDictionary keys. "keys is already a copy"
  baseDictionary keysDo:[:k | keys add: k ].  "keys addAll: baseDictionary keys."
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
	keys := myDictionary keys. "keys is already a copy"
  baseDictionary keysDo:[:k | keys add: k ].  "keys addAll: baseDictionary keys."
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

{ #category : 'accessing' }
RwAbstractClassDefinition >> instanceMethodDefinitionsForCompare [
	^ self instanceMethodDefinitions
]

{ #category : 'testing' }
RwAbstractClassDefinition >> isClassExtension [
	^ false

]

{ #category : 'testing' }
RwAbstractClassDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

  ^ instanceMethodDefinitions isEmpty and:[ classMethodDefinitions isEmpty and:[ super isEmpty]]
]

{ #category : 'private' }
RwAbstractClassDefinition >> key [

	^properties at: #'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> moveToPackageNamed: aPackageName packageConvention: aPackageConvention [
	self subclassResponsibility: #'moveToPackageNamed:packageConvention:'
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> name: aString [

	properties at: #'name' put: aString

]

{ #category : 'copying' }
RwAbstractClassDefinition >> postCopy [

	| oldDefs |
	super postCopy.
	oldDefs := classMethodDefinitions.
	classMethodDefinitions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | classMethodDefinitions at: key put: value copy ].
	oldDefs := instanceMethodDefinitions.
	instanceMethodDefinitions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | instanceMethodDefinitions at: key put: value copy ].
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> removeClassMethod: selector [
	^ self
		removeClassMethod: selector
		ifAbsent: [ self error: 'No method definition for ' , selector printString , ' found.' ]
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> removeClassMethod: selector ifAbsent: absentBlock [
	| definition |
	definition := classMethodDefinitions
		at: selector
		ifAbsent: [ absentBlock value ].
	self removeDefinition: definition from: classMethodDefinitions.
	^ definition
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> removeInstanceMethod: selector [
	^ self
		removeInstanceMethod: selector
		ifAbsent: [ self error: 'No method definition for ' , selector printString , ' found.' ]
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> removeInstanceMethod: selector ifAbsent: absentBlock [
	| definition |
	definition := instanceMethodDefinitions
		at: selector
		ifAbsent: [ ^ absentBlock value ].
	self removeDefinition: definition from: instanceMethodDefinitions.
	^ definition
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> updateClassMethodDefinition: aMethodDefinition [

	self updateDefinition: aMethodDefinition in: classMethodDefinitions
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> updateInstanceMethod: methodSource protocol: protocol [

	^self updateInstanceMethodDefinition: (self _methodDefinitionClass newForSource: methodSource protocol: protocol)
]

{ #category : 'accessing' }
RwAbstractClassDefinition >> updateInstanceMethodDefinition: aMethodDefinition [

	self updateDefinition: aMethodDefinition in: instanceMethodDefinitions
]
