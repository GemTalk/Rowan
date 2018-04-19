"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageStructure',
	#superclass : 'CypressStructure',
	#instVars : [
		'classes',
		'extensions'
	],
	#category : 'Cypress-Structure'
}

{ #category : 'instance creation' }
CypressPackageStructure class >> fromPackage: aCypressPackageDefinition [

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself

]

{ #category : 'instance creation' }
CypressPackageStructure class >> name: aPackageNameString from: classDefinitions classMap: classMap [
  ^ self new
    name: aPackageNameString from: classDefinitions classMap: classMap;
    yourself

]

{ #category : 'accessing' }
CypressPackageStructure >> classes [

	classes ifNil: [ classes := OrderedCollection new ].
	^classes

]

{ #category : 'accessing' }
CypressPackageStructure >> extensions [

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions

]

{ #category : 'initialization' }
CypressPackageStructure >> fromPackage: aCypressPackageDefinition [
  | snapshot classMap classDefinitions |
  snapshot := aCypressPackageDefinition snapshot.
  classDefinitions := OrderedCollection new.
  classMap := Dictionary new.
  snapshot definitions
    do: [ :definition | 
      definition
        classDefinition: [ :classDefinition | classDefinitions add: classDefinition ]
        methodDefinition: [ :methodDefinition | 
          (classMap at: methodDefinition className ifAbsentPut: [ Set new ])
            add: methodDefinition ] ].
  self
    name: aCypressPackageDefinition name , self packageExtension
    from: classDefinitions
    classMap: classMap

]

{ #category : 'initialization' }
CypressPackageStructure >> name: aString from: classDefinitions classMap: classMap [

	| classStructure |
	name := aString.
	properties := Dictionary new.
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure named: className)
			isClassExtension: true;
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].

]

{ #category : 'accessing' }
CypressPackageStructure >> packageExtension [

	^self packageExtensionOr: ['.package' ]

]

{ #category : 'accessing' }
CypressPackageStructure >> packageExtensionOr: aBlock [

	^self properties at: 'extension' ifAbsent: aBlock

]

{ #category : 'accessing' }
CypressPackageStructure >> packageName [

	| extension extensionSize stopIndex |
	extension := self packageExtension.
	extensionSize := extension size.
	stopIndex := self name
				indexOfSubCollection: extension
				startingAt: self name size - extensionSize + 1
				ifAbsent: [self name size + 1].
	^self name copyFrom: 1 to: stopIndex - 1

]

{ #category : 'accessing' }
CypressPackageStructure >> packageStructure [
	^self

]

{ #category : 'snapshotting' }
CypressPackageStructure >> snapshot [
	| definitions |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	^ CypressSnapshot definitions: definitions

]
