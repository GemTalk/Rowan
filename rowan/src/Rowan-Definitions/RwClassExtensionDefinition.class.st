"
Extends an existing class with additional methods.
Properties must include 'name', the name of the class to be extended, but should not include other things like 'super' or 'instvars'.

"
Class {
	#name : 'RwClassExtensionDefinition',
	#superclass : 'RwAbstractClassDefinition',
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwClassExtensionDefinition class >> newForClassNamed: className [

	| propertiesDictionary |
	propertiesDictionary := Dictionary new
		at: 'name' put: className;
		yourself.
	^ self
		withProperties: propertiesDictionary
		classMethodDefinitions: Dictionary new
		instanceMethodDefinitions: Dictionary new
]

{ #category : 'private' }
RwClassExtensionDefinition >> _classNameForCompare: aDefinition [

	^ self key ifNil: 
		[ | cName |
		cName := aDefinition key.
		self name: cName.
		cName ]

]

{ #category : 'comparing' }
RwClassExtensionDefinition >> compareAgainstBase: aDefinition [

	| modification |
	modification := super compareAgainstBase: aDefinition.
	(modification after instanceMethodDefinitions isEmpty and: [ modification after classMethodDefinitions isEmpty ]) 
		ifTrue: [ 
			"class extension after modification is empty, so the receiver should be deleted"
			self yourself ].
	^ modification
]

{ #category : 'comparing' }
RwClassExtensionDefinition >> compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta [

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys
		do: [ :key | 
			| before after modification |
			before := baseDictionary at: key ifAbsent: [ elementClass new ].
			after := myDictionary at: key ifAbsent: [ elementClass new ].
			modification := after compareExtensionMethodsAgainstBase: before.
			modification
				isMeta: isClassMeta;
				classDefinition: self.
			modification isEmpty
				ifFalse: [ anElementsModification addElementModification: modification ] ]
]

{ #category : 'testing' }
RwClassExtensionDefinition >> isClassExtension [
	^ true

]
