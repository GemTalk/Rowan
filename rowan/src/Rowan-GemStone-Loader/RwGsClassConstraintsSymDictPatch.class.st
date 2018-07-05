Class {
	#name : 'RwGsClassConstraintsSymDictPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'actions' }
RwGsClassConstraintsSymDictPatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'installing' }
RwGsClassConstraintsSymDictPatch >> installPropertiesPatchFor: aPatchSet [

	" update class and update loadedClass with new constraints"

	| className existingClass createdClass |
	className := classDefinition key asSymbol.
	existingClass := aPatchSet createdClasses
		at: className
		ifAbsent: [ 
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to update properties for.' ] ].
self error: 'stop here'. "instead of calling createClassFor: ... do update the constraints directly adding and removing as necessary ..."
"still need to worry about how constraints will be handled for new class versions"
	createdClass := self createClassFor: aPatchSet.	"use createClassFor:, but not expected to create new class version"
	createdClass == existingClass
		ifFalse: [ 
			self
				error:
					'internal error - class changed during class constraint update ... should have been a class versioning patch' ].
	self symbolDictionaryRegistry updateClassProperties: existingClass implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]
