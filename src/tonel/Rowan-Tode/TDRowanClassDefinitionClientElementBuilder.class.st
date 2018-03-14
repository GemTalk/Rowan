Class {
	#name : 'TDRowanClassDefinitionClientElementBuilder',
	#superclass : 'TDClassDefinitionClientElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'as yet unclassified' }
TDRowanClassDefinitionClientElementBuilder >> elementSource [

	(self object isKindOf: TDRowanLoadedClassDefinition)
		ifTrue: [ 
			| def |
			def := self object.
			^ def
				classDefinitionFor: self theClass
				inContext: System myUserProfile
				superClassName: self theClass superClass name asString ].
	^ self theClass definition
]
