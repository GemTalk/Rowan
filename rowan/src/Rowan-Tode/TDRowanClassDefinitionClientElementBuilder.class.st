Class {
	#name : 'TDRowanClassDefinitionClientElementBuilder',
	#superclass : 'TDClassDefinitionClientElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'as yet unclassified' }
TDRowanClassDefinitionClientElementBuilder >> elementSource [

	(self object isKindOf: TDRowanLoadedClassDefinition)
		ifTrue: [ 
			^ Rowan projectTools browser
				classCreationTemplateForClass: self theClass
				hybridBrowser: true ].
	^ self theClass definition
]
