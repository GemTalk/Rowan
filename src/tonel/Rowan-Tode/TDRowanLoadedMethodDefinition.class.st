Class {
	#name : 'TDRowanLoadedMethodDefinition',
	#superclass : 'TDMethodDefinition',
	#instVars : [
		'theClassDefinition'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> emphasis [

	self loadedClass
		ifNotNil: [ 
			(self loadedClass includesSelector: self selector isMeta: self isMeta)
				ifTrue: [ ^ {(TextEmphasis bold)} ] ].
	self loadedClassExtensions
		do: [ :loadedClassExtension | 
			(loadedClassExtension includesSelector: self selector isMeta: self isMeta)
				ifTrue: [ 
					^ {(TextEmphasis italic).
					(TextEmphasis underlined)} ] ].
	^ {(TextEmphasis normal)}
]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> loadedClass [

	^ self theClassDefinition loadedClass
]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> loadedClassExtensions [

	^ self theClassDefinition loadedClassExtensions
]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> loadedMethod [

	^ Rowan image loadedMethodForMethod: self method
]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> loadedPackage [

	^ self loadedMethod loadedPackage
]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> loadedProject [

	^ self loadedMethod loadedProject
]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> theClassDefinition [

   ^theClassDefinition

]

{ #category : 'accessing' }
TDRowanLoadedMethodDefinition >> theClassDefinition: anObject [

   theClassDefinition := anObject

]
