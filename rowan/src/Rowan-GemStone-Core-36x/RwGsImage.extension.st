Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsImage class >> loadedMethod: selector inClassNamed: className isMeta: isMeta ifFound: foundBlock ifAbsent: absentBlock [
	"scan the symbol list for a RwLoadedMethod instances for the given selector in the named class"

	| behavior compiledMethod |
	behavior := self objectNamed: className.
	behavior ifNil: [ ^ absentBlock value ].
	isMeta
		ifTrue: [ behavior := behavior class ].
	compiledMethod := behavior
		compiledMethodAt: selector asSymbol
		environmentId: 0
		otherwise: nil.
	compiledMethod ifNil: [ ^ absentBlock value ].
	^ compiledMethod isFromRowan
		ifTrue: [ foundBlock cull: compiledMethod origin ]
		ifFalse: [ absentBlock value ]
]

{ #category : '*rowan-gemstone-core-36x' }
RwGsImage class >> loadedMethodForMethod: compiledMethod ifAbsent: absentBlock [
	"scan the symbol list for a RwLoadedMethod instances for the given compiled method"

	^ compiledMethod isFromRowan
		ifTrue: [ compiledMethod origin ]
		ifFalse: [ absentBlock value ]
]

{ #category : '*rowan-gemstone-core-36x' }
RwGsImage class >> loadedTraitMethod: selector inTraitNamed: traitName isMeta: isMeta ifFound: foundBlock ifAbsent: absentBlock [
	"scan the symbol list for a RwLoadedMethod instances for the given selector in the named class"

	| trait loadedTrait |
	trait := self objectNamed: traitName.
	trait ifNil: [ ^ absentBlock value ].
	loadedTrait := self loadedTraitForTrait: trait ifAbsent: absentBlock.
	^ isMeta
		ifTrue: [ 
			(loadedTrait loadedClassMethods at: selector asString ifAbsent: [  ])
				ifNil: [ absentBlock value ]
				ifNotNil: [ :loadedMethod | loadedMethod ] ]
		ifFalse: [ 
			(loadedTrait loadedInstanceMethods at: selector asString ifAbsent: [  ])
				ifNil: [ absentBlock value ]
				ifNotNil: [ :loadedMethod | loadedMethod ] ]
]

{ #category : '*rowan-gemstone-core-36x' }
RwGsImage class >> removeLoadedMethodForCompileMethod: compiledMethod [
	"The given compiled method is being removed from the system, remove the loadedMethod associated 
		with the compiled method."

	compiledMethod _origin: nil
]
