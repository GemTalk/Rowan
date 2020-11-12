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
	compiledMethod _rowanPackageInfo
		ifNotNil: [ :loadedMethod | ^ loadedMethod ]
		ifNil: [ 
			"old-fashioned lookup needed until all uses of methodRegistry replaced by  _rowanPackageInfo lookup"
			self symbolList
				do: [ :symbolDict | 
					symbolDict rowanSymbolDictionaryRegistry
						ifNotNil: [ :registry | 
							(registry methodRegistry at: compiledMethod ifAbsent: [  ])
								ifNotNil: [ :loadedMethod | ^ foundBlock value: loadedMethod ] ] ].
			^ absentBlock value ]
]

{ #category : '*rowan-gemstone-core-36x' }
RwGsImage class >> loadedMethodForMethod: compiledMethod ifAbsent: absentBlock [
	"scan the symbol list for a RwLoadedMethod instances for the given compiled method"

	compiledMethod _rowanPackageInfo
		ifNotNil: [ :loadedMethod | ^ loadedMethod ]
		ifNil: [ 
			"old-fashioned lookup needed until all uses of methodRegistry replaced by  _rowanPackageInfo lookup"
			self symbolList
				do: [ :symbolDict | 
					symbolDict rowanSymbolDictionaryRegistry
						ifNotNil: [ :registry | 
							(registry methodRegistry at: compiledMethod ifAbsent: [  ])
								ifNotNil: [ :loadedMethod | ^ loadedMethod ] ] ].
			^ absentBlock value ]
]

{ #category : '*rowan-gemstone-core-36x' }
RwGsImage class >> removeLoadedMethodForCompileMethod: compiledMethod [
	"The given compiled method is being removed from the system, remove the loadedMethod associated 
		with the compiled method."

	compiledMethod _rowanPackageInfo
		ifNotNil: [ :loadedMethod | compiledMethod _rowanPackageInfo: nil ]
		ifNil: [ 
			"old-fashioned lookup needed until all uses of methodRegistry replaced by  _rowanPackageInfo lookup"
			self symbolList
				do: [ :symbolDict | 
					symbolDict rowanSymbolDictionaryRegistry
						ifNotNil: [ :registry | registry methodRegistry removeKey: compiledMethod ifAbsent: [  ] ] ] ]
]
