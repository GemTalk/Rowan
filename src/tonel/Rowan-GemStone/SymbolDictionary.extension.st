Extension { #name : 'SymbolDictionary' }

{ #category : '*rowan-gemstone' }
SymbolDictionary >> rowanSymbolDictionaryRegistry [

	"answer the RwGsSymbolDictionaryRegistry instance installed in the receiver, otherwise answer nil"

	^ self at: RwGsSymbolDictionaryRegistry globalSymbol ifAbsent: [  ]
]
