Extension { #name : 'SymbolDictionary' }

{ #category : '*rowan-gemstone-kernel' }
SymbolDictionary >> rowanSymbolDictionaryRegistry [

	"answer the RwGsSymbolDictionaryRegistry instance installed in the receiver, otherwise answer nil"

	^ self at: #'RwSymbolDictionaryRegistry' ifAbsent: [  ]

]
