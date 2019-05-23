tests
testRowanLoaderSystemDictionaryClone

	| rowanLoaderSymbolDict clonedSymDictName clonedSymbolDict |
	rowanLoaderSymbolDict := Rowan globalNamed: 'RowanLoader'.
	clonedSymDictName := #'RowanLoader_cloned'.
	clonedSymbolDict := rowanLoaderSymbolDict 
		_rowanCloneSymbolDictionaryNamed: clonedSymDictName 
		symbolList: GsCurrentSession currentSession symbolList.
