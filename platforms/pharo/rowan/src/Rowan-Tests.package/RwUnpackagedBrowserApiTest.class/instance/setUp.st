running
setUp
	"test class will be removed along with symbol dictionary during teardown"

	| symDict |
	super setUp.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	Object subclass: self _testClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #()