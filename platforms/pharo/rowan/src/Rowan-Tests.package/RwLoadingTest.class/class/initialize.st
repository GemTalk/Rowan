initlization
initialize
	"self initialize"

	"cannot create useful pool dictionaries on the fly, so need  a set of 
	 canonical pool dictionaries."

	PoolDictionaryDictionary := Dictionary new.
	{(SymbolDictionary new
		at: #'Carrot1' put: 1;
		at: #'Radish1' put: 2;
		name: #'PoolDictionary1';
		yourself).
	(SymbolDictionary new
		at: #'Carrot2' put: 1;
		at: #'Radish2' put: 2;
		name: #'PoolDictionary2';
		yourself).
	(SymbolDictionary new
		at: #'Carrot3' put: 1;
		at: #'Radish3' put: 2;
		name: #'PoolDictionary3';
		yourself)} do: [ :pd | PoolDictionaryDictionary at: pd name put: pd ]