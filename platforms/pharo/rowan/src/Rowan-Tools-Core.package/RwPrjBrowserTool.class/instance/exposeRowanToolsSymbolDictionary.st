project browsing
exposeRowanToolsSymbolDictionary

	| transient size dict |
	transient := Rowan image symbolList.
	size := transient size.
	dict := (AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools'.
	transient insertObject: dict at: size + 1.
