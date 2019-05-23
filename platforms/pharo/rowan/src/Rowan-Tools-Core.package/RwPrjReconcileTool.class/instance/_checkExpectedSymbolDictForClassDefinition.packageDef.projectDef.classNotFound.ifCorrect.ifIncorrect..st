private
_checkExpectedSymbolDictForClassDefinition: classDef packageDef: packageDef projectDef: projectDef classNotFound: classNotFoundBlock ifCorrect: correctBlock ifIncorrect: incorrectBlock
	| class ar symDict expectedSymDictName actualSymDictName |
	class := Rowan globalNamed: classDef name.
	class
		ifNil: [ classNotFoundBlock value: classDef ]
		ifNotNil: [
			ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
			ar size > 1 ifTrue: [self error: 'The class ', classDef name printString, ' is in multiple symbol dictionaries'].
			symDict := ar first at: 1.
			actualSymDictName := symDict name asString.
			expectedSymDictName := projectDef symbolDictNameForPackageNamed: packageDef name.
			expectedSymDictName = actualSymDictName
				ifTrue: [ correctBlock value: classDef value: actualSymDictName ]
				ifFalse: [ incorrectBlock value:  classDef value: expectedSymDictName value: actualSymDictName ] ]
