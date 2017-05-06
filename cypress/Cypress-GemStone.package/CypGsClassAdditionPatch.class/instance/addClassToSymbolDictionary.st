"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
addClassToSymbolDictionary
	"Add the name association to the correct 
        SymbolDictionary in the live SymbolList."

	| dictName dict existingClass |
	dictName := classDefinition propertyAt: 'gs_SymbolDictionary'
				ifAbsent: ['UserGlobals'].
	dict := self newOrExistingSymbolDictionaryNamed: dictName.
	existingClass := dict at: newClass name ifAbsent: [nil].
	existingClass
		ifNotNil: 
			[self
				error: 'Internal error. Attempt to install class whose name is already bound.'].
	dict add: symbolAssociation