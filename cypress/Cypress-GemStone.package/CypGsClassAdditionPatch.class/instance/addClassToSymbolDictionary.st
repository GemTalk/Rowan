"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: actions
"
addClassToSymbolDictionary
	"Add the name association to the correct 
        SymbolDictionary in the live SymbolList."

	| dictName dict existingAssociation |
	dictName := classDefinition propertyAt: 'gs_SymbolDictionary'
				ifAbsent: ['UserGlobals'].
	dict := self newOrExistingSymbolDictionaryNamed: dictName.
	existingAssociation := dict associationAt: newClass name ifAbsent: [nil].
	existingAssociation
		ifNotNil: 
			[self
				error: 'Internal error. Attempt to install class whose name is already bound.'].
	symbolAssociation value: newClass.
	dict add: symbolAssociation