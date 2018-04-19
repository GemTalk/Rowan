Class {
	#name : 'RwGsMethodExtensionSessionMethodSymbolDictPatch',
	#superclass : 'RwGsMethodExtensionSymbolDictPatch',
	#instVars : [
		'sessionDictsArray',
		'methDict',
		'catDict',
		'pArray'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'compiling' }
RwGsMethodExtensionSessionMethodSymbolDictPatch >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

	| sourceString symbolList protocol |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: 'protocol') asSymbol.

	methDict := GsMethodDictionary new.
	catDict := GsMethodDictionary new.
	compiledMethod := behavior
		compileMethod: sourceString
		dictionaries: symbolList
		category: protocol
		intoMethodDict: methDict
		intoCategories: catDict
		intoPragmas: pArray
		environmentId: self methodEnvironmentId
]

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodSymbolDictPatch >> installMethod [

	"expecting only a single method to be compiled here"

	compiledMethod := self symbolDictionaryRegistry
		addExtensionSessionMethods: methDict
		catDict: catDict
		for: behavior
		toPackageNamed: self packageName.

	selector := compiledMethod selector
]
