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

	| className class sourceString symbolList protocol pragmaDict |
	className := classDefinition key asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to compile method in.' ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ].
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

	compiledMethod := self packageSymbolDictionary
		addExtensionSessionMethods: methDict
		catDict: catDict
		for: behavior
		toPackageNamed: self packageName.

	selector := compiledMethod selector
]
