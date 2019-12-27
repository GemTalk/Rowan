Class {
	#name : 'RwGsMethodExtensionSessionMethodSymbolDictPatch',
	#superclass : 'RwGsMethodExtensionSymbolDictPatch',
	#instVars : [
		'sessionDictsArray',
		'methDict',
		'catDict',
		'pArray'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'compiling' }
RwGsMethodExtensionSessionMethodSymbolDictPatch >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

  [ | sourceString symbolList protocol |
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
   ] on: (CompileError, CompileWarning) do:[:ex |
     ex addText: (RwRepositoryComponentProjectTonelReaderVisitor lineNumberStringForMethod: methodDefinition).
     ex pass
   ]
]

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodSymbolDictPatch >> installMethod [

	"expecting only a single method to be compiled here"

	compiledMethod := self symbolDictionaryRegistry
		addExtensionSessionMethods: methDict
		catDict: catDict
		for: behavior
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation.

	selector := compiledMethod selector

]
