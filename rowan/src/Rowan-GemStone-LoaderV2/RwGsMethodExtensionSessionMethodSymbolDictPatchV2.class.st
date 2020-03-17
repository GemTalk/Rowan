Class {
	#name : 'RwGsMethodExtensionSessionMethodSymbolDictPatchV2',
	#superclass : 'RwGsMethodExtensionSymbolDictPatchV2',
	#instVars : [
		'sessionDictsArray',
		'methDict',
		'catDict',
		'pArray'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'compiling' }
RwGsMethodExtensionSessionMethodSymbolDictPatchV2 >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

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
     ex addText: (RwRepositoryResolvedProjectTonelReaderVisitorV2 lineNumberStringForMethod: methodDefinition).
     ex pass
   ]
]

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodSymbolDictPatchV2 >> installMethod [

	"expecting only a single method to be compiled here"

	compiledMethod := self symbolDictionaryRegistry
		addExtensionSessionMethods: methDict
		catDict: catDict
		for: behavior
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.

	selector := compiledMethod selector
]