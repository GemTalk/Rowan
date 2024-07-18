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
RwGsMethodExtensionSessionMethodSymbolDictPatchV2 >> compileUsingNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	self
		primeBehaviorNewClassesSymbolList: createdClasses
		andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			self
				error:
					'Class ' , self className printString , ' not found in the symbol dictionary '
						, self symbolDictionaryName printString , ' associated with the method '
						, methodDefinition selector printString ].

	[ 
	| sourceString protocol symbolList |
	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: #'protocol') asSymbol.

	methDict := GsMethodDictionary new.
	catDict := GsMethodDictionary new.
	compiledMethod := behavior
		compileMethod: sourceString
		dictionaries: symbolList
		category: protocol
		intoMethodDict: methDict
		intoCategories: catDict
		environmentId: self methodEnvironmentId ]
		on: CompileError , CompileWarning
		do: [ :ex | 
			ex
				addText:
					(RwRepositoryComponentProjectReaderVisitor
						lineNumberStringForDefinition: methodDefinition).
			ex pass ]
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
