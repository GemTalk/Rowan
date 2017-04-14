"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: compiling
"
compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols

	| className class behavior sourceString symbolList protocol |
	className := classDefinition key asSymbol.
	class := createdClasses at: className
				ifAbsent: 
					[tempSymbols at: className
						ifAbsent: [self error: 'Cannot find class to compile method in.']].
	behavior := isMeta ifTrue: [class class] ifFalse: [class].
	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: 'protocol') asSymbol.
	compiledMethod := behavior
				compileMethod: sourceString
				dictionaries: symbolList
				category: protocol
				intoMethodDict: false
				intoCategories: nil
				intoPragmas: nil
				environmentId: 0