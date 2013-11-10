"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Comparison-accessing
"
classDefinitionString

	^superclassName, ' subclass: ', self name printString, '
		instVarNames: #(', self instanceVariablesString, ')
		classVars: #(', self classVariablesString, ')
		classInstVars: #(', self classInstanceVariablesString, ')
		poolDictionaries: #(', self poolDictionariesString, ')
		inDictionary: UserGlobals
		options: #()'