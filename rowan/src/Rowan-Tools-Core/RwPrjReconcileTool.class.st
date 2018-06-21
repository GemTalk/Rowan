Class {
	#name : 'RwPrjReconcileTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReconcileTool >> _checkExpectedSymbolDictForClassDefinition: classDef packageDef: packageDef projectDef: projectDef classNotFound: classNotFoundBlock ifCorrect: correctBlock ifInCorrect: incorrectBlock [
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
				ifTrue: [ correctBlock value: classDef ]
				ifFalse: [ incorrectBlock value:  classDef value: expectedSymDictName value: actualSymDictName ] ]

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileProjectFromSpecUrl: specUrl [
	| projectDefinition projectSet projectTools spec visitor  visitedConfigurationMap packageToConfigurationMap move stay missing wrong |
	projectTools := Rowan projectTools.
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrl.
	spec := projectDefinition specification.
	projectSet := projectTools read readProjectSetForProjectDefinition: projectDefinition.
	visitor := projectTools read 
		_visitConfigurations: RwProjectLoadConfigurationAnalysisVisitor 
		forProjectDefinition: projectDefinition 
		withConfigurations: spec defaultConfigurationNames 
		groupNames: spec defaultGroupNames.
	visitedConfigurationMap := visitor visitedConfigurationMap.
	packageToConfigurationMap  := visitor packageToConfigurationMap.

	move := Array new.
	stay := Array new.
	missing := Array new.
	wrong := Array new.

	projectSet projects do: [:projectDef |
		projectDef packages values do: [:packageDef |
			packageDef classDefinitions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | missing add: theClassDef ] 
					ifCorrect: [:theClassDef :expectedSymDictName | "ignored"] 
					ifInCorrect: [:theClassDef :expectedSymDictName :actualSymDictName | wrong add: theClassDef ] ].
			packageDef classExtensions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | missing add: theClassDef ] 
					ifCorrect: [:theClassDef :expectedSymDictName | stay add: theClassDef] 
					ifInCorrect: [:theClassDef :expectedSymDictName :actualSymDictName | move add: theClassDef ] ] ] ].

	^{projectSet . visitor . move . stay. missing . wrong }

]
