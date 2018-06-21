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
RwPrjReconcileTool >> reconcileProjectDefinitionSet: projectDefinitionSet [
	| projectTools move stay missing wrong |
	projectTools := Rowan projectTools.

	move := Array new.
	stay := Array new.
	missing := Array new.
	wrong := Array new.

	projectDefinitionSet projects do: [:projectDef |
		| visitor spec visitedConfigurationMap packageToConfigurationMap |
		spec := projectDef specification.
		visitor := projectTools read 
			_visitConfigurations: RwProjectLoadConfigurationAnalysisVisitor 
			forProjectDefinition: projectDef
			withConfigurations: spec defaultConfigurationNames 
			groupNames: spec defaultGroupNames.
		visitedConfigurationMap := visitor visitedConfigurationMap.
		packageToConfigurationMap  := visitor packageToConfigurationMap.
		projectDef packages values do: [:packageDef |
			packageDef classDefinitions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | missing add: theClassDef ] 
					ifCorrect: [:theClassDef | "ignored"] 
					ifInCorrect: [:theClassDef :expectedSymDictName :actualSymDictName | wrong add: theClassDef -> {expectedSymDictName. actualSymDictName } ] ].
			packageDef classExtensions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | missing add: theClassDef ] 
					ifCorrect: [:theClassDef | stay add: theClassDef] 
					ifInCorrect: [:theClassDef :expectedSymDictName :actualSymDictName | move add: theClassDef ] ] ] ].

	^{ projectDefinitionSet. move . stay. missing . wrong }

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileProjectFromSpecUrl: specUrl [
	| projectDefinition projectDefinitionSet projectTools loadedProjectInfo |
	projectTools := Rowan projectTools.
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrl.
	projectDefinitionSet := projectTools read readProjectSetForProjectDefinition: projectDefinition.

	loadedProjectInfo := projectDefinitionSet properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			"install the packageMapSpecs for this load into the specification prior to the reconcile"
			(projectDefinitionSet projectNamed: projectName ifAbsent: [])
				updateGsPlatformSpecLoadedProjectInfo: projectInfo ].

	^ self reconcileProjectDefinitionSet: projectDefinitionSet

]
