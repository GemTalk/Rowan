Class {
	#name : 'RwPrjReconcileTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjReconcileTool >> _checkExpectedSymbolDictForClassDefinition: classDef packageDef: packageDef projectDef: projectDef classNotFound: classNotFoundBlock ifCorrect: correctBlock ifIncorrect: incorrectBlock [
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

{ #category : 'private' }
RwPrjReconcileTool >> _moveClassExtension: theClassDef toNewPackageClonedFrom: packageDef inSymbolDictionary: expectedSymDictName inProject: projectDef [

	| newPackageName newPackageDef |
	newPackageName := packageDef name, '_ext_', expectedSymDictName.
	newPackageDef := projectDef 
		packageNamed: newPackageName 
		ifAbsent: [ 
			| pDef |
			pDef := RwPackageDefinition newNamed: newPackageName.
			projectDef addPackage: pDef.
			pDef ].
	packageDef removeClassExtension: theClassDef.
	newPackageDef addClassExtension: theClassDef

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileProjectDefinitionSet: projectDefinitionSet [
	| projectTools movedSpecs |
	projectTools := Rowan projectTools.

	movedSpecs := Array new.
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
					classNotFound: [:theClassDef  | self error: 'The class ', theClassDef name printString, ' not found' ] 
					ifCorrect: [:theClassDef | "ignored" ] 
					ifIncorrect: [:theClassDef :expectedSymDictName :actualSymDictName | 
						self error: 'The class ', 
							theClassDef name printString, 
							' is in the wrong symbol dictionary. It is in the ', 
							actualSymDictName printString, 
							' symbol dictionary. It is expected to be in the ', 
								expectedSymDictName printSring, 
							' symbol dictioanry.'] ].
			packageDef classExtensions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | self error: 'The class to be extended ', theClassDef name printString, ' not found'  ] 
					ifCorrect: [:theClassDef | "ignored" ] 
					ifIncorrect: [:theClassDef :expectedSymDictName :actualSymDictName | 
						movedSpecs add: {theClassDef . packageDef . expectedSymDictName . projectDef }] ] ] ].
	movedSpecs do: [:ar |
		self 
			_moveClassExtension: (ar at: 1) 
			toNewPackageClonedFrom: (ar at: 2) 
			inSymbolDictionary: (ar at: 3) 
			inProject: (ar at: 4) ].
	^ movedSpecs

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

	self reconcileProjectDefinitionSet: projectDefinitionSet.
	^ projectDefinition

]
