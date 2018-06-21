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
	newPackageDef addClassExtension: theClassDef.
	^ newPackageDef

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileProjectDefinitionSet: projectDefinitionSet [
	| projectTools movedClassExtensions newPackageDefMap movedPackageDefMap |
	projectTools := Rowan projectTools.

	movedPackageDefMap := Dictionary new.
	movedClassExtensions := Set new.
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
						"the class is defined in the actualSymDictName and that's where the package should be loaded"
						(movedPackageDefMap at: packageDef name ifAbsentPut: [ Set new ]) add: {theClassDef . packageDef . actualSymDictName . projectDef }] ].
			packageDef classExtensions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | self error: 'The class to be extended ', theClassDef name printString, ' not found'  ] 
					ifCorrect: [:theClassDef | "ignored" ] 
					ifIncorrect: [:theClassDef :expectedSymDictName :actualSymDictName | 
						"the class extension is in the actualSymDictName and that's where the package should be loaded"
						movedClassExtensions add: {theClassDef . packageDef . actualSymDictName . projectDef } ] ] ] ].
	newPackageDefMap := Dictionary new.
	movedClassExtensions do: [:ar |
		| theClassDef packageDef actualSymDictName projectDef  newPackageDef|
		theClassDef := ar at: 1.
		packageDef := ar at: 2.
		actualSymDictName := ar at: 3.
		projectDef := ar at: 4.
		newPackageDef := self 
			_moveClassExtension: theClassDef 
			toNewPackageClonedFrom: packageDef 
			inSymbolDictionary: actualSymDictName 
			inProject: projectDef.
		(newPackageDefMap at: newPackageDef name ifAbsentPut: [ Set new ]) add: ar ].
	^ newPackageDefMap

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
