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
				ifTrue: [ correctBlock value: classDef value: actualSymDictName ]
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
	| projectTools movedClassExtensions newPackageDefMap movedPackageDefMap packageNameToSymbolDictNameMap |
	projectTools := Rowan projectTools.

	movedPackageDefMap := Dictionary new.
	movedClassExtensions := Set new.
	packageNameToSymbolDictNameMap := Dictionary new.
	projectDefinitionSet projects do: [:projectDef |
		| visitor spec visitedConfigurationMap packageToConfigurationMap config |
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
					ifCorrect: [:theClassDef :actualSymDictName | 
						(packageNameToSymbolDictNameMap at: packageDef name ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol ] 
					ifIncorrect: [:theClassDef :expectedSymDictName :actualSymDictName | 
						"the class is defined in the actualSymDictName and that's where the package should be loaded"
						(movedPackageDefMap at: packageDef name ifAbsentPut: [ Set new ]) add: {theClassDef . packageDef . actualSymDictName . projectDef } ] ].
			packageDef classExtensions values do: [:classDef |
				self 
					_checkExpectedSymbolDictForClassDefinition: classDef 
					packageDef: packageDef 
					projectDef: projectDef 
					classNotFound: [:theClassDef  | self error: 'The class to be extended ', theClassDef name printString, ' not found'  ] 
					ifCorrect: [:theClassDef :actualSymDictName | 
						(packageNameToSymbolDictNameMap at: packageDef name ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol ] 
					ifIncorrect: [:theClassDef :expectedSymDictName :actualSymDictName | 
						"the class extension is in the actualSymDictName and that's where the package should be loaded"
						movedClassExtensions add: {theClassDef . packageDef . actualSymDictName . projectDef } ] ] ].
		newPackageDefMap := Dictionary new.
		movedClassExtensions do: [:ar |
			| theClassDef packageDef actualSymDictName theProjectDef  newPackageDef|
			theClassDef := ar at: 1.
			packageDef := ar at: 2.
			actualSymDictName := ar at: 3.
			theProjectDef := ar at: 4.
			newPackageDef := self 
				_moveClassExtension: theClassDef 
				toNewPackageClonedFrom: packageDef 
				inSymbolDictionary: actualSymDictName 
				inProject: theProjectDef.
			(newPackageDefMap at: newPackageDef name ifAbsentPut: [ Set new ]) add: ar.
			(packageNameToSymbolDictNameMap at: newPackageDef name ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol  ].
		config := projectDef configurationTemplate.
packageNameToSymbolDictNameMap keysAndValuesDo: [:packageName :symDictNames |
	symDictNames size > 1 ifTrue: [ self error: 'More than one symbol dictionary associated with package ', packageName printString ].
    config 
		conditionalPackageMapSpecsAtGemStoneUserId: Rowan image currentUserId 
		andPackageName: packageName 
		setSymbolDictNameTo: symDictNames any ].

self halt. ].

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
