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
RwPrjReconcileTool >> _moveClassExtension: theClassExtension toNewPackageClonedFrom: packageDef inSymbolDictionary: expectedSymDictName inProject: projectDef [

	| newPackageName newPackageDef |
	newPackageName := packageDef name, '-ext-', expectedSymDictName.
	newPackageDef := projectDef 
		packageNamed: newPackageName 
		ifAbsent: [ 
			| pDef |
			pDef := RwPackageDefinition newNamed: newPackageName.
			projectDef addPackage: pDef.
			pDef ].
	packageDef removeClassExtension: theClassExtension.
	newPackageDef addClassExtension: theClassExtension.
	"update the method categories --- hybrid only ... but that is what we're doing"
	theClassExtension instanceMethodDefinitions values do: [:methodDef |
		methodDef protocol: '*', newPackageName ].
	theClassExtension classMethodDefinitions values do: [:methodDef |
		methodDef protocol: '*', newPackageName ].
	^ newPackageDef

]

{ #category : 'private' }
RwPrjReconcileTool >> _moveClassExtension: theClassExtension toNewPackageNamed: newPackageName clonedFrom: packageDef inProject: projectDef [

	| newPackageDef |
	newPackageDef := projectDef 
		packageNamed: newPackageName 
		ifAbsent: [ 
			| pDef |
			pDef := RwPackageDefinition newNamed: newPackageName.
			projectDef addPackage: pDef.
			pDef ].
	packageDef removeClassExtension: theClassExtension.
	newPackageDef addClassExtension: theClassExtension.
	"update the method categories --- hybrid only ... but that is what we're doing"
	theClassExtension instanceMethodDefinitions values do: [:methodDef |
		methodDef protocol: '*', newPackageName ].
	theClassExtension classMethodDefinitions values do: [:methodDef |
		methodDef protocol: '*', newPackageName ].
	^ newPackageDef

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet defaultGroupName: defaultGroupName globalsGroupName: globalsGroupName globalsUserId: globalsUserId [

	| configExportMap |
	configExportMap := Dictionary new.
	projectDefinitionSet projects do: [:projectDef |
		| config movedClassExtensions globalPackageNames originalPackageNames |
		movedClassExtensions := Set new.
		globalPackageNames := Set new.
		originalPackageNames := projectDef packageNames.
		projectDef packages values do: [:packageDef |
			packageDef classExtensions values do: [:classDef |
				(Globals 
					at: classDef name asSymbol 
					ifAbsent: [])
						ifNotNil: [ 
							"the class extension is for a class that is Globals, so it needs to be moved to a new Globals extension package"
							movedClassExtensions add: {classDef . packageDef . projectDef } ] ] ].
		movedClassExtensions do: [:ar |
			| theClassDef packageDef theProjectDef  newPackageName |
			theClassDef := ar at: 1.
			packageDef := ar at: 2.
			theProjectDef := ar at: 3.
			newPackageName :=  packageDef name , '-Globals'.
			self 
				_moveClassExtension: theClassDef 
				toNewPackageNamed: packageDef name , '-Globals'
				clonedFrom: packageDef 
				inProject: theProjectDef.
			globalPackageNames add: newPackageName ].
		config := RwProjectLoadConfiguration new
			name: 'Default';
			projectName: projectDef name;
			yourself.
		{ defaultGroupName . globalsGroupName }
			do: [:groupName | config addDefinedGroupName: groupName ].
		config 
			conditionalPackagesAtConditions: { Rowan platform basePlatformAttribute } 
				andGroup: defaultGroupName 
				addPackageNames: originalPackageNames;
			conditionalPackagesAtConditions: { Rowan platform basePlatformAttribute } 
				andGroup: globalsGroupName 
				addPackageNames: globalPackageNames.
		globalPackageNames do: [:packageName |
			config 
				conditionalPackageMapSpecsAtGemStoneUserId: globalsUserId 
				andPackageName: packageName 
				setSymbolDictNameTo: 'Globals' ].
		configExportMap at: config put: projectDef configurationExportUrl ].
	^ configExportMap

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileGlobalClassExtensionsForProjectFromSpecUrl: specUrl defaultGroupName: defaultGroupName globalsGroupName: globalsGroupName  globalsUserId: globalsUserId writeProject: writeProjectBool [
	| projectDefinition projectDefinitionSet projectTools loadedProjectInfo configExportMap |
	projectTools := Rowan projectTools.
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrl.
	projectDefinitionSet := projectTools read readProjectSetForProjectDefinition: projectDefinition.

	loadedProjectInfo := projectDefinitionSet properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			"install the packageMapSpecs for this load into the specification prior to the reconcile"
			(projectDefinitionSet projectNamed: projectName ifAbsent: [])
				updateGsPlatformSpecLoadedProjectInfo: projectInfo ].

	configExportMap := self 
		reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet 
		defaultGroupName: defaultGroupName 
		globalsGroupName: globalsGroupName  
		globalsUserId: globalsUserId.
	writeProjectBool
		ifTrue: [
			configExportMap keysAndValuesDo: [:config :exportUrl |
				config exportToUrl: exportUrl ].
			projectTools write writeProjectDefinition: projectDefinition ].
	^ projectDefinition

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileProjectDefinitionSet: projectDefinitionSet [
	| movedClassExtensions packageNameToSymbolDictNameMap configExportMap |

	configExportMap := Dictionary new.
	movedClassExtensions := Set new.
	packageNameToSymbolDictNameMap := Dictionary new.
	projectDefinitionSet projects do: [:projectDef |
		| config |
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
						(packageNameToSymbolDictNameMap at: packageDef name ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol ] ].
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
			(packageNameToSymbolDictNameMap at: newPackageDef name ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol  ].
		config := projectDef configurationTemplate.
		packageNameToSymbolDictNameMap keysAndValuesDo: [:packageName :symDictNames |
			symDictNames size > 1 ifTrue: [ self error: 'More than one symbol dictionary associated with package ', packageName printString ].
			config 
				conditionalPackageMapSpecsAtGemStoneUserId: Rowan image currentUserId 
				andPackageName: packageName 
				setSymbolDictNameTo: symDictNames any ].
		configExportMap at: config put: projectDef configurationExportUrl ].
	^ configExportMap

]

{ #category : 'smalltalk api' }
RwPrjReconcileTool >> reconcileProjectFromSpecUrl: specUrl writeProject: writeProjectBool [
	| projectDefinition projectDefinitionSet projectTools loadedProjectInfo configExportMap |
	projectTools := Rowan projectTools.
	projectDefinition := projectTools create createProjectDefinitionFromSpecUrl: specUrl.
	projectDefinitionSet := projectTools read readProjectSetForProjectDefinition: projectDefinition.

	loadedProjectInfo := projectDefinitionSet properties at: 'loadedProjectInfo' ifAbsent: [ Dictionary new ].
	loadedProjectInfo keysAndValuesDo: [:projectName :projectInfo |
			"install the packageMapSpecs for this load into the specification prior to the reconcile"
			(projectDefinitionSet projectNamed: projectName ifAbsent: [])
				updateGsPlatformSpecLoadedProjectInfo: projectInfo ].

	configExportMap := self reconcileProjectDefinitionSet: projectDefinitionSet.
	writeProjectBool
		ifTrue: [
			configExportMap keysAndValuesDo: [:config :exportUrl |
				config exportToUrl: exportUrl ].
			projectTools write writeProjectDefinition: projectDefinition ].
	^ projectDefinition

]
