Class {
	#name : 'RwPrjReconcileToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'private' }
RwPrjReconcileToolV2 >> _checkExpectedSymbolDictForClassDefinition: classDef packageDef: packageDef projectDef: projectDef classNotFound: classNotFoundBlock ifCorrect: correctBlock ifIncorrect: incorrectBlock [
	| class ar symDict expectedSymDictName actualSymDictName |
	class := Rowan globalNamed: classDef name.
	class
		ifNil: [ classNotFoundBlock value: classDef ]
		ifNotNil: [ 
			ar := Rowan image symbolList dictionariesAndSymbolsOf: class.
			ar size > 1
				ifTrue: [ 
					self
						error:
							'The class ' , classDef name printString , ' is in multiple symbol dictionaries' ].
			symDict := ar first at: 1.
			actualSymDictName := symDict name asString.
			expectedSymDictName := projectDef
				symbolDictNameForPackageNamed: packageDef name.
			expectedSymDictName = actualSymDictName
				ifTrue: [ correctBlock value: classDef value: actualSymDictName ]
				ifFalse: [ 
					incorrectBlock
						value: classDef
						value: expectedSymDictName
						value: actualSymDictName ] ]
]

{ #category : 'private' }
RwPrjReconcileToolV2 >> _moveClassExtension: theClassExtension toNewPackageClonedFrom: packageDef inSymbolDictionary: expectedSymDictName inProject: projectDef [
	| newPackageName newPackageDef |
	newPackageName := packageDef name , '-ext-' , expectedSymDictName.
	newPackageDef := projectDef
		packageNamed: newPackageName
		ifAbsent: [ 
			| component |
			component := projectDef componentForPackageNamed: packageDef name.
			projectDef addPackageNamed: newPackageName toComponentNamed: component name ].
	packageDef
		moveClassExtension: theClassExtension
		modifyMethodDefinitions: [ :methodDef | 
			"update the method categories --- hybrid only ... but that is what we're doing"
			methodDef protocol: '*' , newPackageName ]
		toPackage: newPackageDef.
	^ newPackageDef
]

{ #category : 'private' }
RwPrjReconcileToolV2 >> _moveClassExtension: theClassExtension toNewPackageNamed: newPackageName clonedFrom: packageDef inProject: projectDef [
	| newPackageDef |
	newPackageDef := projectDef
		packageNamed: newPackageName
		ifAbsent: [ 
			| component |
			component := projectDef componentForPackageNamed: packageDef name.
			projectDef addPackageNamed: newPackageName toComponentNamed: component name ].
	packageDef
		moveClassExtension: theClassExtension
		modifyMethodDefinitions: [ :methodDef | 
			"update the method categories --- hybrid only ... but that is what we're doing"
			methodDef protocol: '*' , newPackageName ]
		toPackage: newPackageDef.
	^ newPackageDef
]

{ #category : 'smalltalk api' }
RwPrjReconcileToolV2 >> reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet globalsUserId: globalsUserId [
	projectDefinitionSet projects
		do: [ :projectDef | 
			| movedClassExtensions originalPackageNames |
			movedClassExtensions := Set new.
			originalPackageNames := projectDef packageNames.
			projectDef packages values
				do: [ :packageDef | 
					packageDef classExtensions values
						do: [ :classDef | 
							(Globals at: classDef name asSymbol ifAbsent: [  ])
								ifNotNil: [ 
									"the class extension is for a class that is Globals, so it needs to be moved to a new Globals extension package"
									movedClassExtensions
										add:
											{classDef.
											packageDef.
											projectDef} ] ] ].
			movedClassExtensions
				do: [ :ar | 
					| theClassDef packageDef theProjectDef newPackageName |
					theClassDef := ar at: 1.
					packageDef := ar at: 2.
					theProjectDef := ar at: 3.
					newPackageName := packageDef name , '-Globals'.
					self
						_moveClassExtension: theClassDef
						toNewPackageNamed: packageDef name , '-Globals'
						clonedFrom: packageDef
						inProject: theProjectDef.
					theProjectDef
						gemstoneSetSymbolDictNameForUser: globalsUserId
						to: 'Globals'
						forPackageNamed: newPackageName ] ]
]

{ #category : 'smalltalk api' }
RwPrjReconcileToolV2 >> reconcileProjectDefinitionSet: projectDefinitionSet [
	| movedClassExtensions packageNameToSymbolDictNameMap currentUserId |
	movedClassExtensions := Set new.
	packageNameToSymbolDictNameMap := Dictionary new.
	currentUserId := Rowan image currentUserId.
	projectDefinitionSet projects
		do: [ :projectDef | 
			projectDef packages values
				do: [ :packageDef | 
					packageDef classDefinitions values
						do: [ :classDef | 
							self
								_checkExpectedSymbolDictForClassDefinition: classDef
								packageDef: packageDef
								projectDef: projectDef
								classNotFound: [ :theClassDef | self error: 'The class ' , theClassDef name printString , ' not found' ]
								ifCorrect: [ :theClassDef :actualSymDictName | 
									(packageNameToSymbolDictNameMap
										at: packageDef name
										ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol ]
								ifIncorrect: [ :theClassDef :expectedSymDictName :actualSymDictName | 
									"the class is defined in the actualSymDictName and that's where the package should be loaded"
									(packageNameToSymbolDictNameMap
										at: packageDef name
										ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol ] ].
					packageDef classExtensions values
						do: [ :classDef | 
							self
								_checkExpectedSymbolDictForClassDefinition: classDef
								packageDef: packageDef
								projectDef: projectDef
								classNotFound: [ :theClassDef | 
									self
										error:
											'The class to be extended ' , theClassDef name printString , ' not found' ]
								ifCorrect: [ :theClassDef :actualSymDictName | 
									(packageNameToSymbolDictNameMap
										at: packageDef name
										ifAbsentPut: [ IdentitySet new ]) add: actualSymDictName asSymbol ]
								ifIncorrect: [ :theClassDef :expectedSymDictName :actualSymDictName | 
									"the class extension is in the actualSymDictName and that's where the package should be loaded"
									movedClassExtensions
										add:
											{theClassDef.
											packageDef.
											actualSymDictName.
											projectDef} ] ] ].
			movedClassExtensions
				do: [ :ar | 
					| theClassDef packageDef actualSymDictName theProjectDef newPackageDef defaultSymDictName |
					theClassDef := ar at: 1.
					packageDef := ar at: 2.
					actualSymDictName := (ar at: 3) asString.
					theProjectDef := ar at: 4.
					defaultSymDictName := theProjectDef
						gemstoneDefaultSymbolDictNameForUser: currentUserId.

					newPackageDef := self
						_moveClassExtension: theClassDef
						toNewPackageClonedFrom: packageDef
						inSymbolDictionary: actualSymDictName
						inProject: theProjectDef.
					(theProjectDef gemstoneSymbolDictNameForPackageNamed: newPackageDef name)
						= defaultSymDictName
						ifTrue: [ 
							actualSymDictName ~= defaultSymDictName
								ifTrue: [ 
									"Only need to set the sym dict name, if the sym dict name is not the default"
									theProjectDef
										gemstoneSetSymbolDictNameForUser: currentUserId
										to: actualSymDictName
										forPackageNamed: newPackageDef name ] ]
						ifFalse: [ 
							(theProjectDef
								gemstoneSymbolDictNameForPackageNamed: newPackageDef name
								forUser: currentUserId) ~= actualSymDictName
								ifTrue: [ 
									self
										error:
											'mismatched symbol dictionary names for the package '
												, newPackageDef name printString ].

							theProjectDef
								gemstoneSetSymbolDictNameForUser: currentUserId
								to: actualSymDictName
								forPackageNamed: newPackageDef name ] ] ]
]
