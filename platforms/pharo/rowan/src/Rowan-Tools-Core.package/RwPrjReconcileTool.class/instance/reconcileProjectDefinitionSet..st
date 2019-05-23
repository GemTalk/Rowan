smalltalk api
reconcileProjectDefinitionSet: projectDefinitionSet
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
