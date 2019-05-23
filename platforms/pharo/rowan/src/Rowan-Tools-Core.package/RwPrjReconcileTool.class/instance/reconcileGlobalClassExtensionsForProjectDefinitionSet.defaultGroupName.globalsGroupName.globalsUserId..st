smalltalk api
reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet defaultGroupName: defaultGroupName globalsGroupName: globalsGroupName globalsUserId: globalsUserId

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
