Extension { #name : 'RwPrjDeleteTool' }

{ #category : '*rowan-tools-GemStone-35x' }
RwPrjDeleteTool >> deleteProjectSetDefinition: projectSetDefinitionToDelete [
	"Remove the classes and methods managed by the listed project definitions."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects not in the set, the delete will fail ... 
		add projects containing  subclasses to the <projectSetDefinitionToDelete> ."

	| loadedProjectSet loadTool orphanedProjectMap unloadedProjectNames |
	loadedProjectSet := projectSetDefinitionToDelete deriveLoadedThings.

	unloadedProjectNames := loadedProjectSet asProjectDefinitionSet definitionNames
		asSet.
	orphanedProjectMap := Dictionary new.
	Rowan projects
		do: [ :project | 
			| unloadedRequiredProjects |
			unloadedRequiredProjects := project requiredProjectNames asSet * unloadedProjectNames.
			(unloadedProjectNames includes: project name)
				ifFalse: [ 
					"one or more required projects for a project that is NOT being unloaded are being unloaded"
					unloadedRequiredProjects isEmpty
						ifFalse: [ orphanedProjectMap at: project name put: unloadedRequiredProjects asArray sort ] ] ].
	orphanedProjectMap isEmpty
		ifFalse: [ 
			| strm |
			strm := WriteStream on: String new.
			strm
				nextPutAll:
						'Cannot unload projects that are required by other projects that are not being unloaded.';
				lf.
			orphanedProjectMap
				keysAndValuesDo: [ :projectName :unloadedRequiredProjects | 
					strm
						tab;
						nextPutAll:
								'the project ' , projectName printString
										, ' requires the following projects that are not being unloaded:';
						lf.
					unloadedRequiredProjects
						do: [ :pName | 
							strm
								tab;
								tab;
								nextPutAll: pName;
								lf ] ].
			self error: strm contents ].

	projectSetDefinitionToDelete definitions
		do: [ :projectDef | 
			loadTool := Rowan projectTools loadV2.
			projectDef packageNames
				do: [ :packageName | projectDef removePackageNamed: packageName ] ].
	loadTool loadProjectSetDefinition: projectSetDefinitionToDelete.

	loadedProjectSet
		do: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ifAbsent: [  ] ]
]
