tests
testProjectSetLoad1

	| projectTools projectName1 projectName2 projectDefinition1 projectDefinition2 projectSetDefinition projectNames loadedProjects |
	projectTools := Rowan projectTools.
	projectName1 := 'Simple1'.
	projectName2 := 'Simple2'.
	projectNames := {projectName1.
	projectName2}.


	projectNames
		do: [ :projectName | 
			(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName1.
	projectDefinition1
		packageNames: #('Simple1-Core');
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectSetDefinition addDefinition: projectDefinition1.

	projectDefinition2 := RwProjectDefinition
		newForGitBasedProjectNamed: projectName2.
	projectDefinition2
		addPackagesNamed: #('Simple2-Core');
		defaultSymbolDictName: self _symbolDictionaryName2.
	projectSetDefinition addDefinition: projectDefinition2.

	projectTools load loadProjectSetDefinition: projectSetDefinition.

	self
		assert:
			(loadedProjects := self _loadedProjectsNamed: projectNames) size
				= projectNames size.
	loadedProjects entities
		do: [ :loadedProject | self assert: (projectNames includes: loadedProject name) ]