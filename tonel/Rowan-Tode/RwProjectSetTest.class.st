Class {
	#name : 'RwProjectSetTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'tests' }
RwProjectSetTest >> testProjectSetLoad1 [

	| projectTools projectName1 projectName2 projectDefinition1 projectDefinition2 projectSetDefinition projectRegistry projectNames loadedProjects |
	projectTools := Rowan projectTools.
	projectName1 := 'Simple1'.
	projectName2 := 'Simple2'.
	projectNames := {projectName1.
	projectName2}.


	projectNames
		do: [ :projectName | 
			(Rowan projectNamed: projectName ifAbsent: [  ])
				ifNotNil: [ :project | projectTools unregister unregisterSpecification: project specification ] ].

	RwGsImage newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	RwGsImage newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName2.

	projectSetDefinition := RwProjectSetDefinition new.

	projectDefinition1 := RwProjectDefinition
		newForGitPackageProjectNamed: projectName1.
	projectDefinition1
		packageNamesInLoadOrder: #('Simple1-Core');
		defaultSymbolDictName: self _symbolDictionaryName1.
	projectSetDefinition addDefinition: projectDefinition1.

	projectDefinition2 := RwProjectDefinition
		newForGitPackageProjectNamed: projectName2.
	projectDefinition2
		packageNamesInLoadOrder: #('Simple2-Core');
		defaultSymbolDictName: self _symbolDictionaryName2.
	projectSetDefinition addDefinition: projectDefinition2.

	projectTools load loadProjectSetDefinition: projectSetDefinition.

	self
		assert:
			(loadedProjects := Rowan loadedProjectsNamed: projectNames) size
				= projectNames size.
	loadedProjects entities
		do: [ :loadedProject | self assert: (projectNames includes: loadedProject name) ].

	{(UserGlobals at: #'RwGsProjectRegistry').
	(UserGlobals at: #'RwGsLoadedProjectRegistry')}
]
