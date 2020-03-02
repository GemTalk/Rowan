Extension { #name : 'RwLoadingTest' }

{ #category : '*rowan-testsv2' }
RwLoadingTest >> projectDefinitionFor: packageNames [

	| projectDefinition |
	projectDefinition := ((Rowan globalNamed: 'RwProjectDefinition')
		newForGitBasedProjectNamed: 'RwloadingTest')
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName.

	^ projectDefinition
]
