tests-issue 263
testIssue263
	"https://github.com/dalehenrich/Rowan/issues/263"

	| projectName packageName projectDefinition projectSetDefinition |
	projectName := 'Issue263'.
	packageName := 'Issue263-Kernel'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: 'Globals';
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	[
"test"
		"add an extension method to a kernel class (unpackaged class)"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: '*', packageName asLowercase.

		Boolean
			rwCompileMethod: 'bar ^''bar'''
			category: '*', packageName asLowercase.

"validate"
		(Rowan image loadedClassExtensionsForClass: Object) 
			detect: [:ce |  ce loadedProject name = projectName ] ifNone: [ self assert: false description: 'no class extension found' ].

		(Rowan image loadedClassExtensionsForClass: Boolean) 
			detect: [:ce |  ce loadedProject name = projectName ] ifNone: [ self assert: false description: 'no class extension found' ] ]
				ensure: [ 
					Object rwRemoveSelector: #'bar'.
					Boolean rwRemoveSelector: #'bar']