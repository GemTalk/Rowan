tests
testProjectGlobalsClassesExtensionsInSessionMethods

	"This test attempts to add extension methods to a class is in Globals --- a symbol dictionary owned by another user ... and the extension methods are kept in session methods in the package's symbol dictionary ..."

	| projectName projectDefinition projectTools packageDefinition className classExtensionDefinition packageNames |
	projectName := 'Simple'.
	packageNames := #('Simple-Globals-Extensions').
	projectTools := Rowan projectTools.

	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: packageNames;
		defaultSymbolDictName: self _symbolDictionaryName;
		setUseSessionMethodsForExtensions: true
			forPackageNamed: 'Simple-Globals-Extensions';
		yourself.

	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName.

	className := 'Object'.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'instanceFoo'
						protocol: 'accessing'
						source: 'instanceFoo ^''foo''');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'classFoo'
						protocol: 'accessing'
						source: 'classFoo ^''bar''');
		yourself.

	packageDefinition := projectDefinition
		packageNamed: 'Simple-Globals-Extensions'.
	packageDefinition addClassExtensionDefinition: classExtensionDefinition.

	projectTools load loadProjectDefinition: projectDefinition.

	self assert: Object classFoo = 'bar'.
	self assert: Object new instanceFoo = 'foo'