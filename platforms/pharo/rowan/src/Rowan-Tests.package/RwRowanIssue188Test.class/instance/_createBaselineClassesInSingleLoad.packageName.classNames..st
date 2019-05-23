private
_createBaselineClassesInSingleLoad: projectName packageName: packageName classNames: classNames

	"
		Set up baseline:
			1. Class A and B
			2. Class B has a method w that returns class A.
	"

	| projectDefinition packageDefinition |
	{projectName}
		do: [ :name | 
			(Rowan image loadedProjectNamed: name ifAbsent: [  ])
				ifNotNil: [ :project | Rowan image _removeLoadedProject: project ] ].

	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	projectDefinition
		addPackagesNamed: {packageName};
		yourself.
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName1.

	packageDefinition := projectDefinition packageNamed: packageName.

	classNames do: [:className |
		| classDefinition |
		classDefinition := RwClassDefinition
			newForClassNamed: className
			super: 'Object'
			instvars: #()
			classinstvars: #()
			classvars: #()
			category: nil
			comment: 'I am a ', className, ' class'
			pools: #()
			type: 'normal'.
		packageDefinition addClassDefinition: classDefinition ].

	(packageDefinition classDefinitions at: (classNames at: 2))
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'w'
						protocol: 'accessing'
						source: 'w ^ ', (classNames at: 1)).

	Rowan projectTools load loadProjectDefinition: projectDefinition
