private
_loadExtendedProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName1 extensionPackageNames: extensionPackageNames

	| projectDefinition classDefinition classExtensionDefinition projectTools |
	projectTools := Rowan projectTools.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: defaultSymbolDictName
		comment: 'This is a simple project used to test class extension edit API'.

	classDefinition := self _standardClassDefinition: className.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName1
		inProject: projectDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1:'
						protocol: 'accessing'
						source: 'ivar1: anInteger ivar1 := anInteger');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'cadd:'
						protocol: 'addition'
						source: 'cadd: anInteger ^civar1 + anInteger');
		yourself.

	projectTools edit
		addClassExtension: classExtensionDefinition
		inPackageNamed: (extensionPackageNames at: 1)
		inProject: projectDefinition.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'add:'
						protocol: 'addition'
						source: 'add: anInteger ^ivar1 + anInteger');
		yourself.

	projectTools edit
		addClassExtension: classExtensionDefinition
		inPackageNamed: (extensionPackageNames at: 2)
		inProject: projectDefinition.


	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
