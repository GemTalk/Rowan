private
_loadExtendedProjectDefinition: projectName packageNames: packageNames defaultSymbolDictName: defaultSymbolDictName comment: comment className: className inPackageName: packageName1 extensionPackageName: packageName2

	| projectDefinition classDefinition classExtensionDefinition projectTools |
	projectTools := Rowan projectTools.

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
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
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'add:'
						protocol: 'addition'
						source: 'add: anInteger ^ivar1 + anInteger');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'cadd:'
						protocol: 'addition'
						source: 'cadd: anInteger ^civar1 + anInteger');
		yourself.

	projectTools edit
		addClassExtension: classExtensionDefinition
		inPackageNamed: packageName2
		inProject: projectDefinition.

	[ projectTools load loadProjectDefinition: projectDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
