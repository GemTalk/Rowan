private
_loadMultiProjectDefinition: projectNames packageNames: packageNames defaultSymbolDictName: defaultSymbolDictNames comment: comments className: classNames inPackageName: classPackageNames

	| projectTools projectSetDefinition |
	projectTools := Rowan projectTools.

	projectSetDefinition := RwProjectSetDefinition new.

	1 to: projectNames size do: [ :index | 
		| projectDefinition classDefinition |
		(Rowan image loadedProjectNamed: (projectNames at: index) ifAbsent: [  ])
			ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

		projectDefinition := self
			_standardProjectDefinition: (projectNames at: index)
			packageNames: (packageNames at: index)
			defaultSymbolDictName: (defaultSymbolDictNames at: index)
			comment:
				'This is a simple project used to work out a "classic" class browser-style API'.

		classDefinition := self _standardClassDefinition: (classNames at: index).

		projectTools edit
			addClass: classDefinition
			inPackageNamed: (classPackageNames at: index)
			inProject: projectDefinition.

		projectSetDefinition addProject: projectDefinition ].

	[ projectTools load loadProjectSetDefinition: projectSetDefinition ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
