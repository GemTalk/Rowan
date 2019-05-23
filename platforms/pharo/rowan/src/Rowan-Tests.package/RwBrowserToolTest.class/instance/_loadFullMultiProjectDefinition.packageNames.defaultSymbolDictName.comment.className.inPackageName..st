private
_loadFullMultiProjectDefinition: projectNames packageNames: packageNames defaultSymbolDictName: defaultSymbolDictNames comment: comments className: classNames inPackageName: classPackageNames

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
		classDefinition category: (classPackageNames at: index).

		projectTools edit
			addClass: classDefinition
			inPackageNamed: (classPackageNames at: index)
			inProject: projectDefinition.

		projectSetDefinition addProject: projectDefinition.

		index = 1
			ifTrue: [ 
				{(self
					_basicClassDefinition: 'SimpleNormal1'
					superclass: 'Object'
					type: 'normal').
				(self
					_basicClassDefinition: 'SimpleVariable1'
					superclass: 'Object'
					type: 'variable').
				(self
					_basicClassDefinition: 'SimpleBag1'
					superclass: 'Bag'
					gs_options: #('disallowGciStore')
					type: 'normal').
				(self
					_basicClassDefinition: 'SimpleCustomByteArray1'
					superclass: 'Object'
					type: 'bytes').
				(self
					_basicClassDefinition: 'SimpleByteArray1'
					superclass: 'ByteArray'
					type: 'bytes').
				(self _basicClassDefinition: 'SimpleProto1' superclass: 'nil' type: 'normal')}
					do: [ :cd | 
						cd category: (classPackageNames at: index).
						projectTools edit
							addClass: cd
							inPackageNamed: (classPackageNames at: index)
							inProject: projectDefinition ] ] ].

	[ projectTools load loadProjectSetDefinition: projectSetDefinition  ]
		on: RwExecuteClassInitializeMethodsAfterLoadNotification
		do: [:ex | ex resume: true ].
