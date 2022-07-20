Extension { #name : 'RwProject' }

{ #category : '*rowan-corev2' }
RwProject >> addNewPackageNamed: packageName inSybolDictionaryNamed: symbolDictionaryName toComponentNamed: componentName [
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			| projectDefinition component |
			projectDefinition := self defined.
			component := projectDefinition
				componentNamed: componentName
				ifAbsent: [ 
					self
						error:
							'The component ' , componentName printString , ' does not exist in the project'
								, self name printString ].
			self loadSpecification gemstoneDefaultSymbolDictName = symbolDictionaryName
				ifTrue: [ projectDefinition addPackageNamed: packageName toComponentNamed: componentName ]
				ifFalse: [ 
					projectDefinition
						addPackageNamed: packageName
						toComponentNamed: componentName
						gemstoneDefaultSymbolDictionaryForUser:
							System myUserProfile userId -> symbolDictionaryName ].
			^ projectDefinition load ].
	self
		error:
			'The package ' , packageName printString , ' already exists in the project '
				, self name printString
]

{ #category : '*rowan-corev2' }
RwProject >> addNewPackageNamed: packageName toComponentNamed: componentName [
	^ self
		addNewPackageNamed: packageName
		inSybolDictionaryNamed: self loadSpecification gemstoneDefaultSymbolDictName
		toComponentNamed: componentName
]

{ #category : '*rowan-corev2' }
RwProject >> exportLoadSpecification [
	^ self _loadedProject asDefinition exportLoadSpecification
]

{ #category : '*rowan-corev2' }
RwProject >> removePackageNamed: packageName [
	| projectDefinition |
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self
				error:
					'The package ' , packageName printString , ' does not exists in the project '
						, self name printString ].
	projectDefinition := self defined.
	projectDefinition removePackageNamed: packageName.
	^ projectDefinition load
]

{ #category : '*rowan-corev2' }
RwProject >> requiredProjects [
	"return closure of dependent projects"

	"https://github.com/GemTalk/Rowan/issues/571 is addressed"

	^ self requiredProjectNames asArray
		collect: [ :projectName | Rowan projectNamed: projectName ]
]

{ #category : '*rowan-corev2' }
RwProject >> requiredProjectSet [
	"return a project definition set containing the receiver and the closure of required projects"

	| requiredProjectNames requiredProjectSet |
	requiredProjectSet := RwProjectSetDefinition new.
	requiredProjectNames := self requiredProjectNames asSet.
	requiredProjectNames add: self projectName.
	[ requiredProjectNames isEmpty ]
		whileFalse: [ 
			| trp |
			trp := requiredProjectNames copy.
			requiredProjectNames := Set new.
			trp
				do: [ :pn | 
					requiredProjectSet
						projectNamed: pn
						ifAbsent: [ 
							| project |
							project := Rowan projectNamed: pn.
							requiredProjectSet addProject: project asDefinition.
							requiredProjectNames addAll: project requiredProjectNames ] ] ].
	^ requiredProjectSet
]
