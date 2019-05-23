private
_copyClassDefinitionNamed: className to: newName

	"answer a projectSetDefinition that includes the copied class definition"

	| loadedClass classDef loadedPackage projectDef packageDef  projectSetDefinition |
	(self _loadedClassNamed: newName ifAbsent:  [])
		ifNotNil: [ self error: 'There is already a class named ', newName printString, ' in the system.' ].
	loadedClass := self 
		_loadedClassNamed: className 
		ifAbsent: [  self error: 'No loaded class named: ', className printString , ' found.' ].
	projectSetDefinition := RwProjectSetDefinition new.
	loadedPackage := loadedClass loadedPackage.
	projectDef := loadedPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	classDef := loadedClass asDefinition.
	classDef name: newName.
	packageDef addClassDefinition: classDef.
	projectSetDefinition addProject: projectDef.

	(self _loadedClassExtensionsNamed: className ifAbsent: [ #() ])
		do: [:loadedClassExtension | 
			| classExtDef loadedProject |
			loadedPackage := loadedClassExtension loadedPackage.
			loadedProject := loadedPackage loadedProject.
			projectDef := projectSetDefinition
				projectNamed: loadedPackage loadedProject name
				ifAbsent: [ 
					projectDef := loadedProject asDefinition.
					projectSetDefinition addProject: projectDef.
					projectDef ].

			packageDef := projectDef packageNamed: loadedPackage name.
			classExtDef := loadedClassExtension asDefinition.
			classExtDef name: newName.
			packageDef addClassExtensionDefinition: classExtDef ].

	^ projectSetDefinition