method browsing
addOrUpdateMethod: methodSource inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName

	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	| projectTools loadedPackage classExtensionDef methodDef updateBlock projectDefinition packageDefinition projectSetDefinition loadedMethodToBeRemoved |
	projectSetDefinition := RwProjectSetDefinition new.

	methodDef := RwMethodDefinition newForSource: methodSource protocol: protocol.
	loadedMethodToBeRemoved := self
		_loadedMethod: methodDef selector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			"no pre-existing method for this selector installed"
			 ].

	projectTools := Rowan projectTools.
	updateBlock := [ :cDef :pDef | 
	loadedMethodToBeRemoved
		ifNil: [
			"no method needs to be remove, just add the method to the class or extension def"
			isMeta
				ifTrue: [ cDef addClassMethodDefinition: methodDef ]
				ifFalse: [ cDef addInstanceMethodDefinition: methodDef ] ]
		ifNotNil: [ :loadedMethod | 
			| loadedPackageForMethod |
			loadedPackageForMethod := loadedMethod loadedPackage.
			loadedPackageForMethod name = packageName
				ifTrue: [ 
					"loaded method being updated in same package, sjust update the method def"
					isMeta
						ifTrue: [ cDef updateClassMethodDefinition: methodDef ]
						ifFalse: [ cDef updateInstanceMethodDefinition: methodDef ] ]
				ifFalse: [ 
					| loadedClassOrExtension projectDef packageDef crDef |
					"loaded method in different package than new version of method"
					projectDef := loadedPackageForMethod loadedProject asDefinition.
					projectDef name = pDef name
						ifTrue: [ 
							"both packages are in same project"
							projectDef := pDef ]
						ifFalse: [ 
							"each package in a different project, will need to load loaded method project as well"
							projectSetDefinition addProject: projectDef ].
					packageDef := projectDef packageNamed: loadedPackageForMethod name.
					loadedClassOrExtension := loadedMethod loadedClass.
					crDef := loadedClassOrExtension isLoadedClass
						ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
						ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
					"remove the method from one package and add it to the other"
					isMeta
						ifTrue: [ 
							crDef removeClassMethod: methodDef selector.
							cDef addClassMethodDefinition: methodDef  ]
						ifFalse: [ 
							crDef removeInstanceMethod: methodDef selector.
							cDef addInstanceMethodDefinition: methodDef  ] ] ].
	projectSetDefinition addProject: pDef.
	projectTools load loadProjectSetDefinition: projectSetDefinition.
	(self _loadedMethod: methodDef selector inClassNamed: className isMeta: isMeta)
		handle ].

	self
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName
				ifTrue: [ ^ updateBlock value: classDef value: projectDef ]
				ifFalse: [ 
					"the named package is different from the class definition package"
					 ] ]
		ifAbsent: [ 
			"no loaded class definition, so we probably need to add a class extension"
			 ].
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'A package named ' , packageName printString , ' was not found.' ].
	projectDefinition := loadedPackage loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: packageName.

	classExtensionDef := packageDefinition classExtensions
		at: className
		ifAbsent: [ 
			"no existing class extension definition ... create a new one"
			classExtensionDef := RwClassExtensionDefinition newForClassNamed: className.

			packageDefinition addClassExtensionDefinition: classExtensionDef.
			classExtensionDef ].

	^ updateBlock value: classExtensionDef value: projectDefinition