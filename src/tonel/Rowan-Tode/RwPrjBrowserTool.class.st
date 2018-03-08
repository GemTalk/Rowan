Class {
	#name : 'RwPrjBrowserTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassExtensionsNamed: className [

	^ self
		_loadedClassExtensionsNamed: className
		ifAbsent: [ 
			self
				error:
					'No extensions for the class ' , className printString , ' were found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassExtensionsNamed: className ifAbsent: absentBlock [

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | ^ loadedClassExtensionSet ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassNamed: className [

	^ self
		_loadedClassNamed: className
		ifAbsent: [ self error: 'The class ' , className printString , ' was not found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedClassNamed: className ifAbsent: absentBlock [

	Rowan image
		loadedClassNamed: className
		ifFound: [ :loadedClass | ^ loadedClass ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta [

	^ self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			self
				error:
					'No loaded method for ' , methodSelector printString , ' was found for class '
						, className printString ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta ifAbsent: absentBlock [

	^ Rowan image
		loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | ^ loadedMethod ]
		ifAbsent: absentBlock
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedPackageNamed: packageName [

	^ Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectForClassNamed: className [

	^ (self _loadedClassNamed: className) loadedProject
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectForMethod: methodSelector inClassNamed: className isMeta: isMeta [

	^ (self _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta)
		loadedProject
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectForPackageNamed: packageName [

	^ (self _loadedPackageNamed: packageName) loadedProject
]

{ #category : 'private' }
RwPrjBrowserTool >> _loadedProjectNamed: projectName [

	^ Rowan image
		loadedProjectNamed: projectName
		ifAbsent: [ self error: 'No project named ' , projectName printString , ' found' ]
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectForClassNamed: className [

	^ (self _loadedProjectForClassNamed: className) asDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectForMethod: methodSelector inClassNamed: className isMeta: isMeta [

	^ (self
		_loadedProjectForMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta) asDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectForPackageNamed: packageName [

	^ (self _loadedProjectForPackageNamed: packageName) asDefinition
]

{ #category : 'private' }
RwPrjBrowserTool >> _projectNamed: projectName [

	^ (self _loadedProjectNamed: projectName) asDefinition
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> addOrUpdateMethod: methodSource inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [

	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package."

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
		ifNotNil: [ :loadedMethod | 
			| loadedPackage |
			loadedPackage := loadedMethod loadedPackage.
			loadedPackage name = packageName
				ifTrue: [ 
					"loaded method being updated in same package, so no need to remove method definition from the package"
					 ]
				ifFalse: [ 
					| loadedClass projectDef packageDef crDef |
					"loaded method in different package than new version of method"
					projectDef := loadedPackage loadedProject asDefinition.
					projectDef name = pDef name
						ifTrue: [ 
							"both packages are in same project"
							projectDef := pDef ]
						ifFalse: [ 
							"each package in a different project, will need to load loaded method project as well"
							projectSetDefinition addProject: projectDef ].
					packageDef := projectDef packageNamed: loadedPackage name.
					loadedClass := loadedMethod loadedClass.
					crDef := loadedClass isLoadedClass
						ifTrue: [ packageDef classDefinitions at: loadedClass name ]
						ifFalse: [ packageDef classExtensions at: loadedClass name ].
					isMeta
						ifTrue: [ crDef removeClassMethod: methodDef selector ]
						ifFalse: [ crDef removeInstanceMethod: methodDef selector ] ] ].
	isMeta
		ifTrue: [ cDef updateClassMethodDefinition: methodDef ]
		ifFalse: [ cDef updateInstanceMethodDefinition: methodDef ].
	projectSetDefinition addProject: pDef.
	^ projectTools load loadProjectSetDefinition: projectSetDefinition ].

	self
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			packageDef name = packageName
				ifTrue: [ updateBlock value: classDef value: projectDef ]
				ifFalse: [ 
					"the named package is different from the class definition package"
					 ] ]
		ifAbsent: [ 
			"no loaded class definition, so we probably need to add a class extension"
			 ].
	loadedPackage := Rowan
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'A package named ' , packageName printString , ' was not found.' ].
	projectDefinition := loadedPackage loadedProject asDefinition.
	packageDefinition := projectDefinition packageNamed: packageName.

	classExtensionDef := packageDefinition classExtensions
		at: className
		ifAbsent: [ 
			"no existing class extension definition ... create a new one"
			classExtensionDef := RwClassExtensionDefinition newForClassNamed: className.

			packageDefinition addClassExtension: classExtensionDef.
			classExtensionDef ].

	updateBlock value: classExtensionDef value: projectDefinition
]

{ #category : 'package browsing' }
RwPrjBrowserTool >> addPackageNamed: packageName toProjectNamed: projectName [

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	projectDefinition addPackage: (RwPackageDefinition newNamed: packageName).
	Rowan projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> classCreationTemplateForSubclassOf: superclassName category: category [

	| stream |
	stream := WriteStream on: String new.
	stream
		nextPutAll: RwClassDefinition name asString;
		cr;
		tab;
		nextPutAll: 'newForClassNamed: ''NameOfSubclass''';
		cr;
		tab;
		nextPutAll: ' super: ' , superclassName;
		cr;
		tab;
		nextPutAll: ' instvars: #() ';
		cr;
		tab;
		nextPutAll: ' classinstvars: #() ';
		cr;
		tab;
		nextPutAll: ' classvars: #() ';
		cr;
		tab;
		nextPutAll: ' category: #() ';
		cr;
		tab;
		nextPutAll: ' comment: ''class comment'' ';
		cr;
		tab;
		nextPutAll: ' pools: #() ';
		cr;
		tab;
		nextPutAll: ' type: ''normal|variable|bytes''';
		yourself.

	^ stream contents
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> createClass: classDefinition inPackageNamed: packageName [

	| loadedPackage projectDefinition projectTools |
	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self error: 'The package named ' , packageName printString , ' was not found' ].
	projectDefinition := loadedPackage loadedProject asDefinition.

	projectTools := Rowan projectTools.

	projectTools edit
		addClass: classDefinition
		inPackageNamed: packageName
		inProject: projectDefinition.

	projectTools load loadProjectDefinition: projectDefinition
]

{ #category : 'project browsing' }
RwPrjBrowserTool >> projectNamed: projectName [

	^ self _projectNamed: projectName
]

{ #category : 'class browsing' }
RwPrjBrowserTool >> removeClassNamed: className [

	| projectSetDefinition loadedClass loadedPackage projectDef packageDef classDef |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedClass := self _loadedClassNamed: className.
	loadedPackage := loadedClass loadedPackage.
	projectDef := loadedClass loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	classDef := packageDef classDefinitions at: loadedClass name.
	packageDef removeClassDefinition: classDef.
	projectSetDefinition addProject: projectDef.

	(self _loadedClassExtensionsNamed: className ifAbsent: [ #() ])
		do: [ :loadedClassExtension | 
			| loadedClassExtensionPackage ceProjectDef cePackageDef |
			loadedClassExtensionPackage := loadedClassExtension loadedPackage.
			ceProjectDef := loadedClassExtension loadedProject asDefinition.
			ceProjectDef name = projectDef name
				ifTrue: [ ceProjectDef := projectDef ]
				ifFalse: [ projectSetDefinition addProject: ceProjectDef ].
			cePackageDef := ceProjectDef packageNamed: loadedClassExtensionPackage name.
			cePackageDef
				removeClassExtension:
					(cePackageDef classExtensions at: loadedClassExtension name) ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition
]

{ #category : 'method browsing' }
RwPrjBrowserTool >> removeMethod: methodSelector forClassNamed: className isMeta: isMeta [

	"Remove the given method selector from the class named className"

	| loadedMethodToBeRemoved loadedPackage loadedClassOrExtension projectDef packageDef crDef |
	loadedMethodToBeRemoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta.
	loadedPackage := loadedMethodToBeRemoved loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , loadedPackage name printString , '.' ].
	projectDef := loadedPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	crDef := loadedClassOrExtension isLoadedClass
		ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
		ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
	isMeta
		ifTrue: [ crDef removeClassMethod: methodSelector ]
		ifFalse: [ crDef removeInstanceMethod: methodSelector ].
	Rowan projectTools load loadProjectDefinition: projectDef
]
