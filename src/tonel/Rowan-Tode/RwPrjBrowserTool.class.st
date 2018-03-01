Class {
	#name : 'RwPrjBrowserTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'method creation' }
RwPrjBrowserTool >> addMethod: methodSource inProtocol: protocol forClassNamed: className isMeta: isMeta inPackageNamed: packageName [

	"If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package."

	| projectTools loadedPackage classExtensionDef methodDef updateBlock projectDefinition packageDefinition |
	projectTools := Rowan projectTools.

	methodDef := RwMethodDefinition newForSource: methodSource protocol: protocol.

	updateBlock := [ :cDef :pDef | 
	isMeta
		ifTrue: [ cDef updateClassMethodDefinition: methodDef ]
		ifFalse: [ cDef updateInstanceMethodDefinition: methodDef ].
	^ projectTools load loadProjectDefinition: pDef ].

	projectTools edit
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			packageDef key = packageName
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

{ #category : 'class creation' }
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

{ #category : 'class creation' }
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
