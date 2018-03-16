Extension { #name : 'Class' }

{ #category : '*rowan-gemstone' }
Class >> _rwClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: symbolDictionary category: category options: optionsArray [

	^ self
		_rwClassDefinition: className
		type: type
		superclass: superclassName
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: symbolDictionary
		category: category
		packageName: category
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> _rwClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: symbolDictionary category: category packageName: packageName options: optionsArray [

	| projectTools browserTool loadedPackage loadedProject loadedClass |
	projectTools := Rowan projectTools.
	browserTool := projectTools browser.
	loadedPackage := browserTool
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	loadedProject := loadedPackage loadedProject.
	symbolDictionary name
		=
			((loadedProject specification platformSpec at: 'gemstone')
				symbolDictNameForPackageNamed: packageName)
		ifFalse: [ 
			self
				error:
					'The symbol dictionary specified for the new class does not match the symbol dictionary specified by the Rowan project' ].
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	loadedClass := browserTool _loadedClassNamed: className ifAbsent: [  ].
	loadedClass
		ifNil: [ 
			| classDef |
			"create a new class"
			classDef := RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: anArrayOfStrings
				classinstvars: anArrayOfClassInstVars
				classvars: anArrayOfClassVars
				category: category
				comment: nil
				pools: #()
				type: type.
			classDef gs_options: optionsArray.
			browserTool createClass: classDef inPackageNamed: packageName ]
		ifNotNil: [ 
			projectTools edit
				definitionsForClassNamed: className
				ifFound: [ :classDef :packageDef :projectDef | 
					| classProjectDef projects projectSetDef |
					classProjectDef := projectDef.
					projectSetDef := RwProjectSetDefinition new.
					projectSetDef addProject: classProjectDef.
					classDef
						superclassName: superclassName;
						instVarNames: anArrayOfStrings;
						classVarNames: anArrayOfClassVars;
						classInstVarNames: anArrayOfClassInstVars;
						gs_options: optionsArray;
						yourself.
					loadedClass packageName = packageName
						ifFalse: [ 
							| thePackageDef |
							"move class to another package"
							packageDef removeClassDefinition: classDef.
							thePackageDef := classProjectDef
								packageNamed: packageName
								ifAbsent: [ 
									| packageProjectDef |
									"loadedPackage in a different project"
									packageProjectDef := loadedPackage loadedProject asDefinition.
									projectSetDef addProject: packageProjectDef.
									packageProjectDef packageNamed: packageName ].
							thePackageDef addClassDefinition: classDef.
							projectTools load loadProjectSetDefinition: projectSetDef ] ]
				ifAbsent: [ 
					"shouldn't happen ... loadedClass known to exist"
					self error: 'unexpected missing class ' , className printString ] ].
	^ Rowan image objectNamed: className
]

{ #category : '*rowan-gemstone' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionaryName category: aCategoryName options: optionsArray [

	^ self
		_rwClassDefinition: aString
		type: 'bytes'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwByteSubclass: aString classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionaryName category: aCategoryName packageName: packageName options: optionsArray [

	^ self
		_rwClassDefinition: aString
		type: 'bytes'
		superclass: self name asString
		instVarNames: #()
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		packageName: packageName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionaryName category: aCategoryName options: optionsArray [

	^ self
		_rwClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwIndexableSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionaryName category: aCategoryName packageName: packageName options: optionsArray [

	^ self
		_rwClassDefinition: aString
		type: 'variable'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		packageName: packageName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionaryName category: aCategoryName options: optionsArray [

	^ self
		_rwClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		options: optionsArray
]

{ #category : '*rowan-gemstone' }
Class >> rwSubclass: aString instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionaryName category: aCategoryName packageName: packageName options: optionsArray [

	^ self
		_rwClassDefinition: aString
		type: 'normal'
		superclass: self name asString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: aDictionaryName
		category: aCategoryName
		packageName: packageName
		options: optionsArray
]
