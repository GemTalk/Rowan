class browsing
addOrUpdateClassDefinition: className type: type superclass: superclassName instVarNames: anArrayOfStrings classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts category: category packageName: packageName constraints: constraintsArray options: optionsArray

	| loadedPackage loadedProject loadedClass theOptions theConstraints |
	loadedPackage := self
		_loadedPackageNamed: packageName
		ifAbsent: [ self error: 'No package named ' , packageName printString , ' found' ].
	loadedProject := loadedPackage loadedProject.
	anArrayOfPoolDicts isEmpty
		ifFalse: [ self error: 'poolDictionaries not supported yet' ].
	loadedClass := self _loadedClassNamed: className ifAbsent: [  ].
	theOptions := optionsArray collect: [:each | each asString ].
	(constraintsArray isKindOf: Array)
		ifTrue: [
			theConstraints := {}.
			constraintsArray do: [:ar |
				(ar isKindOf: Array)
					ifTrue: [ theConstraints add: { (ar at: 1) asString . (ar at: 2) name asString } ]
					ifFalse: [ theConstraints add: ar name asString ] ] ]
		ifFalse: [
			theConstraints := constraintsArray
				ifNil: [ #() ]
				ifNotNil: [ constraintsArray ]].
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
			classDef gs_options: theOptions.
			classDef gs_constraints: theConstraints.
			self createClass: classDef inPackageNamed: packageName ]
		ifNotNil: [ 
			self class edit
				definitionsForClassNamed: className
				ifFound: [ :classDef :packageDef :projectDef | 
					| classProjectDef projectSetDef |
					classProjectDef := projectDef.
					projectSetDef := RwProjectSetDefinition new.
					projectSetDef addProject: classProjectDef.
					classDef
						superclassName: superclassName;
						instVarNames: anArrayOfStrings;
						classVarNames: anArrayOfClassVars;
						classInstVarNames: anArrayOfClassInstVars;
						gs_options: theOptions;
						gs_constraints: theConstraints;
						category: category;
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
							thePackageDef addClassDefinition: classDef ].
					self class load loadProjectSetDefinition: projectSetDef ]
				ifAbsent: [ 
					"shouldn't happen ... loadedClass known to exist"
					self error: 'unexpected missing class ' , className printString ] ].
	^ (self _loadedClassNamed: className ifAbsent: [  ]) handle currentVersion
