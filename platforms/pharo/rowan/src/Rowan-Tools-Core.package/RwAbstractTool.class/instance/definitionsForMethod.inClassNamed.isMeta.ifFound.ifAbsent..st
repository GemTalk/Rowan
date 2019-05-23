class lookup
definitionsForMethod: selector inClassNamed: className isMeta: isMeta ifFound: foundBlock ifAbsent: absentBlock

	^ Rowan image
		loadedMethod: selector
		inClassNamed: className
		isMeta: isMeta
		ifFound: [ :loadedMethod | 
			| loadedPackage loadedClass projectDef packageDef classDef classExtensionDef |
			loadedPackage := loadedMethod loadedPackage.
			loadedClass := loadedMethod loadedClass.
			projectDef := loadedPackage loadedProject asDefinition.
			packageDef := projectDef packageNamed: loadedPackage name.
			loadedClass isLoadedClass
				ifTrue: [ classDef := packageDef classDefinitions at: loadedClass name ]
				ifFalse: [ classExtensionDef := packageDef classExtensions at: loadedClass name ].
			foundBlock
				value: loadedMethod asDefinition
				value: classDef
				value: classExtensionDef
				value: packageDef
				value: projectDef.
			^ self ]
		ifAbsent: absentBlock