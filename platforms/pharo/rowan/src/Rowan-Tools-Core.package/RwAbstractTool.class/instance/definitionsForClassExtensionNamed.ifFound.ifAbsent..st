class lookup
definitionsForClassExtensionNamed: className ifFound: foundBlock ifAbsent: absentBlock

	"validate loaded class extension registry structure"

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | 
			loadedClassExtensionSet
				do: [ :loadedClassExtension | 
					| loadedPackage projectDef packageDef classExtensionDef |
					loadedPackage := loadedClassExtension loadedPackage.
					projectDef := loadedPackage loadedProject asDefinition.
					packageDef := projectDef packageNamed: loadedPackage name.
					classExtensionDef := packageDef classExtensions
						at: loadedClassExtension name.
					foundBlock value: classExtensionDef value: packageDef value: projectDef ].
			^ self ]
		ifAbsent: absentBlock