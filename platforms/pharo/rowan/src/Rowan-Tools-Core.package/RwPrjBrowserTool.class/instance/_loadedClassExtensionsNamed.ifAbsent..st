private
_loadedClassExtensionsNamed: className ifAbsent: absentBlock

	^ Rowan image
		loadedClassExtensionsNamed: className
		ifFound: [ :loadedClassExtensionSet | ^ loadedClassExtensionSet ]
		ifAbsent: absentBlock