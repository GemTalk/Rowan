queries
classOrExtensionForClassNamed: className ifAbsent: absentBlock

	^loadedClasses at: className
		ifAbsent: [loadedClassExtensions at: className ifAbsent: absentBlock]