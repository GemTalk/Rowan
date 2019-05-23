accessing
classExtensionDefinitionNamed: className ifAbsent: absentBlock

	^ self classExtensions at: className ifAbsent: absentBlock