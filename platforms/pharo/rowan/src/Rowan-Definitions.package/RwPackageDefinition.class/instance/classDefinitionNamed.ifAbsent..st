accessing
classDefinitionNamed: className ifAbsent: absentBlock

	^ self classDefinitions at: className ifAbsent: absentBlock