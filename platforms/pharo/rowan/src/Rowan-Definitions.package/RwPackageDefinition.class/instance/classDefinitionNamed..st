accessing
classDefinitionNamed: className

	^ self classDefinitionNamed: className ifAbsent: [ self error: 'No class definition found with the name ', className printString ]