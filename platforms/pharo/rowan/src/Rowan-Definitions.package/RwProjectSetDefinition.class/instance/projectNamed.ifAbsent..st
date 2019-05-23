accessing
projectNamed: projectName ifAbsent: absentBlock

	^ self definitionWithKey: projectName ifAbsent: absentBlock
