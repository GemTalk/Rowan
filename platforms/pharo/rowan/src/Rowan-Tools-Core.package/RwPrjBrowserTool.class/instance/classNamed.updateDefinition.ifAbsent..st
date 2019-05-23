definition updating
classNamed: className updateDefinition: updateBlock ifAbsent: absentBlock

	| projectDefinition classDefinition |
	self class edit
		definitionsForClassNamed: className
		ifFound: [ :classDef :packageDef :projectDef | 
			projectDefinition := projectDef.
			classDefinition := classDef ]
		ifAbsent: [ ^ absentBlock value ].

	updateBlock value: classDefinition.
	self class load loadProjectDefinition: projectDefinition