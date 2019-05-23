accessing
projectHome: projectHomeFileReferenceOrString

	self repositoryDefinition: nil. "changing project home invalidates the current repository definition"
	projectHomeFileReferenceOrString
		ifNil: [ ^ self properties removeKey: 'projectHome' ifAbsent: [] ].
	self properties at: 'projectHome' put: projectHomeFileReferenceOrString asFileReference.