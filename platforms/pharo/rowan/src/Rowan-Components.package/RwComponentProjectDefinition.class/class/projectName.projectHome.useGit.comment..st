instance creation
projectName: projectName projectHome: projectHomeFileReferenceOrString useGit: useGit comment: comment

	| projectRef |
	projectRef := RwProjectReferenceDefinition new
		projectName: projectName;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		yourself.
	^ self newForProjectReference: projectRef