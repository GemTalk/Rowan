Extension { #name : 'RwComponentProjectDefinition' }

{ #category : '*rowan-definitions-deprecated' }
RwComponentProjectDefinition >> repoPath [

	self deprecated: 'Use packagesPath instead'.
	^ self packagesPath

]

{ #category : '*rowan-definitions-deprecated' }
RwComponentProjectDefinition >> repoPath: astring [

	self deprecated: 'Use packagesPath: instead'.
	^ self packagesPath: astring
]

{ #category : '*rowan-definitions-deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames defaultComponentName: defaultComponentName useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment [


	self deprecated: 'pick again:)'.
	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			defaultComponentName: defaultComponentName
			useGit: useGit 
			projectUrl: projectUrl 
			projectHome: projectHomeFileReferenceOrString
			committish: committish 
			committishType: committishType 
			comment: comment)
]

{ #category : '*rowan-definitions-deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment [


	self deprecated: 'pick again:)'.
	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			comment: comment)
]

{ #category : '*rowan-definitions-deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment [

	self deprecated: 'pick again:)'.
	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			committish: committish 
			committishType: committishType 
			comment: comment)
]

{ #category : '*rowan-definitions-deprecated' }
RwComponentProjectDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment [


	self deprecated: 'pick again:)'.
	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			projectHome: projectHomeFileReferenceOrString
			committish: committish 
			committishType: committishType 
			comment: comment)
]
