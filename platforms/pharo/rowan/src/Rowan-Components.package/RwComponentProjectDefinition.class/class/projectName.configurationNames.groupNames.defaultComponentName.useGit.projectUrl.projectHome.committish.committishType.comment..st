deprecated
projectName: projectName configurationNames: configurationNames groupNames: groupNames defaultComponentName: defaultComponentName useGit: useGit projectUrl: projectUrl projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType comment: comment


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