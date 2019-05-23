deprecated
projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment

	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			committish: committish 
			committishType: committishType 
			comment: comment)