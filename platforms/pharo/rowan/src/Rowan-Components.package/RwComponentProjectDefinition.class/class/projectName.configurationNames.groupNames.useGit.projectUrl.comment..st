deprecated
projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment


	^ self newForProjectReference: (RwProjectReferenceDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: useGit 
			projectUrl: projectUrl 
			comment: comment)