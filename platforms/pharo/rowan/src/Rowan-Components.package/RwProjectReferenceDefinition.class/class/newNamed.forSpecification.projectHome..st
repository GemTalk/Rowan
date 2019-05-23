instance creation
newNamed: projectName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString

	^ self
		projectName: projectName
			configurationNames: aRwSpecification defaultConfigurationNames
			groupNames: aRwSpecification defaultGroupNames
			defaultComponentName: aRwSpecification defaultComponentName
			useGit: aRwSpecification useGit
			projectUrl: aRwSpecification projectUrl
			comment: aRwSpecification comment
			projectHome: projectHomeFileReferenceOrString
			committish: aRwSpecification repoSpec committish
			committishType: aRwSpecification repoSpec committishType
			configsPath: aRwSpecification configsPath
			packagesPath: aRwSpecification repoPath
			projectsPath: aRwSpecification projectsPath
			specsPath: aRwSpecification specsPath