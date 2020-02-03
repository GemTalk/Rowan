Extension { #name : 'RwPrjBrowserTool' }

{ #category : '*rowan-tools-corev1' }
RwPrjBrowserTool >> createGitPackageProjectNamed: projectName [

	self createGitPackageProjectNamed: projectName updateDefinition: [  ]
]

{ #category : '*rowan-tools-corev1' }
RwPrjBrowserTool >> createGitPackageProjectNamed: projectName updateDefinition: updateBlock [

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	updateBlock cull: projectDefinition.
	self class load loadProjectDefinition: projectDefinition

]
