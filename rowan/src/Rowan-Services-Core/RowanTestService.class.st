Class {
	#name : 'RowanTestService',
	#superclass : 'RowanService',
	#category : 'Rowan-Services-Core'
}

{ #category : 'command' }
RowanTestService >> createNewTestProjectNamed: projectName [

	| proj |
	proj := RwProjectDefinition newForGitBasedProjectNamed: projectName.
	proj repositoryRootPath: '$ROWAN_PROJECTS_HOME/', projectName.
	Rowan projectTools create createProjectFor: proj.
	Rowan projectTools load loadProjectDefinition: proj.
	^proj
]
