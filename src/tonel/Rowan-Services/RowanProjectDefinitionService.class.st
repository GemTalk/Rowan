Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#category : 'Rowan-Services'
}

{ #category : 'examples' }
RowanProjectDefinitionService >> createProjectNamed: projectName [ 

	| projectDefinition |

	projectDefinition := RwProjectDefinition 
		newForGitPackageProjectNamed: projectName.
	projectDefinition 
		comment: 'Sample Rowan Project';
		defaultSymbolDictName: self sampleSymbolDictionaryName.
	self projectTools load loadProjectDefinition: projectDefinition.
	^projectDefinition

]

{ #category : 'examples' }
RowanProjectDefinitionService >> createProjectNamed: projectName in: symbolDictionaryName [

	| projectDefinition |

	projectDefinition := RwProjectDefinition 
		newForGitPackageProjectNamed: projectName.
	projectDefinition 
		comment: 'Sample Rowan Project';
		defaultSymbolDictName: symbolDictionaryName.
	self projectTools load loadProjectDefinition: projectDefinition.
	^projectDefinition

]

{ #category : 'examples' }
RowanProjectDefinitionService >> createSampleProject [

	self removeProjectNamed: self sampleProjectName.
	self createSampleSymbolDictionary.
	^self createProjectNamed: self sampleProjectName in: self sampleSymbolDictionaryName

]

{ #category : 'examples' }
RowanProjectDefinitionService >> removeProjectNamed: projectName [ 
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | RwGsImage _removeLoadedProject: project ].

]

{ #category : 'examples' }
RowanProjectDefinitionService >> sampleProjectName [
	
	^'SampleProjectName'

]
