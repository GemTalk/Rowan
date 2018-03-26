Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#category : 'Rowan-Services'
}

{ #category : 'examples' }
RowanProjectDefinitionService >> createProjectNamed: projectName [ 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.

]

{ #category : 'examples' }
RowanProjectDefinitionService >> createProjectNamed: projectName in: symbolDictionaryName [

	| projectDefinition |
	projectDefinition := (Rowan loadedProjectNamed: projectName ifAbsent:[
		self projectTools load loadProjectDefinition: (RwProjectDefinition newForGitPackageProjectNamed: projectName)]) asDefinition.
	projectDefinition 
		comment: 'Sample Rowan Project';
		defaultSymbolDictName: symbolDictionaryName.
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
