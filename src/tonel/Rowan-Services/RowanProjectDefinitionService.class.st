Class {
	#name : 'RowanProjectDefinitionService',
	#superclass : 'RowanDefinitionService',
	#instVars : [
		'name',
		'sha',
		'branch'
	],
	#category : 'Rowan-Services'
}

{ #category : 'other' }
RowanProjectDefinitionService >> branch [

	^branch

]

{ #category : 'other' }
RowanProjectDefinitionService >> branch: anObject [

	branch := anObject

]

{ #category : 'examples' }
RowanProjectDefinitionService >> createProjectNamed: projectName [ 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.

]

{ #category : 'examples' }
RowanProjectDefinitionService >> createProjectNamed: projectName in: symbolDictionaryName [

	self rowanFixMe. "Dale doesn't like Rowan projectNames"
	(Rowan projectNames includes: projectName) ifFalse:[
		self browserTool createGitPackageProjectNamed: projectName updateDefinition: [:pd | 
				pd defaultSymbolDictName: symbolDictionaryName; comment:  'Sample Rowan Project'] ].

]

{ #category : 'examples' }
RowanProjectDefinitionService >> createSampleProject [

	self removeProjectNamed: self sampleProjectName.
	self createSampleSymbolDictionary.
	^self createProjectNamed: self sampleProjectName in: self sampleSymbolDictionaryName

]

{ #category : 'other' }
RowanProjectDefinitionService >> name [

	^name

]

{ #category : 'other' }
RowanProjectDefinitionService >> name: anObject [

	name := anObject

]

{ #category : 'rowan' }
RowanProjectDefinitionService >> projectNames [

	^Rowan projectNames collect: [:string | self class new name: string]

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

{ #category : 'other' }
RowanProjectDefinitionService >> sha [

	^sha

]

{ #category : 'other' }
RowanProjectDefinitionService >> sha: anObject [

	sha := anObject

]
