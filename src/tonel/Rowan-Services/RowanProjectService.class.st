Class {
	#name : 'RowanProjectService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'sha',
		'branch'
	],
	#classVars : [
		'DefaultProjectName'
	],
	#category : 'Rowan-Services'
}

{ #category : 'accessing' }
RowanProjectService class >> defaultProjectName [

	^DefaultProjectName

]

{ #category : 'accessing' }
RowanProjectService class >> defaultProjectName: aString [

	DefaultProjectName := aString

]

{ #category : 'other' }
RowanProjectService >> branch [

	^branch

]

{ #category : 'other' }
RowanProjectService >> branch: anObject [

	branch := anObject

]

{ #category : 'rowan' }
RowanProjectService >> changes [
	
	^(Rowan projectTools diff diffSpecUrl: name) asString.

]

{ #category : 'examples' }
RowanProjectService >> createProjectNamed: projectName [ 

	^self createProjectNamed: projectName in: self sampleSymbolDictionaryName.

]

{ #category : 'examples' }
RowanProjectService >> createProjectNamed: projectName in: symbolDictionaryName [

	self rowanFixMe. "Dale doesn't like Rowan projectNames"
	(Rowan projectNames includes: projectName) ifFalse:[
		self browserTool createGitPackageProjectNamed: projectName updateDefinition: [:pd | 
				pd defaultSymbolDictName: symbolDictionaryName; comment:  'Sample Rowan Project'] ].

]

{ #category : 'examples' }
RowanProjectService >> createSampleProject [

	self removeProjectNamed: self sampleProjectName.
	self createSampleSymbolDictionary.
	^self createProjectNamed: self sampleProjectName in: self sampleSymbolDictionaryName

]

{ #category : 'accessing' }
RowanProjectService >> defaultProjectName [

	^self class defaultProjectName

]

{ #category : 'accessing' }
RowanProjectService >> defaultProjectName: aString [

	self class defaultProjectName: aString

]

{ #category : 'rowan' }
RowanProjectService >> definition [

	^(Rowan image loadedProjectNamed: name) asDefinition

]

{ #category : 'rowan' }
RowanProjectService >> loadProjectNamed: aName [

		Rowan projectTools load loadProjectNamed: aName

]

{ #category : 'other' }
RowanProjectService >> name [

	^name

]

{ #category : 'other' }
RowanProjectService >> name: anObject [

	name := anObject

]

{ #category : 'rowan' }
RowanProjectService >> newGitProject: url root: rootPath useSsh: useSsh [
	"set useSsh to false to clone using https:"

	Rowan projectTools clone
		cloneSpecUrl: url
		gitRootPath: rootPath
		useSsh: useSsh.

]

{ #category : 'rowan' }
RowanProjectService >> projectNames [

	^Rowan projectNames collect: [:string | self class new name: string]

]

{ #category : 'examples' }
RowanProjectService >> removeProjectNamed: projectName [ 
	(Rowan loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

]

{ #category : 'examples' }
RowanProjectService >> sampleProjectName [
	
	^'SampleProjectName'

]

{ #category : 'other' }
RowanProjectService >> sha [

	^sha

]

{ #category : 'other' }
RowanProjectService >> sha: anObject [

	sha := anObject

]
