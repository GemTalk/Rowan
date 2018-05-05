Class {
	#name : 'RowanProjectService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'sha',
		'branch'
	],
	#category : 'Rowan-Services'
}

{ #category : 'accessing' }
RowanProjectService class >> defaultProjectName [

	^RowanServicePreferences current defaultProjectName

]

{ #category : 'accessing' }
RowanProjectService class >> defaultProjectName: aString [

	RowanServicePreferences current defaultProjectName: aString

]

{ #category : 'other' }
RowanProjectService >> branch [

	^(RwProject newNamed: name) currentBranchName

]

{ #category : 'rowan' }
RowanProjectService >> branches [

	| project branches |

	project := (RwProject newNamed: name). 
	branches := Rowan gitTools gitbranchIn: project repositoryRootPath with: ''.
	^branches

]

{ #category : 'rowan' }
RowanProjectService >> changes [
	
	^(Rowan projectTools diff diffForProjectName: name) asString

]

{ #category : 'rowan' }
RowanProjectService >> checkout: branchName [

	| project branches |

	project := (RwProject newNamed: name). 
	branches := Rowan gitTools gitcheckoutIn: project repositoryRootPath with: branchName.
	^branches

]

{ #category : 'rowan' }
RowanProjectService >> commitWithMessage: message [
	
	Rowan projectTools write writeProjectNamed: name.
	Rowan projectTools commit
		commitProjectNamed: name
		message: message. 
	System commitTransaction.

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
RowanProjectService >> isDirty [

	^(RwProject newNamed: name) isDirty

]

{ #category : 'rowan' }
RowanProjectService >> isSkew [

	^self sha ~= self repositorySha

]

{ #category : 'rowan' }
RowanProjectService >> loadProjectNamed: aName [

	[Rowan projectTools load loadProjectNamed: aName withConfiguration: 'Default'] 
		on: RwExistingAssociationWithSameKeyNotification, Warning
		do: [ :ex | (ex isKindOf: Warning) ifTrue:[Transcript cr; show: ex description]. ex resume ]	

]

{ #category : 'rowan' }
RowanProjectService >> log [

	^Rowan projectTools log
		commitLogProjectNamed: name
		limit: 25

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
RowanProjectService >> packageNames [
	^(Rowan image loadedProjectNamed: name) packageNames.

]

{ #category : 'rowan' }
RowanProjectService >> packageNameString [
		"return a string showing the package names for a project"

	| ws packageNames |
	ws := WriteStream on: String new.
	ws
		nextPutAll: 'Packages for project: ' , name;
		cr.
	packageNames := Set new.
	packageNames addAll: (Rowan image loadedProjectNamed: name) packageNames.
	packageNames asSortedCollection do: 
			[:packageName |
			ws
				cr; 
				tab;
				nextPutAll: packageName
			].
	^ws contents

]

{ #category : 'rowan' }
RowanProjectService >> projectNames [

	^Rowan projectNames collect: [:string | self class new name: string]

]

{ #category : 'rowan' }
RowanProjectService >> pullFromGit [

	| project |
	project := RwProject newNamed: name. 
	Rowan gitTools
		gitpullIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName

]

{ #category : 'rowan' }
RowanProjectService >> pushToGit [

	| project |
	project := RwProject newNamed: name. 
	Rowan gitTools
		gitpushIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName

]

{ #category : 'rowan' }
RowanProjectService >> removeProjectNamed: projectName [
   "remove project"
 
    (Rowan loadedProjectNamed: projectName ifAbsent: [  ])
      ifNotNil: [ :project | Rowan image _removeLoadedProject: project ]

]

{ #category : 'rowan' }
RowanProjectService >> repositorySha [

	^(RwProject newNamed: name) repositoryCommitId

]

{ #category : 'examples' }
RowanProjectService >> sampleProjectName [
	
	^'SampleProjectName'

]

{ #category : 'other' }
RowanProjectService >> sha [

	^(RwProject newNamed: name) loadedCommitId

]

{ #category : 'rowan' }
RowanProjectService >> unload [

	| loadedProject |
	loadedProject := Rowan image loadedProjects select:[:proj | proj name = name].

]

{ #category : 'rowan' }
RowanProjectService >> write [
	Rowan projectTools write writeProjectNamed: name

]
