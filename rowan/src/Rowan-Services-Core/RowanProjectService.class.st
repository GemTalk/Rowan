Class {
	#name : 'RowanProjectService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'sha',
		'branch',
		'isSkew',
		'isDirty',
		'packages',
		'changes',
		'existsOnDisk',
		'isLoaded'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanProjectService class >> defaultProjectName [

	^RowanServicePreferences current defaultProjectName

]

{ #category : 'accessing' }
RowanProjectService class >> defaultProjectName: aString [

	RowanServicePreferences current defaultProjectName: aString

]

{ #category : 'instance creation' }
RowanProjectService class >> newNamed: aString [

	| inst |
	inst := self new.
	inst name: aString.
	inst refresh.
	^inst
]

{ #category : 'accessing' }
RowanProjectService >> _isSkew [

	^isSkew
]

{ #category : 'comparing' }
RowanProjectService >> = projectService [
	^projectService isProjectService ifTrue: [name = projectService name] ifFalse: [^false]
]

{ #category : 'client commands' }
RowanProjectService >> addPackageNamed: packageName [

	Rowan image loadedPackageNamed: packageName ifAbsent: [
		self browserTool addPackageNamed: packageName toProjectNamed: name. 
		self update.
		^self answer: #added.].
	self answer: #duplicatePackage
]

{ #category : 'initialization' }
RowanProjectService >> basicRefresh [
	(isLoaded := self projectIsLoaded) ifFalse:[
		existsOnDisk := false. 
		updateType := #removedProject:. 
		^RowanCommandResult addResult: self]. 
	isDirty := self isDirty. 
	self setExistsOnDisk.
	isSkew := self isSkew.
	sha := self rowanSha.
	branch := self rowanBranch.
	RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanProjectService >> branch [

	name isNil ifTrue:[^String new].
	^(RwProject newNamed: name) currentBranchName

]

{ #category : 'accessing' }
RowanProjectService >> branch: anObject [

	branch := anObject

]

{ #category : 'client commands' }
RowanProjectService >> changes [

	| projectService jadeServer |
	jadeServer := Rowan platform jadeServerClassNamed: #JadeServer. 
	changes := Array new. 
	self packageNames do:[:packageName |
			| patch |
			patch := Rowan packageTools diff patchForPackageName: packageName.
			changes add:(jadeServer new
				_mcDescriptionOfPatch: patch
				baseName: 'closest ancestor'
				alternateName: nil)].
	self refresh.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanProjectService >> checkout: branchName [

	| project branches |

	project := (RwProject newNamed: name). 
	branches := Rowan gitTools gitcheckoutIn: project repositoryRootPath with: branchName.
	^branches

]

{ #category : 'client commands' }
RowanProjectService >> commitWithMessage: message [
	
	Rowan projectTools write writeProjectNamed: name.
	Rowan projectTools commit
		commitProjectNamed: name
		message: message.
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

{ #category : 'accessing' }
RowanProjectService >> defaultProjectName [

	^self class defaultProjectName

]

{ #category : 'accessing' }
RowanProjectService >> defaultProjectName: aString [

	self class defaultProjectName: aString

]

{ #category : 'accessing' }
RowanProjectService >> existsOnDisk [

	^existsOnDisk
]

{ #category : 'comparing' }
RowanProjectService >> hash [
	^self name hash
]

{ #category : 'initialization' }
RowanProjectService >> initialize [

	super initialize. 
	packages := Array new
]

{ #category : 'rowan' }
RowanProjectService >> isDirty [

	name isNil ifTrue:[^false].
	^(RwProject newNamed: name) isDirty

]

{ #category : 'accessing' }
RowanProjectService >> isDirty: aBoolean [

	isDirty := aBoolean.

]

{ #category : 'rowan' }
RowanProjectService >> isSkew [
	| repositorySha |
	name isNil ifTrue:[^false].
	repositorySha := [self repositorySha] on: Error do:[:ex | repositorySha := 'not on disk'].
	^self sha ~= repositorySha
]

{ #category : 'client commands' }
RowanProjectService >> loadProjectNamed: aName [

	[Rowan projectTools load loadProjectNamed: aName] 
		on: Warning
		do: [ :ex | Transcript cr; show: ex description. ex resume ].
	RowanBrowserService new updateProjects.
]

{ #category : 'rowan' }
RowanProjectService >> log [

	^Rowan projectTools log
		commitLogProjectNamed: name
		limit: 25

]

{ #category : 'accessing' }
RowanProjectService >> name [

	^name

]

{ #category : 'accessing' }
RowanProjectService >> name: anObject [

	name := anObject

]

{ #category : 'client commands' }
RowanProjectService >> newGitProject: url root: rootPath useSsh: useSsh [
	"set useSsh to false to clone using https:"

	Rowan projectTools clone
		cloneSpecUrl: url
		gitRootPath: rootPath
		useSsh: useSsh.
	RowanBrowserService new updateProjects.
]

{ #category : 'rowan' }
RowanProjectService >> packageNames [
	"if no project is selected, return all package names"
	^name isNil ifTrue:[
		"list of visible packageNames for current user"
		Rowan image packageNames ]
	ifFalse:[
		"list of visible packageNames for current user and named project"
		Rowan image packageNamesForLoadedProjectNamed: name ]
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

{ #category : 'accessing' }
RowanProjectService >> packageServices [

	^self packageNames collect:[:packageName | RowanPackageService forPackageNamed: packageName]

]

{ #category : 'printing' }
RowanProjectService >> printOn: aStream [

	super printOn: aStream. 
	aStream nextPut: $:. 
	aStream nextPutAll: (name ifNil: [nil printString])
]

{ #category : 'testing' }
RowanProjectService >> projectIsLoaded [

	^(Rowan image
		loadedProjectNamed: name
		ifAbsent: []) notNil
]

{ #category : 'rowan' }
RowanProjectService >> projects [

		^Rowan projectNames collect: 
			[:string |
			| service |
			service := self class new name: string. 
			service 
				sha: service rowanSha;
				branch: service rowanBranch;
				isDirty: service rowanDirty]

]

{ #category : 'client commands' }
RowanProjectService >> pullFromGit [

	| project |
	project := RwProject newNamed: name. 
	Rowan gitTools
		gitpullIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName

]

{ #category : 'client commands' }
RowanProjectService >> pushToGit [

	| project |
	project := RwProject newNamed: name. 
	Rowan gitTools
		gitpushIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName

]

{ #category : 'initialization' }
RowanProjectService >> refresh [
	self basicRefresh. 
	isLoaded ifTrue:[
		packages := self packageServices].
]

{ #category : 'client commands' }
RowanProjectService >> reloadProject [

	[Rowan projectTools load loadProjectNamed: name] 
		on: Warning
		do: [ :ex | Transcript cr; show: ex description. ex resume ]
]

{ #category : 'rowan' }
RowanProjectService >> removeProjectNamed: projectName [
   "remove project"
 
    (Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
      ifNotNil: [ :project | Rowan image _removeLoadedProject: project ]

]

{ #category : 'rowan' }
RowanProjectService >> repositorySha [

	^(RwProject newNamed: name) repositoryCommitId

]

{ #category : 'rowan' }
RowanProjectService >> rowanBranch [
	
	name isNil ifTrue:[^String new].
	^ [ (RwProject newNamed: name) currentBranchName ] on: Error do: [:ex | ^'ERROR getting repository branch' ]

]

{ #category : 'rowan' }
RowanProjectService >> rowanDirty [

	^(RwProject newNamed: name) isDirty

]

{ #category : 'rowan' }
RowanProjectService >> rowanProjectName [

	^name
]

{ #category : 'rowan' }
RowanProjectService >> rowanSha [

	name isNil ifTrue:[^0].
	^(RwProject newNamed: name) loadedCommitId

]

{ #category : 'rowan' }
RowanProjectService >> rowanSkew [

	^self sha ~= self repositorySha

]

{ #category : 'perform' }
RowanProjectService >> servicePerform: symbol withArguments: collection [
	super servicePerform: symbol withArguments: collection.
	self update.
]

{ #category : 'client commands' }
RowanProjectService >> setDefaultProject [

	self class defaultProjectName: name
]

{ #category : 'rowan' }
RowanProjectService >> setExistsOnDisk [
	"might be a better test than #repositorySha for
	determining if a project exists on disk." 

	existsOnDisk := (RwProject newNamed: name) existsOnDisk.
]

{ #category : 'accessing' }
RowanProjectService >> sha [

	name isNil ifTrue:[^0].
	^(RwProject newNamed: name) loadedCommitId

]

{ #category : 'accessing' }
RowanProjectService >> sha: anObject [

	"because skew is intimately associated with sha
	set it here" 
	sha := anObject.
	isSkew := self rowanSkew

]

{ #category : 'rowan' }
RowanProjectService >> unload [

	| loadedProject |
	loadedProject := Rowan image loadedProjects select:[:proj | proj name = name].

]

{ #category : 'update' }
RowanProjectService >> update [
	self refresh.
]

{ #category : 'update' }
RowanProjectService >> updateInternalService: updatedService [

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 

	1 to: packages size do:[:index |
		| packageService |
		packageService := packages at: index. 
		packageService = updatedService ifTrue:[
			packages at: index put: updatedService
		]].
]

{ #category : 'client commands' }
RowanProjectService >> write [
	Rowan projectTools write writeProjectNamed: name

]
