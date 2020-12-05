"
No class-specific documentation for RowanProjectService, hierarchy is:
Object
  RowanService( definition updates command commandArgs updateType organizer)
    RowanProjectService( rwProject name sha branch isSkew isDirty packages changes existsOnDisk isLoaded projectUrl rowanProjectsHome isDiskDirty)

"
Class {
	#name : 'RowanProjectService',
	#superclass : 'RowanService',
	#instVars : [
		'rwProject',
		'name',
		'sha',
		'branch',
		'isSkew',
		'isDirty',
		'packages',
		'changes',
		'existsOnDisk',
		'isLoaded',
		'projectUrl',
		'rowanProjectsHome',
		'isDiskDirty',
		'projectOop',
		'specService',
		'componentServices'
	],
	#category : 'Rowan-Services-Core'
}

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
	self rowanFixMe.	"remove after #addPackageNamed:toComponentNamed: is integrated in v2.0 work"
	Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self browserTool
				addPackageNamed: packageName
				toComponentNamed: 'Core'
				andProjectNamed: name.
			self update.
			^ self answer: #'added' ].
	self answer: #'duplicatePackage'
]

{ #category : 'rowan' }
RowanProjectService >> audit [
	^ (Rowan projectNamed: name) audit
]

{ #category : 'client commands support' }
RowanProjectService >> basicLoadProject: aBlock [
	| updatedProjects |
	[ updatedProjects := aBlock value ]
		on: Warning
		do: [ :ex | 
			Transcript
				cr;
				show: ex description.
			ex resume ].
	updatedProjects
		do: [ :project | 
			| projectService |
			projectService := project name = name
				ifTrue: [ self	"don't send an extraneous service back across the wire" ]
				ifFalse: [ RowanProjectService newNamed: project name ].
			projectService update.
			RowanCommandResult addResult: projectService ].
	RowanBrowserService new updateProjects
]

{ #category : 'initialization' }
RowanProjectService >> basicRefresh [
	name = Rowan unpackagedName
		ifTrue: [ 
			isLoaded := false.
			RowanBrowserService new updateDictionaries.
			^ self ].
	(isLoaded := self projectIsLoaded)
		ifFalse: [ 
			existsOnDisk := false.
			self isDefinedProject
				ifFalse: [ 
					updateType := #'removedProject:'.
					^ RowanCommandResult addResult: self ] ].
	self updateIsDirty. 
	self setExistsOnDisk.
	isSkew := self isSkew.
	sha := self rowanSha.
	branch := self rowanBranch.
	projectUrl := self rowanProjectUrl.
	rowanProjectsHome := System gemEnvironmentVariable: 'ROWAN_PROJECTS_HOME'.
	isDiskDirty := self isGitDirty.
	componentServices := self componentServices.
	specService := RowanSpecificationService new initialize: self rwProject _loadSpecification asOop.
	RowanCommandResult addResult: self
]

{ #category : 'accessing' }
RowanProjectService >> branch [

	name isNil ifTrue:[^String new].
	^self rwProject currentBranchName
]

{ #category : 'accessing' }
RowanProjectService >> branch: anObject [

	branch := anObject

]

{ #category : 'client commands' }
RowanProjectService >> changes [

	| jadeServer projectNames |
	jadeServer := Rowan platform jadeServerClassNamed: #JadeServer. 
	changes := Array new. 
	projectNames := name ifNil: [ Rowan projectNames ] ifNotNil: [ { name } ].
	projectNames do: [:aProjectName | 
		(Rowan projectTools diff
			patchesForProjectNamed: aProjectName) do: [:assoc | 
				"key is packageName, value is a CypressPatch"
				| patch |
				patch := assoc value.
				changes add:(jadeServer new
					_mcDescriptionOfPatch: patch
					baseName: 'closest ancestor'
					alternateName: nil) ] ].
	self refresh.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanProjectService >> checkout: branchName [

	| project branches |

	project := self rwProject. 
	branches := Rowan gitTools gitcheckoutIn: project repositoryRootPath asFileReference with: branchName.
	^branches
]

{ #category : 'client commands' }
RowanProjectService >> checkoutTag: tagName [
  Rowan gitTools gitcheckoutIn: self repositoryRoot with: tagName
]

{ #category : 'client commands' }
RowanProjectService >> commitWithMessage: message [
	
	Rowan projectTools write writeProjectNamed: name.
	Rowan projectTools commit
		commitProjectNamed: name
		message: message.
]

{ #category : 'accessing' }
RowanProjectService >> componentServices [
	| componentDictionary |
	componentDictionary := Dictionary new.
	componentDictionary
		at: #'nil'
		put:
			(self rwProject componentNames
				collect: [ :componentName | 
					| componentService |
					componentService := RowanComponentService
						forComponentNamed: componentName
						projectService: self.
					componentService addFlattenedHierarchyTo: componentDictionary. 
					componentService ]).

	^ componentDictionary
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

{ #category : 'replication' }
RowanProjectService >> excludedInstVars [

	^ super excludedInstVars, #( #rwProject)

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
	packages := Array new.
	isDiskDirty := false.
]

{ #category : 'rowan' }
RowanProjectService >> isDirty [
	name isNil
		ifTrue: [ ^ false ].
	^ self isDirty
]

{ #category : 'accessing' }
RowanProjectService >> isDirty: aBoolean [

	isDirty := aBoolean.

]

{ #category : 'client commands' }
RowanProjectService >> isGitDirty [
  "From https://github.com/GemTalk/Jadeite/issues/323#issuecomment-442545934"

  [ 
  ^ (Rowan gitTools gitstatusIn: self repositoryRootPath with: '--porcelain')
    isEmpty not ]
    on: Error
    do: [ :ignored | ^ false ]
]

{ #category : 'rowan' }
RowanProjectService >> isSkew [
	| repositorySha |
	name isNil ifTrue:[^false].
	self existsOnDisk ifFalse:[^false]. 
	repositorySha := [self repositorySha] on: Error do:[:ex | repositorySha := 'not on disk'].
	^self sha ~= repositorySha
]

{ #category : 'client commands' }
RowanProjectService >> loadProjectFromFile: fileString [
	self loadProjectFromFile: fileString projectsHome: nil
]

{ #category : 'client commands' }
RowanProjectService >> loadProjectFromFile: fileString projectsHome: projectsHomePath [
	self
		basicLoadProject: [ 
			((RwSpecification fromUrl: fileString)
				projectsHome: projectsHomePath;
				yourself) resolveStrict load ]
]

{ #category : 'client commands' }
RowanProjectService >> loadProjectFromFile: fileString projectsHome: projectsHomePath customConditionalAttributes: attributes componentNames: componentNames [
	self
		basicLoadProject: [ 
			((RwSpecification fromUrl: fileString)
				customConditionalAttributes: attributes;
				projectsHome: projectsHomePath;
				componentNames: componentNames;
				yourself) resolveStrict load ]
]

{ #category : 'client commands' }
RowanProjectService >> loadProjectNamed: aName [
	self basicLoadProject: [ Rowan projectTools load loadProjectNamed: aName ]
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
	"not valid in Rowan V2.0"
	"set useSsh to false to clone using https:"

	"Rowan projectTools clone
		cloneSpecUrl: url
		gitRootPath: rootPath
		useSsh: useSsh.
	(RowanBrowserService new organizer: organizer) updateProjects."
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
RowanProjectService >> packages [
	^packages
]

{ #category : 'accessing' }
RowanProjectService >> packages: object [
	packages := object
]

{ #category : 'accessing' }
RowanProjectService >> packageServices [

	^self packageNames collect:[:packageName | RowanPackageService forPackageNamed: packageName]

]

{ #category : 'client commands' }
RowanProjectService >> performGitCommand: gitCommand with: argsString [
  | project |
  project := RwProject newNamed: name.
  Rowan gitTools
    performGitCommand: gitCommand
    in: project repositoryRootPath
    with: argsString.
  RowanCommandResult addResult: self
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

{ #category : 'accessing' }
RowanProjectService >> projectOop [
	^projectOop
]

{ #category : 'accessing' }
RowanProjectService >> projectOop: object [
	projectOop := object
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

{ #category : 'accessing' }
RowanProjectService >> projectUrl [

	^projectUrl
]

{ #category : 'accessing' }
RowanProjectService >> projectUrl: anObject [

	projectUrl := anObject
]

{ #category : 'client commands' }
RowanProjectService >> pullFromGit [

	| project |
	project := self rwProject. 
	Rowan gitTools
		gitpullIn: project repositoryRootPath
		remote: project remote
		branch: project currentBranchName
]

{ #category : 'client commands' }
RowanProjectService >> pushToGit [

	| project |
	project := self rwProject. 
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
	self basicLoadProject: [ Rowan projectTools load loadProjectNamed: name ]
]

{ #category : 'rowan' }
RowanProjectService >> removeProjectNamed: projectName [
   "remove project"
 
    (Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
      ifNotNil: [ :project | Rowan image _removeLoadedProject: project ]

]

{ #category : 'rowan' }
RowanProjectService >> repositoryRoot [
	^ (RwProject newNamed: name) repositoryRoot
]

{ #category : 'rowan' }
RowanProjectService >> repositoryRootPath [

	^self repositoryRoot pathString
]

{ #category : 'rowan' }
RowanProjectService >> repositorySha [
	^ self rwProject repositoryCommitId
]

{ #category : 'rowan' }
RowanProjectService >> rowanBranch [
	
	name isNil ifTrue:[^String new].
	^ [  self rwProject currentBranchName ] on: Error do: [:ex | ^'ERROR getting repository branch' ]
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
RowanProjectService >> rowanProjectUrl [

	^self rwProject projectUrl
]

{ #category : 'rowan' }
RowanProjectService >> rowanSha [

	name isNil ifTrue:[^0].
	^self rwProject loadedCommitId
]

{ #category : 'rowan' }
RowanProjectService >> rowanSkew [

	^self sha ~= self repositorySha

]

{ #category : 'accessing' }
RowanProjectService >> rwProject [
	^ rwProject
		ifNil: [ 
			rwProject := RwProject newNamed: name. 
			projectOop := rwProject asOop.
			rwProject ]
]

{ #category : 'perform' }
RowanProjectService >> servicePerform: symbol withArguments: collection [
  self isUpdatingButFoundToBeDeleted
    ifTrue: [ ^ self handleDeletedService ].
  super servicePerform: symbol withArguments: collection.
]

{ #category : 'rowan' }
RowanProjectService >> setExistsOnDisk [
	"might be a better test than #repositorySha for
	determining if a project exists on disk." 

	existsOnDisk := self rwProject existsOnDisk.
]

{ #category : 'accessing' }
RowanProjectService >> sha [

	name isNil ifTrue:[^0].
	^self rwProject loadedCommitId
]

{ #category : 'accessing' }
RowanProjectService >> sha: anObject [

	"because skew is intimately associated with sha
	set it here" 
	sha := anObject.
	isSkew := self rowanSkew

]

{ #category : 'accessing' }
RowanProjectService >> specService [
	^specService
]

{ #category : 'accessing' }
RowanProjectService >> specService: object [
	specService := object
]

{ #category : 'rowan' }
RowanProjectService >> unload [

	| loadedProject |
	loadedProject := Rowan image loadedProjects select:[:proj | proj name = name].

]

{ #category : 'update' }
RowanProjectService >> update [
	super update.
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

{ #category : 'other' }
RowanProjectService >> updateIsDirty [

	isDirty := self rowanDirty
]

{ #category : 'testing' }
RowanProjectService >> wasDeleted [
  ^ self projectIsLoaded not
]

{ #category : 'client commands' }
RowanProjectService >> write [
	Rowan projectTools write writeProjectNamed: name

]
