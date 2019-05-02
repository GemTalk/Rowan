Class {
	#name : 'RwProjectReferenceDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newForSpecification: aRwSpecification [

	^ self
		newNamed: aRwSpecification projectName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
			forSpecification: aRwSpecification 
			projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification [

	^ self
		newNamed: projectName 
			forSpecification: aRwSpecification 
			projectHome: nil
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> newNamed: projectName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self
		projectName: projectName
			configurationNames: aRwSpecification defaultConfigurationNames
			groupNames: aRwSpecification defaultGroupNames
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
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl comment: comment [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		comment: comment;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames  useGit: useGit projectUrl: projectUrl comment: comment projectHome: projectHomeFileReferenceOrString committish: committish committishType: committishType configsPath: configsPath packagesPath: packagesPath projectsPath: projectsPath specsPath: specsPath [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		projectUrl: projectUrl;
		useGit: useGit;
		comment: comment;
		projectHome: projectHomeFileReferenceOrString;
		committish: committish committishType: committishType;
		configsPath: configsPath;
		packagesPath: packagesPath;
		projectsPath: projectsPath;
		specsPath: specsPath;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName configurationNames: configurationNames groupNames: groupNames useGit: useGit projectUrl: projectUrl committish: committish committishType: committishType comment: comment [

	^ self new
		projectName: projectName;
		configurationNames: configurationNames;
		groupNames: groupNames;
		useGit: useGit;
		projectUrl: projectUrl;
		comment: comment;
		committish: committish committishType: committishType;
		yourself
]

{ #category : 'instance creation' }
RwProjectReferenceDefinition class >> projectName: projectName useGit: useGit [

	^ self new
		projectName: projectName;
		useGit: useGit;
		yourself
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _createRepositoryFrom: repoDef [ 

	self error: 'not yet implemented'
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _repositoryDefinitionClass [

	self useGit ifTrue: [ ^ RwGitRepositoryDefinition ].
	^ RwAbstractRepositoryDefinition
]

{ #category : 'private' }
RwProjectReferenceDefinition >> _repositoryDefinitionForProjectHome: projectHomeFileReferenceOrString [


	| existingProjectRepository |
	existingProjectRepository := 
		Rowan image 
			projectRepositoryNamed: self projectAlias
				ifAbsent: [
					| newProjectRepository |
					newProjectRepository := self _repositoryDefinitionClass
						newNamed: self projectAlias
							repositoryRoot: projectHomeFileReferenceOrString asFileReference / self projectAlias
							projectUrl: self projectUrl
							committish: self committish 
							committishType: self committishType.
					Rowan image addProjectRepository: newProjectRepository.
					^ newProjectRepository ].
	"confirm that the existing project repository is compatible with the receiver"
	existingProjectRepository projectUrl = self projectUrl
		ifFalse: [ self error: 'incompatible project repository found in registry' ].
	^ existingProjectRepository
]

{ #category : 'visiting' }
RwProjectReferenceDefinition >> acceptVisitor: aVisitor [

	^ aVisitor visitProjectReferenceDefinition: self
]

{ #category : 'conversion' }
RwProjectReferenceDefinition >> asSpecification [

	| repoSpec platformSpec platformDict |
	self useGit
		ifTrue: [
			repoSpec := RwGitRepositorySpecification new
				committish: self committish;
				committishType: self committishType;
				remoteUrl: self remoteUrl;
				yourself ]
		ifFalse: [  repoSpec := RwDiskRepositorySpecification new ] .
	platformSpec := RwGemStoneSpecification new.
	platformSpec packageNameToPlatformPropertiesMap: self packageNameToPlatformPropertiesMap.
	platformDict := (Dictionary new) at: 'gemstone' put: platformSpec; yourself.
	^ RwComponentSpecification new
		specName: self projectName;
		projectUrl: self projectUrl;
		repoPath: self packagesPath;
		configsPath: self configsPath;
		specsPath: self specsPath;
		projectsPath: self projectsPath;
		defaultConfigurationNames: self configurationNames;
		defaultGroupNames: self groupNames;
		comment: self comment;
		repoSpec: repoSpec;
		platformSpec: platformDict;
		yourself.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> branch: aString [

	self committish: aString committishType: 'branch'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> comment [

	^ self properties at: 'comment' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> comment: aString [

	^ self properties at: 'comment' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish [

	^ self properties at: 'committish' ifAbsent: [ 'master' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committish: aString committishType: committishTypeString [

	self properties at: 'committish' put: aString.
	self properties at: 'committishType' put: committishTypeString.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> committishType [

	^ self properties at: 'committishType'  ifAbsent: [ 'branch' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsPath [

	^ self properties at: 'configsPath' ifAbsent: [ 'rowan/configs' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsPath: aString [

	^ self properties at: 'configsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configsRoot [

	^self repositoryRoot / self configsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configurationNames [

	^ self properties at: 'configurationNames' ifAbsent: [ { self defaultComponentName } ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> configurationNames: anArray [

	^ self properties at: 'configurationNames' put: anArray
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> create [
	"write the project structure to disk, if it doesn't already exist"

	Rowan projectTools create
		createProjectRepository: self.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> defaultComponentName [

	^ 'Core'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> defaultGroupName [

	^ 'core'
]

{ #category : 'exporting' }
RwProjectReferenceDefinition >> exportSpecification [

	| spec |
	spec := self asSpecification.
	spec exportToUrl: 'file:',  self specsRoot pathString, '/'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> groupNames [

	^ self properties at: 'groupNames' ifAbsent: [{ self defaultGroupName } ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> groupNames: anArray [

	^ self properties at: 'groupNames' put: anArray
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> imageSpec [
self deprecated: 'temporary method ... simulating RwSpecification api'.
	^self
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> key [

	^ self projectName
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> loadedCommitId [

self deprecated: 'temporary method ... simulating RwSpecification api'.
	^''
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> loadedCommitId:  ignored [
self deprecated: 'temporary method ... simulating RwSpecification api'.
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedConfigurationNames [

	^ self configurationNames
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedConfigurationNames: anArray [

	^ self configurationNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedGroupNames [

	^ self groupNames
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> loadedGroupNames: anArray [

	^ self groupNames: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageConvention [

	^ self properties at: 'packageConvention' ifAbsent: [ 'RowanHybrid' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageConvention: aString [

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	(#( 'RowanHybrid' 'Monticello' 'Rowan' ) includes: aString)
		ifFalse: [ self error: 'Unknown package convention ', aString printString, '. Should be one of: RowanHybrid, Monticello, or Rowan' ].
	^ self properties at: 'packageConvention' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageFormat [

	^ self packageFormatIfAbsent: [ 'tonel' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageFormat: aString [

	(#( 'tonel' 'filetree' ) includes: aString)
		ifFalse: [ self error: 'Unknown package format ', aString printString, '. Should be one of: tonel, or filetree' ].
	^ self properties at: 'packageFormat' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packageFormatIfAbsent: absentBlock [

	^ self properties at: 'packageFormat' ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesPath [

	^ self properties at: 'packagesPath' ifAbsent: [ 'rowan/src' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesPath: aString [

	^ self properties at: 'packagesPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> packagesRoot [

	^self repositoryRoot / self packagesPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectAlias [

	^ self properties at: 'projectAlias' ifAbsent: [ self projectName ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectAlias: aStringOrNil [

	aStringOrNil ifNil: [ ^ self properties removeKey: 'projectAlias' ifAbsent: [] ].
	^ self properties at: 'projectAlias' put: aStringOrNil
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectHome [

	^ self properties at: 'projectHome' ifAbsent: [ FileLocator rowanProjectsHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectHome: projectHomeFileReferenceOrString [

	self repositoryDefinition: nil. "changing project home invalidates the current repository definition"
	projectHomeFileReferenceOrString ifNil: [ ^ self properties removeKey: 'projectHome' ifAbsent: [] ].
	^ self properties at: 'projectHome' put: projectHomeFileReferenceOrString asFileReference
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectName [

	^ self properties at: 'projectName' ifAbsent: []
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectName: aString [

	^ self properties at: 'projectName' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectNames [

	^ self properties at: 'projectNames' ifAbsent: [ #() ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectNames: anArray [

	^ self properties at: 'projectNames' put: anArray
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsPath [

	^ self properties at: 'projectsPath' ifAbsent: [ 'rowan/projects' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsPath: aString [

	^ self properties at: 'projectsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectsRoot [

	^self repositoryRoot / self projectsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl [

	^ self properties at: 'projectUrl' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> projectUrl: aString [

	^ self properties at: 'projectUrl' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> remoteUrl [

	^ self repositoryDefinition remoteUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> remoteUrl: aRemoteUrl [

	self repositoryDefinition remoteUrl: aRemoteUrl
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryDefinition [

	^ self properties at: 'repositoryDefinition' ifAbsentPut: [ self _repositoryDefinitionForProjectHome: self projectHome ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryDefinition: aRepositoryDefinition [

	aRepositoryDefinition ifNil: [ ^ self properties removeKey:  'repositoryDefinition' ifAbsent: [] ].
	^ self properties at: 'repositoryDefinition' put: aRepositoryDefinition
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> repositoryRoot [

	^self projectHome / self projectAlias
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> repositoryRootPath [

self deprecated: 'temporary method ... simulating RwSpecification api'.
	^ self repositoryRoot pathString
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> repoSpec [

self deprecated: 'temporary method ... simulating RwSpecification api'.
	^self
]

{ #category : 'actions' }
RwProjectReferenceDefinition >> resolve [

	^ self repositoryDefinition resolve
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> SHA: aString [

	self committish: aString committishType: 'SHA'
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsPath [

	^ self properties at: 'specsPath' ifAbsent: [ 'rowan/specs' ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsPath: aString [

	^ self properties at: 'specsPath' put: aString
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> specsRoot [

	^self repositoryRoot / self specsPath
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> tag: aString [

	self committish: aString committishType: 'tag'
]

{ #category : 'temporary compat' }
RwProjectReferenceDefinition >> updateLoadedCommitIdForTool: aRwTool [
self deprecated: 'temporary method ... simulating RwSpecification api'.
  ^ aRwTool doGitUpdateLoadedCommitId
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> useGit [

	^ self properties at: 'useGit' ifAbsent: [ false ]
]

{ #category : 'accessing' }
RwProjectReferenceDefinition >> useGit: aBool [

	^ self properties at: 'useGit' put: aBool
]
