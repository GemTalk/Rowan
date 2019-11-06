Class {
	#name : 'RwDiskRepositoryDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'repositoryRoot',
		'projectUrl'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwDiskRepositoryDefinitionV2 class >> _instanceClass: defaultInstanceClass [
	"default class only needed when receiver is the abstract class"

	self == RwDiskRepositoryDefinitionV2
		ifTrue: [ ^ defaultInstanceClass ].
	^ self
]

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
		forSpecification: aRwSpecification 
		projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	| cls projectHomeFileReference |
	projectHomeFileReference := projectHomeFileReferenceOrString asFileReference.
	cls := (aRwSpecification useGit and: [ projectHomeFileReference fileSystem isDiskFileSystem ])
		ifTrue: [ RwGitRepositoryDefinition ]
		ifFalse: [  projectHomeFileReference fileSystem rowanRepositoryDefinitionClass ].
	^ (self _instanceClass: cls)
		newNamed: aRwSpecification specName 
			forSpecification: aRwSpecification 
			repositoryRoot: projectHomeFileReference / aRwSpecification specName
]

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot [

	^ self
		newNamed: repositoryName 
			repositoryRoot: repoRoot
			projectUrl: aRwSpecification projectUrl
			committish: aRwSpecification repoSpec committish 
			committishType: aRwSpecification repoSpec committishType
]

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName repositoryRoot: repoRoot [ 

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		yourself
]

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		yourself
]

{ #category : 'instance creation' }
RwDiskRepositoryDefinitionV2 class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString committish: committish committishType: committishType [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		committish: committish committishType: committishType;
		yourself
]

{ #category : 'private' }
RwDiskRepositoryDefinitionV2 >> _fileSystem [
	^ FileSystem disk
]

{ #category : 'testing' }
RwDiskRepositoryDefinitionV2 >> canCommit [

	^ false
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> clone [

	"attach to existing cloned disk structure or clone project from remote repository"

	"who wins? resolve or clone"

	self subclassResponsibility: #clone
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> commitLog: logLimit [

	^ ''
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> create [

	self error: 'not yet implemented'
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> doCommit: message [

	"noop by default"
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> name [

	^ self key
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> name: aString [ 

	properties at: 'name' put: aString
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> projectUrl [
	^ projectUrl ifNil: [ '' ]
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> projectUrl: anUrlString [
	projectUrl := anUrlString
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryCommitId [

	^ ''
]

{ #category : 'testing' }
RwDiskRepositoryDefinitionV2 >> repositoryExists [

	^ self repositoryRoot exists
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ repositoryRoot
]

{ #category : 'accessing' }
RwDiskRepositoryDefinitionV2 >> repositoryRoot: pathStringOrReference [
	self projectUrl: nil.
	self gitRoot: nil.
	repositoryRoot := pathStringOrReference asFileReference
]

{ #category : 'actions' }
RwDiskRepositoryDefinitionV2 >> resolve [

	"attach to existing repository structure or create"

	"who wins? resolve or clone"

	self subclassResponsibility: #resolve
]
