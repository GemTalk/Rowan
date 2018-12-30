Class {
	#name : 'RwAbstractRepositoryDefinition',
	#superclass : 'RwDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwAbstractRepositoryDefinition class >> _instanceClass: defaultInstanceClass [
	"default class only needed when receiver is the abstract class"

	self == RwAbstractRepositoryDefinition
		ifTrue: [ ^ defaultInstanceClass ].
	^ self
]

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newForSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [

	^ self 
		newNamed: aRwSpecification specName 
		forSpecification: aRwSpecification 
		projectHome: projectHomeFileReferenceOrString
]

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newNamed: repositoryName forSpecification: aRwSpecification projectHome: projectHomeFileReferenceOrString [


	| cls projectHomeFileReference |
	projectHomeFileReference := projectHomeFileReferenceOrString asFileReference.
	cls := self _instanceClass: projectHomeFileReference fileSystem rowanRepositoryDefinitionClass.
	^ cls
		newNamed: repositoryName 
		repositoryRoot: projectHomeFileReference / repositoryName
]

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot [ 

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		yourself
]

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString version: aSemanticVersionoOrString [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		version: aSemanticVersionoOrString;
		yourself
]

{ #category : 'instance creation' }
RwAbstractRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot version: aSemanticVersionoOrString [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		version: aSemanticVersionoOrString;
		yourself
]

{ #category : 'private' }
RwAbstractRepositoryDefinition >> _fileSystem [

	self subclassResponsibility: #_fileSystem
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> alias [

	^ self properties at: 'alias' ifAbsent: [ self name ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> alias: aString [

	self properties at: 'alias' put: aString
]

{ #category : 'actions' }
RwAbstractRepositoryDefinition >> attach [
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> commitId [

	^ self properties at: 'commitId' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> commitId: aString [

	^ self properties at: 'commitId' put: aString
]

{ #category : 'actions' }
RwAbstractRepositoryDefinition >> create [
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> key [

	^properties at: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> name [

	^ self key
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> name: aString [ 

	properties at: 'name' put: aString
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> projectUrl [

	^ self properties at: 'projectUrl' ifAbsent: [ '' ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> projectUrl: anUrlString [

	anUrlString ifNil: [ ^ self properties removeKey: 'projectUrl' ifAbsent: [ ] ].
	self properties at: 'projectUrl' put: anUrlString
]

{ #category : 'testing' }
RwAbstractRepositoryDefinition >> repositoryExists [

	^ self repositoryRoot exists
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> repositoryRoot [

	^ self properties at: 'repositoryRoot'
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> repositoryRoot: pathStringOrReference [

	self version: nil.
	self projectUrl: nil.
	self properties at: 'repositoryRoot' put: pathStringOrReference asFileReference
]

{ #category : 'actions' }
RwAbstractRepositoryDefinition >> resolve [
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> version [

	^ self properties at: 'version' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwAbstractRepositoryDefinition >> version: aSemanticVersionoOrString [

	aSemanticVersionoOrString ifNil: [ ^ self properties removeKey: 'version' ifAbsent: [ ] ].
	^ self properties at: 'version' put: aSemanticVersionoOrString asRwSemanticVersionNumber
]
