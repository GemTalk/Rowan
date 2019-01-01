Class {
	#name : 'RwGitRepositoryDefinition',
	#superclass : 'RwDiskRepositoryDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinition class >> newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot [

	^ (super newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot)
		committish: aRwSpecification repoSpec committish;
		yourself
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId [
self error: 'use a git command to extract commitId'
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId: aString [

	self shouldNotImplement: #commitId
]

{ #category : 'actions' }
RwGitRepositoryDefinition >> resolve [

	"attach to existing git repository structure, clone, or create"

	Rowan projectTools clone
		cloneRepository: self
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> version [

	^ self properties at: 'version' ifAbsent: [
"use `git describe --match v0.0.1` or something"
 self error: 'not yet implemented']
]
