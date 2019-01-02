Class {
	#name : 'RwGitRepositoryDefinition',
	#superclass : 'RwDiskRepositoryDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwGitRepositoryDefinition class >> newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot [

	^ (super newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot)
		committish: aRwSpecification repoSpec committish;
		remoteUrl: aRwSpecification remoteUrl;
		yourself
]

{ #category : 'instance creation' }
RwGitRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString remoteUrl: aRemoreUrlStrng committish: committish [

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		remoteUrl: aRemoreUrlStrng;
		committish: committish;
		yourself
]

{ #category : 'instance creation' }
RwGitRepositoryDefinition class >> newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString version: ignored [

	"version should be picked up from the git repository itself"

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
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

{ #category : 'accessing' }
RwGitRepositoryDefinition >> remoteUrl [

	^ self properties at: 'remoteUrl' ifAbsent: [ ]
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> remoteUrl: aRemoteUrlString [

	^ self properties at: 'remoteUrl' put: aRemoteUrlString
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
