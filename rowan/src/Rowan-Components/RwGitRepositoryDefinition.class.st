Class {
	#name : 'RwGitRepositoryDefinition',
	#superclass : 'RwDiskRepositoryDefinition',
	#category : 'Rowan-Components'
}

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId [
self error: 'use a git command to extract commitId'
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> commitId: aString [

	self shouldNotImplement: #commitId
]

{ #category : 'accessing' }
RwGitRepositoryDefinition >> version [

	^ self properties at: 'version' ifAbsent: [
"use `git describe --match v0.0.1` or something"
 self error: 'not yet implemented']
]
