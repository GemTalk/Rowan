"
This class implements the github: scheme[1]:

  github://GsDevKit/GsDevKit:master/repository

This scheme is supported for backward compatibility. Moving forward the metacello: scheme should be used (see RwMetacelloRepositoryUrl).

[1] https://github.com/Metacello/metacello/blob/master/docs/MetacelloScriptingAPI.md#github
"
Class {
	#name : 'RwGithubUrl',
	#superclass : 'RwFileUrl',
	#instVars : [
		'project',
		'committish',
		'dir'
	],
	#category : 'Rowan-Url-Core'
}

{ #category : 'constants' }
RwGithubUrl class >> schemeName [
  ^ 'github'
]

{ #category : 'converting' }
RwGithubUrl >> asRwRepository [
  "return a platform appropriate repository for the repository identified in the receiver"

  ^ self halt: 'not yet implemented'
]

{ #category : 'accessing' }
RwGithubUrl >> dirPath [
  ^ self pathFor: dir
]

{ #category : 'accessing' }
RwGithubUrl >> isAbsolute [
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^false
]

{ #category : 'printing' }
RwGithubUrl >> printOn: aStream [
  aStream
    nextPutAll: self schemeName;
    nextPut: $:.
  aStream nextPutAll: '//'.
  aStream nextPutAll: self pathString
]

{ #category : 'private-initialization' }
RwGithubUrl >> privateInitializeFromText: aString [
  | parsedPath |
  super privateInitializeFromText: aString.
  parsedPath := {host} , path.
  (parsedPath
    detect: [ :e | e includes: $: ]
    ifNone: [ 
      project := parsedPath copyFrom: 1 to: parsedPath size - 1.
      dir := parsedPath copyFrom: 2 to: parsedPath size ])
    ifNotNil: [ :pivot | 
      project := parsedPath copyFrom: 1 to: (parsedPath indexOf: pivot) - 1.
      project addLast: (pivot copyUpTo: $:).
      committish := pivot copyFrom: (pivot indexOf: $:) + 1 to: pivot size.
      dir := parsedPath
        copyFrom: (parsedPath indexOf: pivot) + 1
        to: parsedPath size ].
  path := parsedPath.
  host := ''
]

{ #category : 'accessing' }
RwGithubUrl >> projectPath [
  ^ self pathFor: project
]
