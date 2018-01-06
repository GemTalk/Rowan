Class {
	#name : 'RwGithubUrl',
	#superclass : 'RwFileUrl',
	#instVars : [
		'project',
		'committish',
		'dir'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'constants' }
RwGithubUrl class >> schemeName [
  ^ 'github'
]

{ #category : 'accessing' }
RwGithubUrl >> dirPath [
  ^ self pathFor: dir
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
