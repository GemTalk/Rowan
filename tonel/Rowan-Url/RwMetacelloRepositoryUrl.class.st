Class {
	#name : 'RwMetacelloRepositoryUrl',
	#superclass : 'RwHierarchicalUrl',
	#instVars : [
		'project',
		'committish',
		'dir'
	],
	#category : 'Rowan-Url'
}

{ #category : 'constants' }
RwMetacelloRepositoryUrl class >> schemeName [
  ^ 'metacello'
]

{ #category : 'accessing' }
RwMetacelloRepositoryUrl >> committish [

   ^committish

]

{ #category : 'accessing' }
RwMetacelloRepositoryUrl >> dir [

   ^dir

]

{ #category : 'accessing' }
RwMetacelloRepositoryUrl >> dirPath [
  ^ self pathFor: dir
]

{ #category : 'parsing' }
RwMetacelloRepositoryUrl >> privateParsePath: remainder relativeTo: basePath [
  | parsedPath |
  parsedPath := super privateParsePath: remainder relativeTo: basePath.
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
  ^ parsedPath
]

{ #category : 'accessing' }
RwMetacelloRepositoryUrl >> project [

   ^project

]

{ #category : 'accessing' }
RwMetacelloRepositoryUrl >> projectPath [
  ^ self pathFor: project
]
