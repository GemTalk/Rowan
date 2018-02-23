"
This class implements the smalltalk: scheme:

 smalltalk://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 smalltalk://git@git.gemtalksystems.com/btree340:dev/repository
 smalltalk://git@github.com/GsDevKit/GsDevKit:350/repository
 smalltalk://github.com/GsDevKit/GsDevKit:350/repository

The smalltalk: scheme is based on Thierry Goubier's gitfiletree url[1]:

  smalltalk:// <[user[:password]@]host[:port]> / user/ projectName [ : versionIdentifier ] [ / repositoryPath ]

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st
"
Class {
	#name : 'RwSmalltalkRepositoryUrl',
	#superclass : 'RwHierarchicalUrl',
	#instVars : [
		'project',
		'committish',
		'dir'
	],
	#category : 'Rowan-Url'
}

{ #category : 'constants' }
RwSmalltalkRepositoryUrl class >> schemeName [
  ^ 'smalltalk'
]

{ #category : 'accessing' }
RwSmalltalkRepositoryUrl >> committish [

   ^committish

]

{ #category : 'accessing' }
RwSmalltalkRepositoryUrl >> dir [

   ^dir

]

{ #category : 'accessing' }
RwSmalltalkRepositoryUrl >> dirPath [
  ^ self pathFor: dir
]

{ #category : 'parsing' }
RwSmalltalkRepositoryUrl >> privateParsePath: remainder relativeTo: basePath [
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
RwSmalltalkRepositoryUrl >> project [

   ^project

]

{ #category : 'accessing' }
RwSmalltalkRepositoryUrl >> projectPath [
  ^ self pathFor: project
]
