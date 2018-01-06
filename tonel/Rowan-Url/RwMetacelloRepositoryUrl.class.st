"
This class implements the metacello: scheme:

 metacello://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 metacello://git@git.gemtalksystems.com/btree340:dev/repository
 metacello://git@github.com/GsDevKit/GsDevKit:350/repository
 metacello://github.com/GsDevKit/GsDevKit:350/repository

The metacello: scheme is based on Thierry Goubier's gitfiletree url[1]:

  metacello:// <[user[:password]@]host[:port]> / user/ projectName [ : versionIdentifier ] [ / repositoryPath ]

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st
"
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
