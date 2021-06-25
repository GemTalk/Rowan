"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressGitFileUrl',
	#superclass : 'CypressAbstractFileUrl',
	#instVars : [
		'projectPath',
		'projectBranchOrTag',
		'repositoryPath'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressGitFileUrl class >> schemeName [
  "A gitcypress url with a host is the target for a remote. All other parameters are optional.
	Parameters are:
		dir : the directory inside the repository where the target MC packages are.
		branch : the git branch to fetch.
		protocol: the user name part to add to the ssh Url, default to git, but can also be https (which implies read only access).
		readOnly : is the repository read only? If present, reduce the history to a minimum (and change the GUI).
	Alternative url syntax:
		gitcypress://github.com/dalehenrich/filetree:pharo5.0_dev/repository
	with:
		host : github.com
		project : dalehenrich/filetree
		branch : pharo5.0_dev
		dir : repository
"

  ^ 'gitcypress'

]

{ #category : 'accessing' }
CypressGitFileUrl >> codeFormat [

	^'Cypress'

]

{ #category : 'printing' }
CypressGitFileUrl >> httpsAccessString [
  ^ 'https://' , self host , self projectPath , '.git'

]

{ #category : 'private-initialization' }
CypressGitFileUrl >> initializeFromPathString: aPathString [
  | projectDelim repoDelimIndex branchOrTagDelimIndex |
  projectBranchOrTag := repositoryPath := nil.
  projectDelim := aPathString indexOf: $/ startingAt: 2.
  repoDelimIndex := aPathString indexOf: $/ startingAt: projectDelim + 1.
  (branchOrTagDelimIndex := aPathString indexOf: $:) == 0
    ifTrue: [ repoDelimIndex == 0
        ifTrue: [ self projectPath: aPathString ]
        ifFalse: [ self projectPath: (aPathString copyFrom: 1 to: repoDelimIndex - 1).
          self
            repositoryPath:
              (aPathString copyFrom: repoDelimIndex + 1 to: aPathString size) ] ]
    ifFalse: [ self projectPath: (aPathString copyFrom: 1 to: branchOrTagDelimIndex - 1).
      repoDelimIndex == 0
        ifTrue: [ projectBranchOrTag := aPathString
            copyFrom: branchOrTagDelimIndex + 1
            to: aPathString size ]
        ifFalse: [ self projectPath: (aPathString copyFrom: 1 to: branchOrTagDelimIndex - 1).
          self parseBranchOrTagField: [ :pv :rp | projectBranchOrTag := pv.
              self repositoryPath: rp ] pathString: aPathString
          branchOrTagDelimIndex: branchOrTagDelimIndex ] ]

]

{ #category : 'testing' }
CypressGitFileUrl >> isStrict [

	^true

]

{ #category : 'private-initialization' }
CypressGitFileUrl >> parseBranchOrTagField: parseBlock pathString: aPathString branchOrTagDelimIndex: branchOrTagDelimIndex [
  | strm done escaped repoDelimIndex |
  strm := WriteStream on: String new.
  repoDelimIndex := branchOrTagDelimIndex + 1.
  escaped := done := false.
  [ done ] whileFalse: [ | char |
      repoDelimIndex > aPathString size
        ifTrue: [ done := true ]
        ifFalse: [ char := aPathString at: repoDelimIndex.
          char == $\
            ifTrue: [ escaped
                ifTrue: [ "$\ not legal in branch name ... literally ignored"
                  escaped := false ]
                ifFalse: [ escaped := true ] ]
            ifFalse: [ char == $/
                ifTrue: [ escaped
                    ifFalse: [ done := true ] ].
              done
                ifFalse: [ strm nextPut: char ].
              escaped := false ].
          repoDelimIndex := repoDelimIndex + 1 ] ].
  repoDelimIndex := repoDelimIndex - 1.
  parseBlock
    value: strm contents
    value: (aPathString copyFrom: repoDelimIndex + 1 to: aPathString size)

]

{ #category : 'printing' }
CypressGitFileUrl >> printOn: aStream [

	aStream nextPutAll: self schemeName , '://' , self host.
	aStream
		nextPutAll: self projectPath;
		nextPut: $:.
	self projectBranchOrTag do: 
			[:char |
			char = $/ ifTrue: [aStream nextPut: $\].
			aStream nextPut: char].
	aStream
		nextPut: $/;
		nextPutAll: self repositoryPath

]

{ #category : 'accessing' }
CypressGitFileUrl >> projectBranchOrTag [
  ^ projectBranchOrTag ifNil: [ 'master' ]

]

{ #category : 'accessing' }
CypressGitFileUrl >> projectBranchOrTag: anObject [

   projectBranchOrTag := anObject

]

{ #category : 'accessing' }
CypressGitFileUrl >> projectPath [

   ^projectPath

]

{ #category : 'accessing' }
CypressGitFileUrl >> projectPath: aString [
  aString last = self fileUtils pathNameDelimiter last
    ifTrue: [ projectPath := aString copyFrom: 1 to: aString size - 1 ]
    ifFalse: [ projectPath := aString ]

]

{ #category : 'accessing' }
CypressGitFileUrl >> repositoryClass [
  ^ CypressFileSystemGitRepository

]

{ #category : 'accessing' }
CypressGitFileUrl >> repositoryPath [
  ^ repositoryPath ifNil: [ '' ]

]

{ #category : 'accessing' }
CypressGitFileUrl >> repositoryPath: aString [
  (aString size > 0
    and: [ aString last = self fileUtils pathNameDelimiter last ])
    ifTrue: [ repositoryPath := aString copyFrom: 1 to: aString size - 1 ]
    ifFalse: [ repositoryPath := aString ]

]
