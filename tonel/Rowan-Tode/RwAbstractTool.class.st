"
Fundamental concepts:

	read			(disk -> def)
	write			(def -> disk)

	install			(def -> image)
	derive			(image -> def)

	load			(disk ->  image)
	commit			(image -> disk)

	project			manages collections of packages
	package			manages collections of definitions
	definitions	manages properties of classes and methods
	repository		manages disk representation of defintions
"
Class {
	#name : 'RwAbstractTool',
	#superclass : 'Object',
	#instVars : [
		'specification'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'commands' }
RwAbstractTool class >> git [
  ^ RwPrjGitTool new
]

{ #category : 'git' }
RwAbstractTool >> doGitCommit: messageString [
  | gitTool gitRootDir commitMessageFileName status loadedCommitId |
  gitTool := self class git.
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  commitMessageFileName := gitTool createTmpFileWith: messageString.
  gitTool gitaddIn: gitRootDir with: '.'.
  gitTool gitcommitIn: gitRootDir with: '-a --file=' , commitMessageFileName.
  status := gitTool gitlogIn: gitRootDir with: '-1'.
  Transcript
    cr;
    show: '==============';
    cr;
    show: status.
  ^ status
]

{ #category : 'git' }
RwAbstractTool >> doGitUpdateLoadedCommitId [
  | gitRootDir |
  gitRootDir := ServerFileDirectory
    on: specification repoSpec repositoryRootPath.
  ^ self doGitUpdateLoadedCommitId: gitRootDir
]

{ #category : 'git' }
RwAbstractTool >> doGitUpdateLoadedCommitId: gitRootDir [
  | gitTool loadedCommitId |
  gitTool := self class git.
  loadedCommitId := gitTool gitcommitShaIn: gitRootDir.
  ^ specification imageSpec loadedCommitId: loadedCommitId
]

{ #category : 'private' }
RwAbstractTool >> fileUtilities [
  ^ Rowan fileUtilities
]

{ #category : 'smalltalk api' }
RwAbstractTool >> help [

	^self manPage asText
]

{ #category : 'man page' }
RwAbstractTool >> manPage [
  self subclassResponsibility: #'manPage'
]

{ #category : 'smalltalk api' }
RwAbstractTool >> specification: aRwSpecification [
  self validate: aRwSpecification.
  ^ specification := aRwSpecification
]

{ #category : 'smalltalk api' }
RwAbstractTool >> specUrl: aString [
  ^ self specification: (RwSpecification fromUrl: aString)
]

{ #category : 'validation' }
RwAbstractTool >> validate: aRwSpecification [
  aRwSpecification specName
    ifNil: [ self error: 'The specification must have a specName specified.' ]
]

{ #category : 'validation' }
RwAbstractTool >> validateForRead: aRwSpecification [
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]

{ #category : 'validation' }
RwAbstractTool >> validateForWrite: aRwSpecification [
  aRwSpecification repositoryRootPath
    ifNil: [ 
      self
        error:
          'The repositoryRootPath must be defined in the specification: '
            , aRwSpecification specName printString ]
]
