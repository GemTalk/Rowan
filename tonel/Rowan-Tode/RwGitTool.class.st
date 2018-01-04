Class {
	#name : 'RwGitTool',
	#superclass : 'RwTool',
	#instVars : [
		'tdGitTool'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwGitTool >> gitrevparseShowTopLevelIn: aDirectory [
  ^ self tdGitTool gitrevparseShowTopLevelIn: aDirectory
]

{ #category : 'smalltalk api' }
RwGitTool >> performOnServer: command logging: logging [
  ^ self tdGitTool performOnServer: command logging: logging
]

{ #category : 'accessing' }
RwGitTool >> tdGitTool [
  ^ tdGitTool ifNil: [ tdGitTool := TDGitTool new ]
]
