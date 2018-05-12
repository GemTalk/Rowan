Class {
	#name : 'RwRepositoryTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'commands' }
RwRepositoryTool class >> add [
  ^ RwRepAddTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> commit [
  ^ RwRepCommitTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> list [
  ^ RwRepListTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> packages [
  ^ RwRepPackagesTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> pull [
  ^ RwRepPullTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> push [
  ^ RwRepPushTool new
]

{ #category : 'commands' }
RwRepositoryTool class >> remove [
  ^ RwRepRemoveTool new
]
