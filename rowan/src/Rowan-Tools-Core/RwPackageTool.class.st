Class {
	#name : 'RwPackageTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'commands' }
RwPackageTool class >> audit [
  ^ RwPkgAuditTool new

]

{ #category : 'commands' }
RwPackageTool class >> commit [
  ^ RwPkgCommitTool new
]

{ #category : 'commands' }
RwPackageTool class >> copy [
  ^ RwPkgCopyTool new
]

{ #category : 'commands' }
RwPackageTool class >> create [
  ^ RwPkgCreateTool new
]

{ #category : 'commands' }
RwPackageTool class >> diff [
  ^ RwPkgDiffTool new
]

{ #category : 'commands' }
RwPackageTool class >> edit [

	^ RwPkgEditTool new
]

{ #category : 'commands' }
RwPackageTool class >> install [
	^ RwPkgInstallTool new
]

{ #category : 'commands' }
RwPackageTool class >> list [
  ^ RwPkgListTool new
]

{ #category : 'commands' }
RwPackageTool class >> load [
  ^ RwPkgLoadTool new
]

{ #category : 'commands' }
RwPackageTool class >> move [
  ^ RwPkgMoveTool new
]

{ #category : 'commands' }
RwPackageTool class >> read [
	^ RwPkgReadTool new
]

{ #category : 'commands' }
RwPackageTool class >> recompile [
  ^ RwPkgRecompileTool new
]

{ #category : 'commands' }
RwPackageTool class >> remove [
  ^ RwPkgRemoveTool new
]

{ #category : 'commands' }
RwPackageTool class >> rename [
  ^ RwPkgRenameTool new
]

{ #category : 'commands' }
RwPackageTool class >> revert [
  ^ RwPkgRevertTool new
]

{ #category : 'commands' }
RwPackageTool class >> write [
  ^ RwPkgWriteTool new
]
