Extension { #name : 'RwPackageTool' }

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> commit [

	self deprecated: 'RwPackageTool class >> commit deprecated in Rowan 1.2.6+'.
  ^ RwPkgCommitTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> copy [
	self deprecated: 'RwPackageTool class >> copy deprecated in Rowan 1.2.6+'.
  ^ RwPkgCopyTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> edit [

	self deprecated: 'RwPackageTool class >> edit deprecated in Rowan 1.2.6+'.
	^ RwPkgEditTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> install [
	self deprecated: 'RwPackageTool class >> install deprecated in Rowan 1.2.6+'.
	^ RwPkgInstallTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> list [
	self deprecated: 'RwPackageTool class >> list deprecated in Rowan 1.2.6+'.
  ^ RwPkgListTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> move [
	self deprecated: 'RwPkgMoveTool class deprecated in Rowan 1.2.6+'.
  ^ RwPkgMoveTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> read [
	self deprecated: 'RwPkgReadTool class deprecated in Rowan 1.2.6+'.
	^ RwPkgReadTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> recompile [
	self deprecated: 'RwPkgRecompileTool class deprecated in Rowan 1.2.6+'.
  ^ RwPkgRecompileTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> remove [
	self deprecated: 'RwPkgRemoveTool class deprecated in Rowan 1.2.6+'.
  ^ RwPkgRemoveTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> rename [
	self deprecated: 'RwPkgRenameTool class deprecated in Rowan 1.2.6+'.
  ^ RwPkgRenameTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> revert [
	self deprecated: 'RwPkgRevertTool class deprecated in Rowan 1.2.6+'.
  ^ RwPkgRevertTool new
]

{ #category : '*rowan-tools-deprecated' }
RwPackageTool class >> write [
	self deprecated: 'RwPkgWriteTool class deprecated in Rowan 1.2.6+'.
  ^ RwPkgWriteTool new
]
