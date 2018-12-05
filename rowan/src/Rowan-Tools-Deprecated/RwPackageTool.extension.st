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
