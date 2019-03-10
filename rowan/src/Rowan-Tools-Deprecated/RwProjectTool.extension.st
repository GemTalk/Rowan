Extension { #name : 'RwProjectTool' }

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> diff [

	self deprecated: 'RwPrjDiffTool class deprecated in Rowan 1.2.6+'.
	^RwPrjDiffTool new
]

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> install [
	self deprecated: 'RwPrjInstallTool class deprecated in Rowan 1.2.6+'.
	^ RwPrjInstallTool new
]

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> list [

	self deprecated: 'RwPrjListTool class deprecated in Rowan 1.2.6+'.
	^RwPrjListTool new
]

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> register [
	self deprecated: 'RwPrjRegisterTool class deprecated in Rowan 1.2.6+'.
  ^ RwPrjRegisterTool new
]

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> registry [

	self deprecated: 'RwPrjRegistryTool class deprecated in Rowan 1.2.6+'.
	^RwPrjRegistryTool new
]

{ #category : '*rowan-tools-deprecated' }
RwProjectTool class >> unregister [

	self deprecated: 'RwPrjUnregisterTool class deprecated in Rowan 1.2.6+'.
	^RwPrjUnregisterTool new
]
