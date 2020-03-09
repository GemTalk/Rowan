Extension { #name : 'RwProjectTool' }

{ #category : '*rowan-tools-corev2' }
RwProjectTool class >> browserV2 [
	^ RwPrjBrowserToolV2 new
]

{ #category : '*rowan-tools-corev2' }
RwProjectTool class >> createV2 [
	^ RwPrjCreateToolV2 new
]

{ #category : '*rowan-tools-corev2' }
RwProjectTool class >> loadV2 [

	^RwPrjLoadToolV2 new
]

{ #category : '*rowan-tools-corev2' }
RwProjectTool class >> readV2 [
  ^ RwPrjReadToolV2 new
]

{ #category : '*rowan-tools-corev2' }
RwProjectTool class >> reconcileV2 [

	^ RwPrjReconcileToolV2 new
]

{ #category : '*rowan-tools-corev2' }
RwProjectTool class >> writeV2 [
	^ RwPrjWriteToolV2 new
]
