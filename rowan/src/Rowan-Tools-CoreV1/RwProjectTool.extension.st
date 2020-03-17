Extension { #name : 'RwProjectTool' }

{ #category : '*rowan-tools-corev1' }
RwProjectTool class >> browser [

	^ RwPrjBrowserTool new
]

{ #category : '*rowan-tools-corev1' }
RwProjectTool class >> create [
  ^ RwPrjCreateTool new
]

{ #category : '*rowan-tools-corev1' }
RwProjectTool class >> load [

	^RwPrjLoadTool new
]

{ #category : '*rowan-tools-corev1' }
RwProjectTool class >> read [
  ^ RwPrjReadTool new
]

{ #category : '*rowan-tools-corev1' }
RwProjectTool class >> reconcile [

	^ RwPrjReconcileTool new

]

{ #category : '*rowan-tools-corev1' }
RwProjectTool class >> write [

	^RwPrjWriteTool new
]