Class {
	#name : 'RwProjectTool',
	#superclass : 'RwAbstractTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'commands' }
RwProjectTool class >> adopt [

	^ RwPrjAdoptTool new
]

{ #category : 'commands' }
RwProjectTool class >> audit [

	^RwPrjAuditTool new

]

{ #category : 'commands' }
RwProjectTool class >> browser [

	^ RwPrjBrowserTool new
]

{ #category : 'commands' }
RwProjectTool class >> checkout [

	^RwPrjCheckoutTool new
]

{ #category : 'commands' }
RwProjectTool class >> clone [

	^RwPrjCloneTool new
]

{ #category : 'commands' }
RwProjectTool class >> commit [

	^RwPrjCommitTool new
]

{ #category : 'commands' }
RwProjectTool class >> create [
  ^ RwPrjCreateTool new
]

{ #category : 'commands' }
RwProjectTool class >> delete [

	^ RwPrjDeleteTool new

]

{ #category : 'commands' }
RwProjectTool class >> diff [

	^ RwPrjDiffTool new
]

{ #category : 'commands' }
RwProjectTool class >> disown [

	^ RwPrjDisownTool new

]

{ #category : 'commands' }
RwProjectTool class >> edit [

	^ RwPrjEditTool new
]

{ #category : 'commands' }
RwProjectTool class >> load [

	^RwPrjLoadTool new
]

{ #category : 'commands' }
RwProjectTool class >> loadV2 [

	^RwPrjLoadToolV2 new
]

{ #category : 'commands' }
RwProjectTool class >> log [

	^RwPrjLogTool new
]

{ #category : 'commands' }
RwProjectTool class >> pull [

	^RwPrjPullTool new
]

{ #category : 'commands' }
RwProjectTool class >> push [

	^RwPrjPushTool new
]

{ #category : 'commands' }
RwProjectTool class >> query [

	^ RwPrjQueryTool new
]

{ #category : 'commands' }
RwProjectTool class >> read [
  ^ RwPrjReadTool new
]

{ #category : 'commands' }
RwProjectTool class >> reconcile [

	^ RwPrjReconcileTool new

]

{ #category : 'commands' }
RwProjectTool class >> revert [

	^RwPrjRevertTool new
]

{ #category : 'commands' }
RwProjectTool class >> test [

	^RwPrjTestTool new
]

{ #category : 'commands' }
RwProjectTool class >> write [

	^RwPrjWriteTool new
]

{ #category : 'smalltalk api' }
RwProjectTool >> repositoryUrl [
  ^ self specification repositoryUrl
]
