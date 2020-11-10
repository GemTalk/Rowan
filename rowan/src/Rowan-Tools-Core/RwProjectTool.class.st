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
RwProjectTool class >> clone [

	^RwPrjCloneTool new
]

{ #category : 'commands' }
RwProjectTool class >> commit [

	^RwPrjCommitTool new
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
RwProjectTool class >> revert [

	^RwPrjRevertTool new
]

{ #category : 'commands' }
RwProjectTool class >> test [

	^RwPrjTestTool new
]

{ #category : 'commands' }
RwProjectTool class >> trace [

	^RwPrjTraceTool new
]

{ #category : 'smalltalk api' }
RwProjectTool >> repositoryUrl [
  ^ self specification repositoryUrl
]
