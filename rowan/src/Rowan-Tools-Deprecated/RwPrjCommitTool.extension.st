Extension { #name : 'RwPrjCommitTool' }

{ #category : '*rowan-tools-deprecated' }
RwPrjCommitTool >> commitPackagesForSpecification: aRwSpecification message: messageString [

	"commit repository ... packages must be written out independently with write tool"

self deprecated: 'package commit not supported in v2'.
	self specification: aRwSpecification.
	specification canCommit
		ifFalse: [ 
			| msg |
			msg := 'repository for ' , specification specName printString
				,
					' does not support commit operations. Source written to repository and skipping commit'.
			self inform: msg.
			^ msg ].
	specification commitForTool: self message: messageString.
	^ specification updateLoadedCommitIdForTool: self
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCommitTool >> commitPackagesForSpecUrl: aSpecUrlString message: messageString [

	"commit repository ... packages must be written out independently with write tool"

self deprecated: 'package commit not supported in v2'.
	^ self
		commitPackagesForSpecification: (RwSpecification fromUrl: aSpecUrlString)
		message: messageString
]

{ #category : '*rowan-tools-deprecated' }
RwPrjCommitTool >> commitProjectDefinition: projectDefinition message: messageString [

	"commit repository ... packages must be written out independently with write tool"

self deprecated: 'Use RwProjectDefinition>>commit:'.
	projectDefinition canCommit
		ifFalse: [ 
			| msg |
			msg := 'repository for project ' , projectDefinition name printString
				, ' does not support commit operations.'.
			self inform: msg.
			^ msg ].
	self specification: projectDefinition specification.
	specification commitForTool: self message: messageString.
	^ specification updateLoadedCommitIdForTool: self
]
