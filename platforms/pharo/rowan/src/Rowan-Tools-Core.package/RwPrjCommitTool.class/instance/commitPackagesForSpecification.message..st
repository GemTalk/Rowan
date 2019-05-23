smalltalk api
commitPackagesForSpecification: aRwSpecification message: messageString

	"commit repository ... packages must be written out independently with write tool"

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