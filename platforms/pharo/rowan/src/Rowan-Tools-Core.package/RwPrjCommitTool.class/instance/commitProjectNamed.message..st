smalltalk api
commitProjectNamed: projectName message: messageString

	"commit repository ... packages must be written out independently with write tool"

	| loadedProject projectDefinition |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
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