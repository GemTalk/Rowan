Class {
	#name : 'RwPrjCommitTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjCommitTool >> commitPackagesForSpecification: aRwSpecification message: messageString [

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
]

{ #category : 'smalltalk api' }
RwPrjCommitTool >> commitPackagesForSpecUrl: aSpecUrlString message: messageString [

	"commit repository ... packages must be written out independently with write tool"

	^ self
		commitPackagesForSpecification: (RwSpecification fromUrl: aSpecUrlString)
		message: messageString
]

{ #category : 'smalltalk api' }
RwPrjCommitTool >> commitProjectDefinition: projectDefinition message: messageString [

	"commit repository ... packages must be written out independently with write tool"

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

{ #category : 'smalltalk api' }
RwPrjCommitTool >> commitProjectNamed: projectName message: messageString [

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
]

{ #category : 'cypress api' }
RwPrjCommitTool >> commitSpecification: aRwSpecification message: messageString [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	"write packages and commit repository"

	self specification: aRwSpecification.
	self class write writeSpecification: specification.
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

{ #category : 'cypress api' }
RwPrjCommitTool >> commitSpecUrl: aSpecUrlString message: messageString [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	"write packages and commit repository"

	^ self
		commitSpecification: (RwSpecification fromUrl: aSpecUrlString)
		message: messageString
]
