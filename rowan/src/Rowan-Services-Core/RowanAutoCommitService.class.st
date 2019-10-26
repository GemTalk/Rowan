"
Simple service that provides updates to the client related to auto commit.
"
Class {
	#name : 'RowanAutoCommitService',
	#superclass : 'RowanService',
	#instVars : [
		'autoCommit'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'client commands' }
RowanAutoCommitService >> autoCommit: boolean [

	self class setAutoCommit: boolean.
	autoCommit := self class autoCommit. 
	updateType := #autoCommitUpdate:.
	RowanCommandResult addResult: self.
]

{ #category : 'client commands' }
RowanAutoCommitService >> flipAutoCommit [

	autoCommit := self class flipAutoCommit. 
	updateType := #autoCommitUpdate:.
	RowanCommandResult addResult: self.
]
