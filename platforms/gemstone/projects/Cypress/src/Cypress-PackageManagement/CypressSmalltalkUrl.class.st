"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressSmalltalkUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressSmalltalkUrl class >> schemeName [

	^'chunk'

]

{ #category : 'accessing' }
CypressSmalltalkUrl >> codeFormat [

	^'Chunk'

]

{ #category : 'testing' }
CypressSmalltalkUrl >> isStrict [

	^true

]

{ #category : 'accessing' }
CypressSmalltalkUrl >> repositoryClass [

	^CypressSmalltalkRepository

]
