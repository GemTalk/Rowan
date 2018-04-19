"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressCypressFileUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressCypressFileUrl class >> schemeName [

	^'cypress'

]

{ #category : 'accessing' }
CypressCypressFileUrl >> codeFormat [

	^'Cypress'

]

{ #category : 'testing' }
CypressCypressFileUrl >> isStrict [

	^true

]
