"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressLaxFileUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressLaxFileUrl class >> schemeName [

	^'cypresslax'

]

{ #category : 'accessing' }
CypressLaxFileUrl >> codeFormat [

	^'Cypress'

]

{ #category : 'testing' }
CypressLaxFileUrl >> isStrict [

	^false

]
