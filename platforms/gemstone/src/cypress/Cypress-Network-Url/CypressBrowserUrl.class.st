"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

URLs that instruct a browser to do something.
"
Class {
	#name : 'CypressBrowserUrl',
	#superclass : 'CypressGenericUrl',
	#category : 'Cypress-Network-Url'
}

{ #category : 'constants' }
CypressBrowserUrl class >> schemeName [

	^'browser'

]

{ #category : 'downloading' }
CypressBrowserUrl >> hasContents [

	^true

]
