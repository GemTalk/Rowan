"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

a URL specifying a mailing address; activating it triggers a mail-sender to start up, if one is present.
"
Class {
	#name : 'CypressMailtoUrl',
	#superclass : 'CypressGenericUrl',
	#category : 'Cypress-Network-Url'
}

{ #category : 'constants' }
CypressMailtoUrl class >> schemeName [

	^ 'mailto'

]
