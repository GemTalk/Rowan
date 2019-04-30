"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

A URL that can be accessed via the Hypertext Transfer Protocol (HTTP), ie, a standard Web URL

realm = the name of the security realm that has been discovered for this URL.   Look it up in Passwords.

Passwords = a Dictionary of (realm -> encoded user&password)


TODO: use the username and password, if specified

"
Class {
	#name : 'CypressHttpUrl',
	#superclass : 'CypressHierarchicalUrl',
	#instVars : [
		'realm'
	],
	#classVars : [
		'Passwords'
	],
	#category : 'Cypress-Network-Url'
}

{ #category : 'constants' }
CypressHttpUrl class >> schemeName [

	^ 'http'.

]
