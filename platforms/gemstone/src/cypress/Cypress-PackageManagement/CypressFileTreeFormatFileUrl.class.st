"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileTreeFormatFileUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressFileTreeFormatFileUrl class >> schemeName [

	^'cypressft'

]

{ #category : 'accessing' }
CypressFileTreeFormatFileUrl >> codeFormat [

	^'FileTree'

]

{ #category : 'testing' }
CypressFileTreeFormatFileUrl >> isStrict [

	^false

]
