"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressTopazUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressTopazUrl class >> schemeName [

	^'topaz'

]

{ #category : 'accessing' }
CypressTopazUrl >> codeFormat [

	^'Topaz'

]

{ #category : 'testing' }
CypressTopazUrl >> isStrict [

	^true

]

{ #category : 'accessing' }
CypressTopazUrl >> repositoryClass [

	^CypressTopazRepository

]
