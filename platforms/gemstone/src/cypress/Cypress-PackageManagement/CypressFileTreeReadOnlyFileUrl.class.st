"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileTreeReadOnlyFileUrl',
	#superclass : 'CypressAbstractFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressFileTreeReadOnlyFileUrl class >> schemeName [

	^'cypressfiletree'

]

{ #category : 'accessing' }
CypressFileTreeReadOnlyFileUrl >> codeFormat [

	^'FileTree'

]

{ #category : 'testing' }
CypressFileTreeReadOnlyFileUrl >> isStrict [

	^true

]
