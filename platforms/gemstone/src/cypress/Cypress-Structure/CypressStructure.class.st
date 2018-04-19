"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressStructure',
	#superclass : 'CypressObject',
	#instVars : [
		'name',
		'properties',
		'packageStructure'
	],
	#category : 'Cypress-Structure'
}

{ #category : 'instance creation' }
CypressStructure class >> named: aString [

	^(self new)
		name: aString;
		yourself

]

{ #category : 'accessing' }
CypressStructure >> name [

	^name

]

{ #category : 'accessing' }
CypressStructure >> name: aString [ 

	name := aString

]

{ #category : 'accessing' }
CypressStructure >> packageStructure [
	^packageStructure

]

{ #category : 'accessing' }
CypressStructure >> packageStructure: aCypressPackageStructure [
	packageStructure := aCypressPackageStructure

]

{ #category : 'printing' }
CypressStructure >> printDetailsOn: aStream [

	aStream nextPutAll: self name.

]

{ #category : 'accessing' }
CypressStructure >> properties [

	properties ifNil: [ properties := Dictionary new ].
	^properties

]

{ #category : 'accessing' }
CypressStructure >> properties: aDictionary [

	properties := aDictionary

]
