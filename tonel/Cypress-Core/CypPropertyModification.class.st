"
No class-specific documentation for CypPropertyModification, hierarchy is: 
Object
  CypDiff( changes)
    CypLeafModification( key before after)
      CypPropertyModification

"
Class {
	#name : 'CypPropertyModification',
	#superclass : 'Object',
	#instVars : [
		'key',
		'oldValue',
		'newValue'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypPropertyModification class >> key: key oldValue: oldValue newValue: newValue [

	^(self new)
		key: key;
		oldValue: oldValue;
		newValue: newValue;
		yourself
]

{ #category : 'testing' }
CypPropertyModification >> isEmpty [

	^oldValue = newValue
]

{ #category : 'Accessing' }
CypPropertyModification >> key [
	^key

]

{ #category : 'Updating' }
CypPropertyModification >> key: znewValue [
	key := znewValue

]

{ #category : 'Accessing' }
CypPropertyModification >> newValue [
	^newValue

]

{ #category : 'Updating' }
CypPropertyModification >> newValue: znewValue [
	newValue := znewValue

]

{ #category : 'Accessing' }
CypPropertyModification >> oldValue [
	^oldValue

]

{ #category : 'Updating' }
CypPropertyModification >> oldValue: znewValue [
	oldValue := znewValue

]
