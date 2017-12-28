"
No class-specific documentation for CypPropertyModification, hierarchy is: 
Object
  CypDiff( changes)
    CypLeafModification( key before after)
      CypPropertyModification

"
Class {
	#name : 'RwPropertyModification',
	#superclass : 'Object',
	#instVars : [
		'key',
		'oldValue',
		'newValue'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwPropertyModification class >> key: key oldValue: oldValue newValue: newValue [

	^(self new)
		key: key;
		oldValue: oldValue;
		newValue: newValue;
		yourself
]

{ #category : 'testing' }
RwPropertyModification >> isEmpty [

	^oldValue = newValue
]

{ #category : 'Accessing' }
RwPropertyModification >> key [
	^key

]

{ #category : 'Updating' }
RwPropertyModification >> key: znewValue [
	key := znewValue

]

{ #category : 'Accessing' }
RwPropertyModification >> newValue [
	^newValue

]

{ #category : 'Updating' }
RwPropertyModification >> newValue: znewValue [
	newValue := znewValue

]

{ #category : 'Accessing' }
RwPropertyModification >> oldValue [
	^oldValue

]

{ #category : 'Updating' }
RwPropertyModification >> oldValue: znewValue [
	oldValue := znewValue

]
