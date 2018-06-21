Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#instVars : [
		'definedGroupNames'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> addDefinedGroupName: groupName [ 

	"add a group that does not include any other groups"

	self addDefinedGroupName: groupName includes: #()

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> addDefinedGroupName: groupName includes: includedGroups [

	"add a group that does not include any other groups"

	self definedGroupNames at: groupName put: includedGroups

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames [

	^ definedGroupNames ifNil: [ definedGroupNames :=  Dictionary new ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames: aColl [

	definedGroupNames := aColl

]

{ #category : 'initialization' }
RwProjectLoadConfiguration >> initialize [

	super initialize.
	definedGroupNames := Dictionary new

]
