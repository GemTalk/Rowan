Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#instVars : [
		'definedGroupNames'
	],
	#category : 'RowanV12Metadata-Core'
}

{ #category : 'visiting' }
RwProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> addDefinedGroupName: groupName [ 

	"add a group that does not include any other groups"

	self addDefinedGroupName: groupName includeGroups: #()

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> addDefinedGroupName: groupName includeGroups: includedGroups [

	"add a group that does not include any other groups"

	self definedGroupNames at: groupName put: includedGroups asArray sort

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames [

	^ definedGroupNames ifNil: [ definedGroupNames :=  Dictionary new ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> definedGroupNames: aColl [

	definedGroupNames := aColl

]
