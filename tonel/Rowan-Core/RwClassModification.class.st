"
Represents modifications to a single class definition or class extension.

Instvars:

propertiesModification			PropertiesModification -- changes to class or class extension properties
instanceMethodsModification	MethodsModification -- changes to instance methods defined
classMethodsModification		MethodsModification -- changes to class methods defined
"
Class {
	#name : 'RwClassModification',
	#superclass : 'RwModification',
	#instVars : [
		'instanceMethodsModification',
		'classMethodsModification'
	],
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwClassModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassModification: self
]

{ #category : 'patching' }
RwClassModification >> addModificationToPatchSet: aPatchSet inPackage: aPackage [

	self isAddition
		ifTrue: [ 
			aPatchSet addAddedClass: self after inPackage: aPackage.
			propertiesModification isEmpty
				ifFalse: [self halt: 'Need to do something with modified properties'] ].
	self isDeletion
		ifTrue: 
			[ aPatchSet addDeletedClass: self before inPackage: aPackage ].
	self isModification
		ifTrue: [ 
			RwGsClassVersioningPatch 
				addPatchedClassModification: self 
				inPackage: aPackage 
				toPatchSet: aPatchSet ].
]

{ #category : 'Accessing' }
RwClassModification >> classMethodsModification [
	^classMethodsModification

]

{ #category : 'Updating' }
RwClassModification >> classMethodsModification: newValue [
	classMethodsModification := newValue

]

{ #category : 'initialization' }
RwClassModification >> initialize [
	"Dummy initialization that understands isEmpty for printing."

	super initialize.
	instanceMethodsModification := #().
	classMethodsModification := #()
]

{ #category : 'Accessing' }
RwClassModification >> instanceMethodsModification [
	^instanceMethodsModification

]

{ #category : 'Updating' }
RwClassModification >> instanceMethodsModification: newValue [
	instanceMethodsModification := newValue

]

{ #category : 'testing' }
RwClassModification >> isEmpty [

	^super isEmpty & instanceMethodsModification isEmpty
		& classMethodsModification isEmpty
]

{ #category : 'merging' }
RwClassModification >> mergeForNewClassVersionWith: aClassModification [

	"merge aClassModification modifictions into the receiver"

	after == aClassModification after
		ifFalse: [ self error: 'internal error - both modifications must share a common after' ].
	aClassModification propertiesModification isEmpty
		ifFalse: [ 
			self
				error:
					'internal error - an incoming class modification is not expected to have properties when a new class version is involved' ].
	instanceMethodsModification
		mergeForNewClassVersionWith: aClassModification instanceMethodsModification.
	classMethodsModification
		mergeForNewClassVersionWith: aClassModification classMethodsModification
]
