Class {
	#name : 'RwNestedProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectLoadConfiguration',
	#category : 'Rowan-Configurations'
}

{ #category : 'visiting' }
RwNestedProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitNestedProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwNestedProjectLoadConfiguration >> conditionalPackageMapSpecs [
	"No conditional package map specs in nested configs"

	^ Dictionary new

]

{ #category : 'accessing' }
RwNestedProjectLoadConfiguration >> definedGroupNames [
	"No group names in nested configs"

	^ #()

]
