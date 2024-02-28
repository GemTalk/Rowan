"
This notification is signalled when an project set load encounters a method (or class) that is already being managed by another project, that is not present in the incoming project set. If unhandled, by default the method (or class) will be moved to the incoming project/package.
"
Class {
	#name : 'RwExistingVisitorChangingPackageOwnershipNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'incomingProject',
		'incomingPackage',
		'loadedClassOrMethodDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'handling' }
RwExistingVisitorChangingPackageOwnershipNotification >> defaultAction [
	"move defintion to incoming project/package"

	^ true
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> incomingPackage [
	^incomingPackage
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> incomingPackage: object [
	incomingPackage := object
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> incomingProject [
	^incomingProject
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> incomingProject: object [
	incomingProject := object
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> loadedClassOrMethodDefinition [
	^loadedClassOrMethodDefinition
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> loadedClassOrMethodDefinition: object [
	loadedClassOrMethodDefinition := object
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> loadedPackage [
	^self loadedClassOrMethodDefinition loadedPackage
]

{ #category : 'accessing' }
RwExistingVisitorChangingPackageOwnershipNotification >> loadedProject [
	^self loadedClassOrMethodDefinition loadedProject
]
