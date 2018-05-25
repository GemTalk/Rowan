Class {
	#name : 'RwPackageProjectImageSpecification',
	#superclass : 'RwImageSpecification',
	#instVars : [
		'loadedPackageNames'
	],
	#category : 'Rowan-Deprecated'
}

{ #category : 'accessing' }
RwPackageProjectImageSpecification >> loadedPackageNames [

	"method and iv should go away when Rowan is managed in Rowan"

	^ loadedPackageNames ifNil: [ #() ]
]

{ #category : 'accessing' }
RwPackageProjectImageSpecification >> loadedPackageNames: anObject [

	loadedPackageNames := anObject
]

{ #category : 'copying' }
RwPackageProjectImageSpecification >> postCopy [

	"don't forget to copy any compound state objects"

	super postCopy.
	loadedPackageNames := loadedPackageNames copy
]
