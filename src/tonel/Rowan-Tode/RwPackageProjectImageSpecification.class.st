Class {
	#name : 'RwPackageProjectImageSpecification',
	#superclass : 'RwImageSpecification',
	#instVars : [
		'loadedPackageNames'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwPackageProjectImageSpecification >> loadedPackageNames [

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

{ #category : 'actions' }
RwPackageProjectImageSpecification >> updateLoadedPackageNamesTool: aRwTool packageNames: aPackageNames [
  ^ aRwTool doUpdateLoadedPackageNames: aPackageNames
]
