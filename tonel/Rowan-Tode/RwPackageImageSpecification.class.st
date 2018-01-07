Class {
	#name : 'RwPackageImageSpecification',
	#superclass : 'RwImageSpecification',
	#instVars : [
		'loadedPackages'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'accessing' }
RwPackageImageSpecification >> loadedPackages [

   ^loadedPackages

]

{ #category : 'accessing' }
RwPackageImageSpecification >> loadedPackages: anObject [

   loadedPackages := anObject

]

{ #category : 'copying' }
RwPackageImageSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy.
  loadedPackages := loadedPackages copy
]

{ #category : 'actions' }
RwPackageImageSpecification >> updateLoadedPackageNamesTool: aRwTool packageNames: aPackageNames [
  ^ aRwTool doUpdateLoadedPackageNames: aPackageNames
]
