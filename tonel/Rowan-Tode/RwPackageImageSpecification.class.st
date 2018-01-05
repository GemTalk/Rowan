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
