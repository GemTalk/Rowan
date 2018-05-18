Class {
	#name : 'RowanPackageUpdate',
	#superclass : 'RowanCommandResult',
	#instVars : [
		'packages',
		'modifiedPackages',
		'selections'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'initialization' }
RowanPackageUpdate >> initialize [

	packages := Array new. 
	modifiedPackages := Array new.
	selections := Array new.

]

{ #category : 'accessing' }
RowanPackageUpdate >> modifiedPackages [

	^modifiedPackages

]

{ #category : 'accessing' }
RowanPackageUpdate >> packages [

	^packages

]

{ #category : 'accessing' }
RowanPackageUpdate >> selections [

	^selections

]

{ #category : 'jade' }
RowanPackageUpdate >> writeModifiedPackagesTo: writeStream [

	modifiedPackages do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanPackageUpdate >> writePackagesTo: writeStream [

	packages do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]

{ #category : 'jade' }
RowanPackageUpdate >> writeSelectionsTo: writeStream [

	selections do: [:each | writeStream nextPutAll: each; tab].
	writeStream lf.

]
