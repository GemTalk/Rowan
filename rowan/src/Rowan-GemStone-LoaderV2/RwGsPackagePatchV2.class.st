Class {
	#name : 'RwGsPackagePatchV2',
	#superclass : 'RwGsPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsPackagePatchV2 class >> for: aPackageDefinition [

	^self new packageDefinition: aPackageDefinition
]

{ #category : 'printing' }
RwGsPackagePatchV2 >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '(' , packageDefinition name , ')'
]
