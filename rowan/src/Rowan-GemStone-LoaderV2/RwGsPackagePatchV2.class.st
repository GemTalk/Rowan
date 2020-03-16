Class {
	#name : 'RwGsPackagePatchV2',
	#superclass : 'RwGsPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsPackagePatchV2 class >> for: aPackageDefinition [

	^self new packageDefinition: aPackageDefinition
]
