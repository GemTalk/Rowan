Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectConfiguration',
	#instVars : [
		'conditionalPackages',
		'conditionalPackageMapSpecs'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'private' }
RwProjectLoadConfiguration class >> _readStonFrom: stream [

	| reader | 
	(reader := STONReader on: stream)
		allowComplexMapKeys: true.
	^reader next
		initializeForImport;
		yourself

]
