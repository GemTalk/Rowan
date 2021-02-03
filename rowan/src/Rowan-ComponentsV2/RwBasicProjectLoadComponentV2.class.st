Class {
	#name : 'RwBasicProjectLoadComponentV2',
	#superclass : 'Object',
	#instVars : [
		'name',
		'comment',
		'projectName',
		'conditionalPackageMapSpecs',
		'preloadDoitName',
		'postloadDoitName',
		'doitDict'
	],
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'dispatching' }
RwBasicProjectLoadComponentV2 >> _addToResolvedProjectComponents: aRwResolvedProjectComponentsV2 [
	"dispatch to _addActiveComponent: or _addPackageGroup: as appropriate"

	aRwResolvedProjectComponentsV2 _addActiveComponent: self
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecs [
	"duplication of subclass implementation until we've moved to V2.1, since this method is 
		used during project load and superclass methods removed before subclass methods added.
		This duplication will go away, once this obsolete class is removed (planned for v2.1)."

	^ conditionalPackageMapSpecs
		ifNil: [ conditionalPackageMapSpecs := Dictionary new ]
]

{ #category : 'accessing' }
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecsAtGemStoneUserId: userId [
	"duplication of subclass implementation until we've moved to V2.1, since this method is 
		used during project load and superclass methods removed before subclass methods added.
		This duplication will go away, once this obsolete class is removed (planned for v2.1)."

	^ (self conditionalPackageMapSpecs
		at: 'gemstone'
		ifAbsent: [ ^ Dictionary new ]) at: userId ifAbsent: [ ^ Dictionary new ]
]
