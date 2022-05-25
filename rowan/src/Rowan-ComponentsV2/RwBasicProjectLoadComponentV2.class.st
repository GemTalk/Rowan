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
RwBasicProjectLoadComponentV2 >> conditionalPackageMapSpecs: aDict [
	"duplication of subclass implementation until we've moved to V2.1, since this method is 
		used during project load and superclass methods removed before subclass methods added.
		This duplication will go away, once this obsolete class is removed (planned for v2.1)."

	 conditionalPackageMapSpecs := aDict
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

{ #category : 'copying' }
RwBasicProjectLoadComponentV2 >> postCopy [
	super postCopy.
	doitDict := doitDict copy.
	conditionalPackageMapSpecs
		ifNotNil: [ 
			| packageMapSpecsCopy |
			packageMapSpecsCopy := conditionalPackageMapSpecs copy.
			conditionalPackageMapSpecs
				keysAndValuesDo: [ :platformName :userMap | 
					platformName = 'gemstone'
						ifTrue: [ 
							| userMapCopy |
							userMapCopy := userMap copy.
							packageMapSpecsCopy at: platformName put: userMapCopy.
							userMap
								keysAndValuesDo: [ :userName :attributeMap | 
									| attributeMapCopy |
									attributeMapCopy := attributeMap copy.
									userMapCopy at: userName put: attributeMapCopy.
									attributeMap
										keysAndValuesDo: [ :attributeName :packageMap | 
											| packageMapCopy |
											packageMapCopy := packageMap copy.
											attributeMapCopy at: attributeName put: packageMapCopy.
											packageMap
												keysAndValuesDo: [ :packageName :packageAttribute | packageMapCopy at: packageName put: packageAttribute copy ] ] ] ]
						ifFalse: [ self error: 'Unknown platform name ' , platformName printString ] ].
			conditionalPackageMapSpecs := packageMapSpecsCopy ]
]
