Class {
	#name : 'RwAbstractProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectConfiguration',
	#instVars : [
		'conditionalPackages',
		'conditionalPackageMatchers',
		'conditionalPackageMapSpecs',
		'conditionalPackageMapSpecMatchers'
	],
	#category : 'RowanV12Metadata-Core'
}

{ #category : 'private' }
RwAbstractProjectLoadConfiguration >> _platformPatternMatcherFor: pattern [

	" Returns an instance of RwAbstractConfigurationPlatformAttributeMatcher:
		RwStringConfigurationPlatformAttributeMatcher,
		RwGemStoneVersionConfigurationPlatformAttributeMatcher,
		or RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher
	"

	| versionPattern gsVersion1 gsVersion2 |
	(pattern beginsWith: 'gs')
		ifFalse: [ 
			"simple equality match"
			^ RwStringConfigurationPlatformAttributeMatcher new
					pattern: pattern;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	"GemStone version pattern"
	versionPattern := (pattern copyFrom: 3 to: pattern size) substrings: '.'.
	(versionPattern last beginsWith: '[')
		ifTrue: [ 
			| vpSize rangePattern dashIndex |
			"range pattern"
			vpSize := versionPattern size.
			gsVersion1 := RwGemStoneVersionNumber new: vpSize .
			1 to: vpSize - 1
				do: [:index | gsVersion1 at: index put: (versionPattern at: index) asInteger ].
			gsVersion1 at: vpSize put: 0.
			rangePattern := (versionPattern at: vpSize) trimBoth.
			(((rangePattern at: 1) = $[) and: [ (rangePattern at: rangePattern size) = $] ])
				ifFalse: [ self error: 'Poorly formed GemStone version range pattern ', rangePattern printString, ' in ', pattern printString ].
			rangePattern := rangePattern copyFrom: 2 to: rangePattern size -1.
			dashIndex := rangePattern indexOf: $-.
			dashIndex <= 1
				ifTrue: [ self error: 'Invalid version range pattern missing range begin' , rangePattern printString, ' in ', pattern printString ].
			gsVersion1 at: vpSize put: (rangePattern copyFrom: 1 to: dashIndex -1) asInteger.
			dashIndex = rangePattern size
				ifTrue: [
					"open range"
					gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size -1.
					gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
					^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
							pattern: gsVersion1;
							pattern2: gsVersion2;
							patternMatchBlock: [:a :b :c | (a <= b) & (b < c ) ];
							yourself ].
			"closed range"
			gsVersion2 := gsVersion1 copy.
			gsVersion2 at: vpSize put: (rangePattern copyFrom: dashIndex + 1 to: rangePattern size) asInteger.
			^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
					pattern: gsVersion1;
					pattern2: gsVersion2;
					patternMatchBlock: [:a :b :c | (a <= b) & (b <= c ) ];
					yourself ].
	versionPattern last = 'x' 
		ifFalse: [
			"specific version number match, use ="
			^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
					pattern: (pattern copyFrom: 3 to: pattern size) asRwGemStoneVersionNumber;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	" 'gs', <gemstone-version-number> , '.x'"
	"match all values in x field"
	gsVersion1 := ((pattern copyFrom: 3 to: pattern size - 2), '.0') asRwGemStoneVersionNumber.
	gsVersion2 := gsVersion1 copyFrom: 1 to: gsVersion1 size - 1.
	gsVersion2 at: gsVersion2 size put: (gsVersion2 at: gsVersion2 size) + 1.
	^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
			pattern: gsVersion1;
			pattern2: gsVersion2;
			patternMatchBlock: [:a :b :c | (a <= b) & (b < c ) ];
			yourself

]

{ #category : 'visiting' }
RwAbstractProjectLoadConfiguration >> acceptCompoundVisitor: aVisitor [

	self error: 'project load configurations may not be nested inside of compound configurations'

]

{ #category : 'private' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMapSpecMatchers [

	conditionalPackageMapSpecMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMapSpecMatchers := Dictionary new.
	self conditionalPackageMapSpecs keysAndValuesDo: [:platformPattern :packageMapSpecsMap |
		conditionalPackageMapSpecMatchers
			at: (self _platformPatternMatcherFor: platformPattern)
			put: packageMapSpecsMap ].
	^ conditionalPackageMapSpecMatchers

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMapSpecs [

	^ conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMapSpecsAtGemStoneUserId: userId [ 

	^ ((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsent: [ ^ Dictionary new ])

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName [

	^ (((self conditionalPackageMapSpecs at: 'gemstone' ifAbsent: [ ^ Dictionary new ])
		at: userId ifAbsentPut: [ ^ Dictionary new ])
			at: #packageNameToPlatformPropertiesMap ifAbsent: [ ^ Dictionary new ])
				at: packageName ifAbsent: [ ^ Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMapSpecsAtGemStoneUserId: userId andPackageName: packageName setSymbolDictNameTo: symbolDictName [

	((((self conditionalPackageMapSpecs at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #packageNameToPlatformPropertiesMap ifAbsentPut: [ Dictionary new ])
				at: packageName ifAbsentPut: [ Dictionary new ])
					at: 'symbolDictName' put: symbolDictName asString

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMapSpecsAtGemStoneUserId: userId setDefaultSymbolDictNameTo: defaultSymbolDictName [

	((self conditionalPackageMapSpecs at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultSymbolDictName put: defaultSymbolDictName asString

]

{ #category : 'private' }
RwAbstractProjectLoadConfiguration >> conditionalPackageMatchers [

	conditionalPackageMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMatchers := Dictionary new.
	self conditionalPackages keysAndValuesDo: [:platformPatterns :groupMap |
		conditionalPackageMatchers
			at: (platformPatterns collect: [:pattern | self _platformPatternMatcherFor: pattern ])
			put: groupMap ].
	^ conditionalPackageMatchers

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackages [

	^ conditionalPackages ifNil: [ conditionalPackages := Dictionary new ]

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackagesAtConditions: conditions andGroup: groupName [

	| thePackageNameMap |
	thePackageNameMap := (self conditionalPackages at: conditions asArray sort ifAbsent: [ ^ Set new])
		at: groupName ifAbsent: [ ^ Set new ].
	^ (thePackageNameMap at: #packageNames ifAbsent: [ Set new ]) asSet

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> conditionalPackagesAtConditions: conditions andGroup: groupName addPackageNames: packageNames [

	| thePackageNames thePackageNameMap |
	thePackageNameMap := (self conditionalPackages at: conditions asArray sort ifAbsentPut: [ Dictionary new])
		at: groupName ifAbsentPut: [ Dictionary new ].
	thePackageNames := (thePackageNameMap at: #packageNames ifAbsentPut: [ Set new ]) asSet.
	thePackageNames addAll: packageNames.
	thePackageNameMap at: #packageNames put: thePackageNames asArray sort

]

{ #category : 'accessing' }
RwAbstractProjectLoadConfiguration >> definedGroupNames [
	"Empty list by default"

	^ #()

]

{ #category : 'initialization' }
RwAbstractProjectLoadConfiguration >> initializeForExport [

	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	super initializeForExport.
	conditionalPackageMatchers :=  conditionalPackageMapSpecMatchers := nil

]
