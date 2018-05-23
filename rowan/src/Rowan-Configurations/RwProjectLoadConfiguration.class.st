Class {
	#name : 'RwProjectLoadConfiguration',
	#superclass : 'RwAbstractProjectConfiguration',
	#instVars : [
		'conditionalPackages',
		'conditionalPackageMatchers',
		'conditionalPackageMapSpecs'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'private' }
RwProjectLoadConfiguration >> _platformPatternMatcherFor: pattern [

	" Returns an instance of RwAbstractConfigurationPlatformAttributeMatcher:
		RwStringConfigurationPlatformAttributeMatcher,
		RwGemStoneVersionConfigurationPlatformAttributeMatcher,
		or RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher
	"

	| versionPattern |
	(pattern beginsWith: 'gs')
		ifFalse: [ 
			"simple equality match"
			^ RwStringConfigurationPlatformAttributeMatcher new
					pattern: pattern;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	"GemStone version pattern"
	versionPattern := (pattern copyFrom: 3 to: pattern size) substrings: '.'.
	versionPattern last = 'x' 
		ifFalse: [
			"specific version number match, use ="
			^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
					pattern: (pattern copyFrom: 3 to: pattern size) asRwGemStoneVersionNumber;
					patternMatchBlock: [:a :b | a = b ];
					yourself ].
	(pattern includesString: '[')
		ifTrue: [ 
			| vpSize gsVersion1 gsVersion2 rangePattern dashIndex |
			"range pattern"
			vpSize := versionPattern size.
			gsVersion1 := RwGemStoneVersionNumber new: (vpSize - 1).
			1 to: vpSize - 2
				do: [:index | gsVersion1 at: index put: (versionPattern at: index) asInteger ].
			gsVersion1 at: vpSize -1 put: 0.
			rangePattern := (versionPattern at: vpSize - 1) trimBoth.
			(((rangePattern at: 1) = $[) and: [ (rangePattern at: rangePattern size) = $] ])
				ifFalse: [ self error: 'Poorly formed GemStone version range pattern ', rangePattern printString ].
			rangePattern := rangePattern copyFrom: 2 to: rangePattern size -1.
			dashIndex := rangePattern indexOf: $-.
			dashIndex = rangePattern size
				ifTrue: [ 
					"open ended range use ~>"
					^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
						pattern:
							(gsVersion1 
								at: vpSize -1 put: (rangePattern copyFrom: 1 to: rangePattern size -1) asInteger;
								yourself);
						patternMatchBlock: [:a :b | b ~> a ];
						yourself ].
			"closed range"
			gsVersion1 at: vpSize -1 put: (rangePattern copyFrom: 1 to: dashIndex -1) asInteger.
			gsVersion2 := gsVersion1 copy.
			gsVersion2 at: vpSize -1 put: (rangePattern copyFrom: dashIndex + 1 to: rangePattern size) asInteger.
			^ RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher new
					pattern: gsVersion1;
					pattern2: gsVersion2;
					patternMatchBlock: [:a :b :c | (a <= b) & (b <= c ) ];
					yourself ].
	" 'gs', <gemstone-version-number> , '.x'"
	"no range pattern - use ~> comparison"
	^ RwGemStoneVersionConfigurationPlatformAttributeMatcher new
			pattern: (pattern copyFrom: 3 to: pattern size - 2) asRwGemStoneVersionNumber;
			patternMatchBlock: [:a :b | b ~> a ];
			yourself

]

{ #category : 'visiting' }
RwProjectLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitProjectLoadConfiguration: self

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackageMapSpecs [

	^conditionalPackageMapSpecs ifNil: [ conditionalPackageMapSpecs := Dictionary new ]

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackageMatchers [

	conditionalPackageMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMatchers := Dictionary new.
	self conditionalPackages keysAndValuesDo: [:platformPatterns :groupMap |
		conditionalPackageMatchers
			at: (platformPatterns collect: [:pattern | self _platformPatternMatcherFor: pattern ])
			put: groupMap ].
	^ conditionalPackageMatchers

]

{ #category : 'accessing' }
RwProjectLoadConfiguration >> conditionalPackages [

	^conditionalPackages ifNil: [ conditionalPackages := Dictionary new ]

]
