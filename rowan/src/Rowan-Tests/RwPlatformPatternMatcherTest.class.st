Class {
	#name : 'RwPlatformPatternMatcherTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwPlatformPatternMatcherTest >> _patternMatcherFor: pattern [
	^ RwAbstractComponent _platformPatternMatcherFor: pattern
]

{ #category : 'tests' }
RwPlatformPatternMatcherTest >> testGemStoneVersionPattern [
	| matcher pattern |
	pattern := 'gs3.1.4'.
	matcher := self _patternMatcherFor: pattern.
	self assert: matcher class == RwGemStoneVersionConfigurationPlatformAttributeMatcher.
	self assert: (matcher match: '3.1.4' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.1.4.1' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.1.3' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.1' asRwGemStoneVersionNumber).
	self deny: (matcher match: '2.1.4' asRwGemStoneVersionNumber).
]

{ #category : 'tests' }
RwPlatformPatternMatcherTest >> testGemStoneVersionRangePattern [
	| matcher pattern |
	pattern := 'gs3.[2-6]'.
	matcher := self _patternMatcherFor: pattern.
	self assert: matcher class == RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher.
	self assert: (matcher match: '3.2.0' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.2.0.10' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.2.0.0.1' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.3' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.4' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.5' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6.1' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6.100' asRwGemStoneVersionNumber).

	self deny: (matcher match: '3.1.0' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.7.0' asRwGemStoneVersionNumber).

	pattern := 'gs3.6.[2-6]'.
	matcher := self _patternMatcherFor: pattern.
	self assert: matcher class == RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher.
	self deny: (matcher match: '3.6.1' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6.2' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6.3.5.7' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6.6' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.6.6.5' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.6.7' asRwGemStoneVersionNumber).

	pattern := 'gs[2-6]'.
	matcher := self _patternMatcherFor: pattern.
	self assert: matcher class == RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher.
	self deny: (matcher match: '1.0.0' asRwGemStoneVersionNumber).
	self assert: (matcher match: '2.0.0' asRwGemStoneVersionNumber).
	self assert: (matcher match: '6.0.0' asRwGemStoneVersionNumber).
	self assert: (matcher match: '6.5' asRwGemStoneVersionNumber).
	self deny: (matcher match: '7.0.0' asRwGemStoneVersionNumber).
]

{ #category : 'tests' }
RwPlatformPatternMatcherTest >> testGemStoneVersionWildCardPattern [
	| matcher pattern |
	pattern := 'gs3.1.x'.
	matcher := self _patternMatcherFor: pattern.
	self assert: matcher class == RwGemStoneVersionRangeConfigurationPlatformAttributeMatcher.
	self assert: (matcher match: '3.1.5.6' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.1.5' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.1' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3' asRwGemStoneVersionNumber).
	self deny: (matcher match: '2' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.0' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3.2' asRwGemStoneVersionNumber).

	pattern := 'gs3.x'.
	matcher := self _patternMatcherFor: pattern.
	self assert: (matcher match: '3.1.5' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.1' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3' asRwGemStoneVersionNumber).
	self deny: (matcher match: '2' asRwGemStoneVersionNumber).

	pattern := 'gs3.15.x'.
	matcher := self _patternMatcherFor: pattern.
	self assert: (matcher match: '3.15.5.1' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.15.5' asRwGemStoneVersionNumber).
	self assert: (matcher match: '3.15' asRwGemStoneVersionNumber).
	self deny: (matcher match: '3' asRwGemStoneVersionNumber).
	self deny: (matcher match: '2' asRwGemStoneVersionNumber).
]

{ #category : 'tests' }
RwPlatformPatternMatcherTest >> testPatternErrors [
	| matcher pattern hitExpectedError |
	hitExpectedError := false.
	pattern := 'gs3.[2-6'.
	[ matcher := self _patternMatcherFor: pattern ]
		on: Error
		do: [ :ex | 
			self
				assert: ex description
				equals:
					'UserDefinedError: Poorly formed GemStone version range pattern ''[2-6'' in ''gs3.[2-6'''.
			hitExpectedError := true ].
	self assert: hitExpectedError.

	hitExpectedError := false.
	pattern := 'gs3[2-6'.
	[ matcher := self _patternMatcherFor: pattern ]
		on: Error
		do: [ :ex | 
			self
				assert: ex description
				equals:
					'UserDefinedError: Encountered a non-digit character $[ in a numeric version number field'.
			hitExpectedError := true ].
	self assert: hitExpectedError.

	hitExpectedError := false.
	pattern := 'gs3.[26]'.
	[ matcher := self _patternMatcherFor: pattern ]
		on: Error
		do: [ :ex | 
			self
				assert: ex description
				equals:
					'UserDefinedError: Invalid version range pattern missing range begin''26'' in ''gs3.[26]'''.
			hitExpectedError := true ].
	self assert: hitExpectedError.

	hitExpectedError := false.
	pattern := 'gs3x]'.
	[ matcher := self _patternMatcherFor: pattern ]
		on: Error
		do: [ :ex | 
			self assert: ex description equals: 'UserDefinedError: Encountered a non-digit character $x in a numeric version number field'.
			hitExpectedError := true ].
	self assert: hitExpectedError.

	hitExpectedError := false.
	pattern := 'gs3.2-6'.
	[ matcher := self _patternMatcherFor: pattern ]
		on: Error
		do: [ :ex | 
			self assert: ex description equals: 'UserDefinedError: Encountered a non-digit character $- in a numeric version number field'.
			hitExpectedError := true ].
	self assert: hitExpectedError
]

{ #category : 'tests' }
RwPlatformPatternMatcherTest >> testStringPattern [
	| matcher pattern |
	pattern := 'common'.
	matcher := self _patternMatcherFor: pattern.
	self assert: matcher class == RwStringConfigurationPlatformAttributeMatcher.
	self assert: (matcher match: 'common').
	self deny: (matcher match: 'commons')
]
