"
# Metacello version format

Thanks to [Mozilla Toolkit version format](https://developer.mozilla.org/en/Toolkit_version_format) for inspiration.

##Version Format
A version string consists of one or more version parts, separated with dots or dashes.

A version part with a leading dot is numeric. A version part with a leading dash is string.

The rationale behind splitting a version part into a sequence of strings and numbers is that when comparing version parts, the numeric parts are compared as numbers, e.g. '1.0-pre.1' < '1.0-pre.10', while the strings are compared bytewise. See the next section for details on how versions are compared.

##Comparing versions

When two version strings are compared, their version parts are compared left to right. Empty parts are ignored.

If at some point a version part of one version string is greater than the corresponding version part of another version string, then the first version string is greater than the other one.

If a version string has extra parts and the common parts are equal, the shorter version string is less than the longer version string (1.0 is less than 1.0.0).

Otherwise, the version strings are equal. 

##Comparing version parts

Version parts are also compared left to right, A string-part that exists is always less-then a nonexisting string-part (1.6-a is less than 1.6).

Examples

```
1 == 1. < 1.0 == 1..--0
< 1.1-a < 1.1-aa < 1.1-ab < 1.1-b < 1.1-c
< 1.1-pre < 1.1-pre.0 
< 1.1-pre.1-a < 1.1-pre.1-aa < 1.1-pre.1-b < 1.1-pre.1
< 1.1-pre.2
< 1.1-pre.10
< 1.1 < 1.1.0 < 1.1.00
< 1.10
< 2.0
```
"
Class {
	#name : 'RwGemStoneVersionNumber',
	#superclass : 'Magnitude',
	#type : 'variable',
	#category : 'Rowan-Configurations'
}

{ #category : 'private' }
RwGemStoneVersionNumber class >> extractNumericComponent: subString [
	"$. separated components are integers"

	| number |
	number := [subString asNumber] 
						on: Error 
						do: [:ex | ex return: subString ].
	^number asString = subString
		ifTrue: [ number ]
		ifFalse: [ subString ]

]

{ #category : 'instance creation' }
RwGemStoneVersionNumber class >> fromString: aString [

	| new components |
	components := OrderedCollection new.
	(aString substrings: '.') do: [:subString | | strs |
		strs := subString findTokens: '-'.
		"first subString token could be an integer"
		components add: (self extractNumericComponent: strs first).
		strs size > 1
			ifTrue: [
				"remaining are uncoditionally Strings, because of leading $-"
				components addAll: strs allButFirst ]].
	new := self new: components size.
	1 to: components size do: [:i | new at: i put: (components at: i) ].
	^new

]

{ #category : 'comparing' }
RwGemStoneVersionNumber >> < aRwGemStoneVersionNumber [

	| condensed aCondensed |
	aRwGemStoneVersionNumber species = self species
		ifFalse: [ ^ false ].
	condensed := self collapseZeros.
	aCondensed := aRwGemStoneVersionNumber collapseZeros.
	(condensed ~~ self or: [ aCondensed ~~ aRwGemStoneVersionNumber ])
		ifTrue: [ ^ condensed compareLessThan: aCondensed ].
	^ self compareLessThan: aRwGemStoneVersionNumber

]

{ #category : 'comparing' }
RwGemStoneVersionNumber >> = aRwGemStoneVersionNumber [

	| condensed aCondensed |
	aRwGemStoneVersionNumber species = self species
		ifFalse: [ ^ false ].
	condensed := self collapseZeros.
	aCondensed := aRwGemStoneVersionNumber collapseZeros.
	(condensed ~~ self or: [ aCondensed ~~ aRwGemStoneVersionNumber ])
		ifTrue: [ ^ condensed compareEqualTo: aCondensed ].
	^ self compareEqualTo: aRwGemStoneVersionNumber

]

{ #category : 'comparing' }
RwGemStoneVersionNumber >> ~> aRwGemStoneVersionNumber [

	aRwGemStoneVersionNumber size == 1 ifTrue: [ ^false ].
	^self >= aRwGemStoneVersionNumber and: [ self < aRwGemStoneVersionNumber approximateBase ]

]

{ #category : 'accessing' }
RwGemStoneVersionNumber >> approximateBase [

	| base |
	base := self copyFrom: 1 to: self size - 1.
	base at: base size put: (base at: base size) + 1.
	^base

]

{ #category : 'converting' }
RwGemStoneVersionNumber >> asRwGemStoneVersionNumber [

	^self

]

{ #category : 'printing' }
RwGemStoneVersionNumber >> asString [
	"Answer a string that represents the receiver."

	^ self printString

]

{ #category : 'private' }
RwGemStoneVersionNumber >> collapseZeros [
	"the rule must be that zeros can be collapsed as long as the series of zeros ends in a string term"

	| collection newSize new j lastElementIsStringOrZero canCollapse |
	(self size = 0 or: [ self at: 1 ]) == 0
		ifTrue: [ ^ self ].
	collection := OrderedCollection new.
	lastElementIsStringOrZero := true.
	canCollapse := true.
	self size to: 1 by: -1 do: [ :i | 
		| element |
		element := self at: i.
		(canCollapse and: [ element == 0 ])
			ifTrue: [ 
				lastElementIsStringOrZero
					ifFalse: [ 
						canCollapse := false.
						collection addFirst: element.]]
			ifFalse: [ 
				collection addFirst: element.
				canCollapse := lastElementIsStringOrZero := element isString ] ].
	collection size = self size
		ifTrue: [ ^ self ].
	newSize := collection size.
	new := self species new: newSize.
	j := 0.
	collection
		do: [ :element | 
			new at: j + 1 put: element.
			j := j + 1 ].
	^ new

]

{ #category : 'private' }
RwGemStoneVersionNumber >> compareEqualTo: aRwGemStoneVersionNumber [

	| mySize |
	aRwGemStoneVersionNumber species = self species ifFalse: [ ^false ].
	mySize := self size.
	mySize = aRwGemStoneVersionNumber size 
		ifFalse: [ ^false ].
	1 to: mySize do: [:i |
		(self at: i) = (aRwGemStoneVersionNumber at: i) ifFalse: [ ^false ]].
	^true

]

{ #category : 'private' }
RwGemStoneVersionNumber >> compareLessThan: aRwGemStoneVersionNumber [

	| mySize aSize commonSize count more |
	mySize := self size.
	aSize := aRwGemStoneVersionNumber size.
	commonSize :=  mySize min: aSize.
	count := 0.
	more := true.
	[ more and: [ count < commonSize ]] whileTrue: [
		(self at: count + 1) = (aRwGemStoneVersionNumber at: count + 1)
			ifTrue: [ count := count + 1 ]
			ifFalse: [ more := false ]].
	count < commonSize
		ifTrue: [ 
			^(self at: count + 1) 
				metacelloVersionComponentLessThan: (aRwGemStoneVersionNumber at: count + 1) ].
	mySize < aSize
		ifTrue: [ 
			mySize = 0 ifTrue: [ ^true ].
			"if the versions at commonSize are equal and the next version slot in aRwGemStoneVersionNumber 
			 is a string, then it's considered that I'm > aRwGemStoneVersionNumber
			 (i.e., '2.9.9' is greater than '2.9.9-alpha.2')"
			(self at: commonSize) = (aRwGemStoneVersionNumber at: commonSize)
				ifFalse: [ ^true ]. 
			^(aRwGemStoneVersionNumber at: commonSize+1) isString not]
		ifFalse: [ 
			mySize = aSize ifTrue: [ ^false ].
			aSize <= 0 ifTrue: [ ^false ].
			"if the versions at commonSize are equal and the next version slot is a string, 
			 then it's considered that I'm < aRwGemStoneVersionNumber
			 (i.e., '2.9.9-alpha.2' is less than '2.9.9')"
			(self at: commonSize) = (aRwGemStoneVersionNumber at: commonSize)
				ifFalse: [ ^false ].
			 ^(self at: commonSize+1) isString]

]

{ #category : 'copying' }
RwGemStoneVersionNumber >> copyFrom: start to: stop [ 
	"Answer a copy of a subset of the receiver, starting from element at 
	index start until element at index stop."

	| newSize new j |
	newSize := stop - start + 1.
	new := self species new: newSize.
	j := 0.
	start to: stop do: [:i |
		new at: j + 1 put: (self at: i).
		j := j + 1 ].
	^new

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> decrementMajorVersion [
  self decrementVersionAt: 1

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> decrementMinorVersion [
  self size < 2
    ifTrue: [ self at: 2 put: 0 ].
  self decrementVersionAt: 2

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> decrementMinorVersionNumber [
	| int |
	self size to: 1 by: -1 do: [ :index | 
		(int := self at: index) isString
			ifFalse: [ 
				int > 0
					ifTrue: [ self at: index put: int - 1 ].
				^ self ] ]

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> decrementPatchVersion [
  self size < 2
    ifTrue: [ self at: 2 put: 0 ].
  self size < 3
    ifTrue: [ self at: 3 put: 0 ].
  self decrementVersionAt: 3

]

{ #category : 'private' }
RwGemStoneVersionNumber >> decrementVersionAt: index [
  | int |
  int := self at: index.
  (int := self at: index) isString
    ifFalse: [ 
      int > 0
        ifTrue: [ self at: index put: int - 1 ] ]

]

{ #category : 'enumerating' }
RwGemStoneVersionNumber >> do: aBlock [ 
	"Refer to the comment in Collection|do:."
	1 to: self size do:
		[:index | aBlock value: (self at: index)]

]

{ #category : 'enumerating' }
RwGemStoneVersionNumber >> do: elementBlock separatedBy: separatorBlock [
	"Evaluate the elementBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [separatorBlock value].
		elementBlock value: each]

]

{ #category : 'comparing' }
RwGemStoneVersionNumber >> hash [

"Returns a numeric hash key for the receiver."

| mySize interval hashValue |

(mySize := self size) == 0
  ifTrue: [ ^15243 ].

"Choose an interval so that we sample at most 5 elements of the receiver"
interval := ((mySize - 1) // 4) max: 1.

hashValue := 4459.
1 to: mySize by: interval do: [ :i | | anElement |
  anElement := self at: i.
  (anElement isKindOf: SequenceableCollection)
    ifTrue: [
      hashValue := (hashValue bitShift: -1) bitXor: anElement size.
      ]
    ifFalse: [
      hashValue := (hashValue bitShift: -1) bitXor: anElement hash.
      ].
  ].

^ hashValue abs

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> incrementMajorVersion [
  self incrementVersionAt: 1

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> incrementMinorVersion [
  self size < 2
    ifTrue: [ self at: 2 put: 0 ].
  self incrementVersionAt: 2

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> incrementMinorVersionNumber [

	| int |
	self size to: 1 by: -1 do: [:index | 
		(int := self at: index) isString 
			ifFalse: [ 
				self at: index put: int + 1.
				^self ]].

]

{ #category : 'operations' }
RwGemStoneVersionNumber >> incrementPatchVersion [
  self size < 2
    ifTrue: [ self at: 2 put: 0 ].
  self size < 3
    ifTrue: [ self at: 3 put: 0 ].
  self incrementVersionAt: 3

]

{ #category : 'private' }
RwGemStoneVersionNumber >> incrementVersionAt: index [
  | int |
  int := self at: index.
  (int := self at: index) isString
    ifFalse: [ self at: index put: int + 1 ]

]

{ #category : 'comparing' }
RwGemStoneVersionNumber >> match: aVersionPattern [
	"Answer whether the version number of the receiver matches the given pattern string.

	 A Metacello version number is made up of version sequences delimited by the characters $. and $-.
	 The $. introduces a numeric version sequence and $- introduces an alphanumeric version sequence.
	 
	 A version pattern is made up of version pattern match sequences. also delimited by the characters $. 
	 and $-.. Each pattern match sequence is tested against the corresponding version sequence of the 
	 receiver, using the 'standard' pattern matching rules. All sequences must answer true for a match.
	
	 The special pattern sequence '?' is a match for the corresponding version sequence and all subsequent 
	 version sequences. '?' as the version pattern matches all versions. No more version pattern 
	 sequences are permitted once the '?' sequence is used. If used, it is the last version pattern
	 sequence. "
	
	| patternVersion mySize patternSize |
	patternVersion := aVersionPattern asRwGemStoneVersionNumber.
	mySize := self size.
	patternSize := patternVersion size.
	mySize = patternSize 
		ifFalse: [ 
			mySize < patternSize ifTrue: [ ^false ].
			(patternVersion at: patternSize) ~= '?' ifTrue: [ ^false ].
			mySize := patternSize ].
	1 to: mySize do: [:i | | pattern |
		pattern := (patternVersion at: i) asString.
		pattern = '?'
			ifTrue: [i = mySize ifFalse: [ ^self error: 'Invalid version match pattern: ', aVersionPattern printString ]]
			ifFalse: [ (pattern match: (self at: i) asString)  ifFalse: [ ^false ]]].
	^true
"
  '1.1.1' asRwGemStoneVersionNumber match: '*.*.*'. -> true
  '1.1.1' asRwGemStoneVersionNumber match: '*.#.*'. -> true
  '1.10.1' asRwGemStoneVersionNumber match: '*.#.*'. -> false
  '1.1.1' asRwGemStoneVersionNumber match: '*.*'. -> false
  '1.1.1' asRwGemStoneVersionNumber match: '*.?'. -> true
  '1.0' asRwGemStoneVersionNumber match: '1.?'. -> true
  '2.0' asRwGemStoneVersionNumber match: '1.?'. -> false
  '1.1.1' asRwGemStoneVersionNumber match: '?'. -> true
  '1' asRwGemStoneVersionNumber match: '*.?'. -> false
  '1-alpha5.0' asRwGemStoneVersionNumber match: '1-alpha*.?'. -> true
  '1-alpha15.0.1' asRwGemStoneVersionNumber match: '1-alpha*.?'. -> true
  '1.1' asRwGemStoneVersionNumber match: '?.?'. -> ERROR: invalid version match pattern
"

]

{ #category : 'printing' }
RwGemStoneVersionNumber >> printOn: aStream [

	| beforeFirst | 
	beforeFirst := true.
	self do:
		[:each |
		beforeFirst
			ifTrue: [beforeFirst := false]
			ifFalse: [
				each isString
					ifTrue: [ aStream nextPut: $- ]
					ifFalse: [ aStream nextPut: $. ] ].
		aStream nextPutAll: each asString ]

]

{ #category : 'accessing' }
RwGemStoneVersionNumber >> versionString [

	| strm |
	strm := WriteStream on: String new.
	self printOn: strm.
	^strm contents

]
