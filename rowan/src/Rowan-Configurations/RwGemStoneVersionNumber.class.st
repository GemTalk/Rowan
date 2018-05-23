"
# GemStone version format

GemStone versions are simply an unbounded collection of $. separated positive integers.
"
Class {
	#name : 'RwGemStoneVersionNumber',
	#superclass : 'Magnitude',
	#type : 'variable',
	#category : 'Rowan-Configurations'
}

{ #category : 'instance creation' }
RwGemStoneVersionNumber class >> fromString: aString [

	| new components |
	components := OrderedCollection new.
	(aString substrings: '.') do: [:subString | 
		components add: subString asInteger].
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
			^(self at: count + 1) < (aRwGemStoneVersionNumber at: count + 1) ].
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

{ #category : 'matching' }
RwGemStoneVersionNumber >> rwPlatformAttributeMatchForGemStoneVersion: anRwGemStoneVersionConfigurationPlatformAttributeMatcher [

	^ anRwGemStoneVersionConfigurationPlatformAttributeMatcher matchVersion: self

]

{ #category : 'matching' }
RwGemStoneVersionNumber >> rwPlatformAttributeMatchForString: anRwStringConfigurationPlatformAttributeMatcher [

	^ anRwStringConfigurationPlatformAttributeMatcher matchString: self printString

]

{ #category : 'accessing' }
RwGemStoneVersionNumber >> versionString [

	| strm |
	strm := WriteStream on: String new.
	self printOn: strm.
	^strm contents

]
