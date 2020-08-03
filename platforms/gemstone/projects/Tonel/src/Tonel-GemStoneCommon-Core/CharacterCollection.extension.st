Extension { #name : 'CharacterCollection' }

{ #category : '*tonel-gemstonecommon-core' }
CharacterCollection >> join: aCollection [ 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
	^ self class new: (aCollection size * self size) streamContents: [:stream | 
			aCollection
				do: [:each | stream nextPutAll: each asString] 
				separatedBy: [stream nextPutAll: self]]

]

{ #category : '*tonel-gemstonecommon-core' }
CharacterCollection >> lineIndicesDo: aBlock [
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| cr lf start sz nextLF nextCR |
	start := 1.
	sz := self size.
	cr := Character cr.
	nextCR := self indexOf: cr startingAt: 1.
	lf := Character lf.
	nextLF := self indexOf: lf startingAt: 1.
	[ start <= sz ] whileTrue: [
		(nextLF = 0 and: [ nextCR = 0 ])
			ifTrue: [ "No more CR, nor LF, the string is over"
					aBlock value: start value: sz value: sz.
					^self ].
		(nextCR = 0 or: [ 0 < nextLF and: [ nextLF < nextCR ] ])
			ifTrue: [ "Found a LF"
					aBlock value: start value: nextLF - 1 value: nextLF.
					start := 1 + nextLF.
					nextLF := self indexOf: lf startingAt: start ]
			ifFalse: [ 1 + nextCR = nextLF
				ifTrue: [ "Found a CR-LF pair"
					aBlock value: start value: nextCR - 1 value: nextLF.
					start := 1 + nextLF.
					nextCR := self indexOf: cr startingAt: start.
					nextLF := self indexOf: lf startingAt: start ]
				ifFalse: [ "Found a CR"
					aBlock value: start value: nextCR - 1 value: nextCR.
					start := 1 + nextCR.
					nextCR := self indexOf: cr startingAt: start ]]]

]

{ #category : '*tonel-gemstonecommon-core' }
CharacterCollection >> withLineEndings: lineEndingString [
	| stream |
	stream := nil.
	self
		lineIndicesDo: [ :start :endWithoutDelimiters :end | 
			(stream isNil and: [ endWithoutDelimiters ~= end ])
				ifTrue: [ 
					((self copyFrom: endWithoutDelimiters + 1 to: end)
						_unicodeEqual: lineEndingString)
						ifFalse: [ 
							stream := WriteStreamPortable with: self copy.
							stream position: start - 1 ] ].
			stream
				ifNotNil: [ 
					stream next: endWithoutDelimiters - start + 1 putAll: self startingAt: start.
					endWithoutDelimiters = end
						ifFalse: [ stream nextPutAll: lineEndingString ] ] ].
	^ stream
		ifNil: [ self ]
		ifNotNil: [ 
			stream position = self size
				ifTrue: [ stream originalContents ]
				ifFalse: [ stream contents ] ]
]
