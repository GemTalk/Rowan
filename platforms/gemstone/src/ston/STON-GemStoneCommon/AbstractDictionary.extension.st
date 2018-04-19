Extension { #name : 'AbstractDictionary' }

{ #category : '*ston-gemstonecommon-core' }
AbstractDictionary >> stonOn: stonWriter [
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
	self class == STON mapClass
		ifTrue: [ 
			stonWriter writeMap: self ]
		ifFalse: [ 
			stonWriter 
				writeObject: self 
				do: [ stonWriter encodeMap: self ] ]

]

{ #category : '*ston-gemstonecommon-core' }
AbstractDictionary >> stonProcessSubObjects: block [
	"Execute block to (potentially) change each of my subObjects.
	In general, all instance and indexable variables are processed.
	Overwrite when necessary. Not used when #stonContainSubObjects returns false."
	(self class isVariable and: [ self class isBytes not and: [self class isIndexable]])
		ifTrue: [
			1 to: self _basicSize do: [ :each | |val|			
									val:= (block value: (self basicAt: each)).
									self basicAt: each put: val ] ]"
							super stonProcessSubObjects: block"

]

{ #category : '*ston-gemstonecommon-core' }
AbstractDictionary class >> fromSton: stonReader [
	"Instances of STON mapClass will be read directly and won't arrive here.
	Other (sub)classes will use this method."
	
	| dictionary |
	dictionary := self new.
	stonReader parseMapDo: [ :key :value |
		dictionary at: key put: value ].
	^ dictionary

]
