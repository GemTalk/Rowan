Extension { #name : 'TonelWriter' }

{ #category : '*tonel-gemstonecommon-core' }
TonelWriter class >> lineEnding [
  "Answer the os-specific line endings"

  ^ String with: Character lf

]

{ #category : '*tonel-gemstonecommon-core' }
TonelWriter class >> orderedDictionaryClass [
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary

]
