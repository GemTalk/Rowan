Extension { #name : 'SequenceableCollection' }

{ #category : '*STON-GemStoneBase-Core' }
SequenceableCollection class >> new: newSize streamContents: blockWithArg [
  | stream |
  stream := WriteStreamPortable on: (self new: newSize).
  blockWithArg value: stream.
  ^ stream contents

]

{ #category : '*STON-GemStoneBase-Core' }
SequenceableCollection class >> streamContents: blockWithArg [
  ^ self new: 100 streamContents: blockWithArg

]
