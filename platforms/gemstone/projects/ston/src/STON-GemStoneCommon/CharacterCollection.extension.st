Extension { #name : 'CharacterCollection' }

{ #category : '*ston-gemstonecommon-core' }
CharacterCollection >> stonContainSubObjects [
  ^ false

]

{ #category : '*ston-gemstonecommon-core' }
CharacterCollection >> stonOn: stonWriter [

        self isSymbol
                ifTrue: [stonWriter writeSymbol: self]
                ifFalse: [stonWriter writeString: self]

]
