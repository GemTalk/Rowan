Class {
	#name : 'RwFiletreeUrl',
	#superclass : 'RwFileUrl',
	#category : 'Rowan-Tode'
}

{ #category : 'constants' }
RwFiletreeUrl class >> schemeName [
	^'filetree'

]

{ #category : 'printing' }
RwFiletreeUrl >> printOn: aStream [
  aStream
    nextPutAll: self schemeName;
    nextPut: $:.
  aStream nextPutAll: '//'.
  aStream nextPutAll: self pathString
]

{ #category : 'private-initialization' }
RwFiletreeUrl >> privateInitializeFromText: aString [
  "Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

  | schemeName pathString bare i |
  host := ''.
  bare := aString trimSeparators.
  schemeName := RwUrl schemeNameForString: bare.
  (schemeName == nil or: [ schemeName ~= self schemeName ])
    ifTrue: [ pathString := bare ]
    ifFalse: [ 
      "First remove schemeName and colon"
      bare := bare copyFrom: schemeName size + 2 to: bare size.
      (bare beginsWith: '//')
        ifTrue: [ pathString := bare copyFrom: 3 to: bare size ]
        ifFalse: [ pathString := bare ] ].
  self initializeFromPathString: pathString
]
