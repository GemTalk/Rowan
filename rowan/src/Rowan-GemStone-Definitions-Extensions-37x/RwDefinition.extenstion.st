Extension { #name : 'RwDefinition' }

{ #category : '*rowan-gemstone-definitions-extensions-37x' }
RwDefinition class >> diffString: aString to: bString [
  | fa fb cmd res |
  fa := FileReference newTempFilePrefix: 'STON' suffix: '.txt' .
  fb := FileReference newTempFilePrefix: 'STON' suffix: '.txt' .
  fa writeStream nextPutAll: aString ; flush ; close .
  fb writeStream nextPutAll: bString ; flush ; close .
  cmd := '/usr/bin/diff ' , fa asString,' ', fb asString .
  res := [ GsHostProcess  execute: cmd 
         ] on: ChildError do:[:ex | ex status == 1 ifTrue:[ ex resume ]. ex pass ].
  fa delete . fb delete .
  ^ res
]

{ #category : '*rowan-gemstone-definitions-extensions-37x' }
RwDefinition >> diffSTON: aRwDefinition [
  ^ self class diffString: (self printSTON ) 
                       to: (aRwDefinition printSTON )
]


