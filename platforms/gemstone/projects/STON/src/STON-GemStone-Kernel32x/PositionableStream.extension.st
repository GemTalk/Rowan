Extension { #name : 'PositionableStream' }

{ #category : '*ston-gemstone-kernel32x' }
PositionableStream >> beforeEnd [
"Returns true if the receiver can access more objects, false if not .
 GemStone extension. "

^position < readLimit
]
