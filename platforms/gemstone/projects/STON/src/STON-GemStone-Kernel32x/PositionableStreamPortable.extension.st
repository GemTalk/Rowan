Extension { #name : 'PositionableStreamPortable' }

{ #category : '*ston-gemstone-kernel32x' }
PositionableStreamPortable >> beforeEnd [
"Returns true if the receiver can access more objects, false if not .
 GemStone extension. "

^position < readLimit
]
