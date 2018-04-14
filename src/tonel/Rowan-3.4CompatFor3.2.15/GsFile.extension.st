Extension { #name : 'GsFile' }

{ #category : '*rowan-3.4compatfor3.2.15' }
GsFile class >> _stat: aName  isLstat: aBoolean [ 

"uses the server file system access. 
 Returns a SmallInteger errno value if an error occurs or 
 or if aName is not a valid file or directory. Otherwise
 returns a new instance of GsFileStat. "
<primitive: 757>
aName _validateClass: String .
aBoolean _validateClass: Boolean .
self _primitiveFailed: #stat:isLstat: args: { aName . aBoolean }

]
