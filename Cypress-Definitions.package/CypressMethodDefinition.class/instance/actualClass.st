"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
actualClass

	| cls |
	cls := self theNonMetaClass.
	^self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls  ].
