private
_methodPrintString

	^ self className, (self isMeta ifTrue: [ ' class>>' ] ifFalse: [ '>>' ]), self methodDefinition selector