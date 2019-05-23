private
_errorMessage

	| extensionMessage metaClassMessage |
	metaClassMessage := self isMetaclass
		ifTrue: [ ' class << #' ]
		ifFalse: [ '  << #' ].
	extensionMessage := self isClassExtension
		ifTrue: [ ' extension ' ]
		ifFalse: [ ' ' ].
	^ 'Unable to adopt the method ', self className, metaClassMessage, self selector asString, ' into the package ', self packageName printString, '. The method is not present in the class''s method dictionary.'