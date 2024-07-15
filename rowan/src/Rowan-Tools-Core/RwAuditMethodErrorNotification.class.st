Class {
	#name : 'RwAuditMethodErrorNotification',
	#superclass : 'RwAdoptAuditMethodErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwAuditMethodErrorNotification >> _errorMessage [
	| extensionMessage metaClassMessage typeMessage |
	metaClassMessage := self isMetaclass
		ifTrue: [ ' class << #' ]
		ifFalse: [ '  << #' ].
	extensionMessage := self isClassExtension
		ifTrue: [ ' extension ' ]
		ifFalse: [ ' ' ].
	typeMessage := self isTrait
		ifTrue: [ 'trait''s' ]
		ifFalse: [ 'class''s' ].
	^ self className , metaClassMessage , self selector asString
		, ' in the package ' , self packageName printString
		, ' is not present in the ' , typeMessage , ' method dictionary.'
]
