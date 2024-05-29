Class {
	#name : 'RwAuditMethodErrorNotification',
	#superclass : 'RwAdoptAuditMethodErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwAuditMethodErrorNotification >> _errorMessage [

	| extensionMessage metaClassMessage |
	metaClassMessage := self isMetaclass
		ifTrue: [ ' class << #' ]
		ifFalse: [ '  << #' ].
	extensionMessage := self isClassExtension
		ifTrue: [ ' extension ' ]
		ifFalse: [ ' ' ].
	^ 'Unable to adopt the method ', self className, metaClassMessage, self selector asString, ' into the package ', self packageName printString, '. The method is not present in the class''s method dictionary.'
]

{ #category : 'Handling' }
RwAuditMethodErrorNotification >> defaultAction [
	"record audit error"
	^ true
]
