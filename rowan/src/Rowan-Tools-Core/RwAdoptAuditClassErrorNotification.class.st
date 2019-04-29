Class {
	#name : 'RwAdoptAuditClassErrorNotification',
	#superclass : 'RwAdoptAuditErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'Handling' }
RwAdoptAuditClassErrorNotification >> methodErrorDo: methodBlock classErrorDo: classBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	classBlock value
]
