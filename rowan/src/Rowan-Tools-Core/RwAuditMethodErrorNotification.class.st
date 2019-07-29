Class {
	#name : 'RwAuditMethodErrorNotification',
	#superclass : 'RwAdoptAuditMethodErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'Handling' }
RwAuditMethodErrorNotification >> defaultAction [
	"record audit error"
	^ true
]
