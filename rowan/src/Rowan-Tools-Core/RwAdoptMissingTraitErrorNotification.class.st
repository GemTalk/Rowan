Class {
	#name : 'RwAdoptMissingTraitErrorNotification',
	#superclass : 'RwAdoptAuditTraitErrorNotification',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwAdoptMissingTraitErrorNotification >> _errorMessage [

	^ 'Unable to adopt the trait ', self traitName printString, ' into the package ', self packageName printString, ' as it is not present in the current user''s symbol list.'
]
