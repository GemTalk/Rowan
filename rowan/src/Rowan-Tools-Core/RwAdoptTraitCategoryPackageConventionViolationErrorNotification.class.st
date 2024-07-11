Class {
	#name : 'RwAdoptTraitCategoryPackageConventionViolationErrorNotification',
	#superclass : 'RwAdoptAuditTraitErrorNotification',
	#instVars : [
		'category',
		'packageConvention'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptTraitCategoryPackageConventionViolationErrorNotification class >> traitNamed: traitName traitCategory: aTraitCategory packageConvention: packageConvention intoPackageNamed: packageName [
	^ self new
		traitName: traitName;
		category: aTraitCategory;
		packageConvention: packageConvention;
		packageName: packageName;
		yourself
]

{ #category : 'private' }
RwAdoptTraitCategoryPackageConventionViolationErrorNotification >> _errorMessage [

	^ 'Unable to adopt the trait ', self traitName printString, ' into the package ', self packageName printString, ' as it''s class category ', self category printString, ' violates the ', self packageConvention, ' package convention .'
]

{ #category : 'accessing' }
RwAdoptTraitCategoryPackageConventionViolationErrorNotification >> category [
	^category
]

{ #category : 'accessing' }
RwAdoptTraitCategoryPackageConventionViolationErrorNotification >> category: object [
	category := object
]

{ #category : 'accessing' }
RwAdoptTraitCategoryPackageConventionViolationErrorNotification >> packageConvention [
	^packageConvention
]

{ #category : 'accessing' }
RwAdoptTraitCategoryPackageConventionViolationErrorNotification >> packageConvention: object [
	packageConvention := object
]
