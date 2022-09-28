Class {
	#name : 'RwAdoptClassCategoryPackageConventionViolationErrorNotification',
	#superclass : 'RwAdoptAuditClassErrorNotification',
	#instVars : [
		'category',
		'packageConvention'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'instance creation' }
RwAdoptClassCategoryPackageConventionViolationErrorNotification class >> classNamed: className classCategory: aClassCategory packageConvention: packageConvention intoPackageNamed: packageName [
	^ self new
		className: className;
		category: aClassCategory;
		packageConvention: packageConvention;
		packageName: packageName;
		yourself
]

{ #category : 'private' }
RwAdoptClassCategoryPackageConventionViolationErrorNotification >> _errorMessage [

	^ 'Unable to adopt the class ', self className printString, ' into the package ', self packageName printString, ' as it''s class category ', self category printString, ' violates the ', self packageConvention, ' package convention .'
]

{ #category : 'accessing' }
RwAdoptClassCategoryPackageConventionViolationErrorNotification >> category [
	^category
]

{ #category : 'accessing' }
RwAdoptClassCategoryPackageConventionViolationErrorNotification >> category: object [
	category := object
]

{ #category : 'accessing' }
RwAdoptClassCategoryPackageConventionViolationErrorNotification >> packageConvention [
	^packageConvention
]

{ #category : 'accessing' }
RwAdoptClassCategoryPackageConventionViolationErrorNotification >> packageConvention: object [
	packageConvention := object
]
