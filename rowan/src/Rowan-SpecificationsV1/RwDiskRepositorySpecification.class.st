Class {
	#name : 'RwDiskRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#category : 'Rowan-SpecificationsV1'
}

{ #category : 'accessing' }
RwDiskRepositorySpecification >> committish [
  ^ ''

]

{ #category : 'accessing' }
RwDiskRepositorySpecification >> committishType [
  ^ ''
]

{ #category : 'copying' }
RwDiskRepositorySpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
]
