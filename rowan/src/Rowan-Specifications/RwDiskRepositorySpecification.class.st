Class {
	#name : 'RwDiskRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#category : 'Rowan-Specifications'
}

{ #category : 'accessing' }
RwDiskRepositorySpecification >> committish [
  ^ ''

]

{ #category : 'copying' }
RwDiskRepositorySpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
]
