Class {
	#name : 'RwDiskRepositorySpecification',
	#superclass : 'RwRepositorySpecification',
	#category : 'RowanV12Metadata-Core'
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
