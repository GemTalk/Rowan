Class {
	#name : 'RwRowanSample10V2Test',
	#superclass : 'RwRowanSample10Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample10V2Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample10GitBranchDict'
]

{ #category : 'private' }
RwRowanSample10V2Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample10LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample10V2Test class >> _rowanSample10ProjectName [
	^ 'RowanSample10'
]
