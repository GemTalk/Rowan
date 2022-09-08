Class {
	#name : 'RwRowanSample9V2Test',
	#superclass : 'RwRowanSample9Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample9V2Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9V2Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9V2Test class >> _rowanSample9ProjectName [
	^ 'RowanSample9'
]
