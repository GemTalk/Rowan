Class {
	#name : 'RwRowanSample9V4Test',
	#superclass : 'RwRowanSample9V3Test',
	#category : 'Rowan-TestsV4'
}

{ #category : 'private' }
RwRowanSample9V4Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample9V4GitBranchDict'
]

{ #category : 'private' }
RwRowanSample9V4Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9V4LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample9V4Test class >> _rowanSample9ProjectName [
	^ 'RowanSample9V4'
]
