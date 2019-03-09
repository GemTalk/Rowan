Class {
	#name : 'RowanBrowserServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanBrowserServiceTest >> test_flipAutoCommit [
	| autoCommit service | 
	self jadeiteIssueTested: #issue396 withTitle: 'Ability to turn on autocommit would be nice'.
	autoCommit := RowanService autoCommit. 
	service := RowanBrowserService new. 
	[service flipAutoCommit. 
	self assert: RowanService autoCommit equals: autoCommit not. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit not.
	service flipAutoCommit. 
	self assert: RowanService autoCommit equals: autoCommit. 
	self assert: (SessionTemps current at: #'Jadeite_AutoCommit') equals: autoCommit] ensure: [
		RowanService setAutoCommit: autoCommit.
		self assert: RowanService autoCommit equals: autoCommit.]
]

{ #category : 'tests' }
RowanBrowserServiceTest >> test_windowsRegistry [

	| browserService object |
	self jadeiteIssueTested: #issue385 withTitle: 'Inspector should keep associated root object alive'.
	browserService := RowanBrowserService new. 
	object := Object new. 
	browserService saveRootObject: object asOop windowHandle: 123456. 
	self assert: (browserService openWindows at: 123456) equals: object.
	browserService releaseWindowHandle: 123456. 
	self assert: (browserService openWindows at: 123456 ifAbsent:['gone']) equals: 'gone'.
]
