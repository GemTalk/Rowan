Class {
	#name : 'RowanBrowserServiceTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanBrowserServiceTest >> test_windowsRegistry [

	| browserService object |
	self jadeiteIssueTested: #issue385 withTitle: 'Inspector should keep associated root object alive'.
	browserService := RowanBrowserService new. 
	object := Object new. 
	browserService saveRootObject: object windowHandle: 123456. 
	self assert: (browserService openWindows at: 123456) equals: object.
	browserService releaseWindowHandle: 123456. 
	self assert: (browserService openWindows at: 123456 ifAbsent:['gone']) equals: 'gone'.
]