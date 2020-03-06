"
TonelExampleTest is an application used to verify that the tonel writer and reader for VA does work
"
Class {
	#name : 'TonelExampleTest',
	#superclass : 'TestCase',
	#instVars : [
		'instVar1',
		'instVar2',
		'instVar3'
	],
	#classVars : [
		'ClassVar1',
		'ClassVar2',
		'ClassVar3'
	],
	#pools : [
		'TonelExampleConstants'
	],
	#classInstVars : [
		'classInstVar1',
		'classInstVar2',
		'classInstVar3'
	],
	#category : 'TonelExampleApp'
}

{
	#category : 'Accessing'
}
TonelExampleTest class >> classInstVar1 [

	^ classInstVar1 ifNil: [ classInstVar1 := 'classInstVar1' ]
]

{
	#category : 'Accessing'
}
TonelExampleTest class >> classVar1 [

	^ ClassVar1 ifNil: [ ClassVar1 := 'ClassVar1' ]
]

{
	#category : 'Accessing',
	#vaCategories : ['Accessing','Another One','Private','Real Category'],
	#vaVisibility : 'private'
}
TonelExampleTest >> privateMethod [

	^'privateMethod'
]

{
	#category : 'Running'
}
TonelExampleTest >> setUp [
	instVar1 := 'instVar1'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testClassDefinedInSubApp [

	self assert: TonelExampleSubAppClass notCategorizedMethod equals: 'notCategorizedMethod'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testClassDefinedInSubSubApp [

	self assert: TonelExampleSubSubAppClass notCategorizedMethod2 equals: 'notCategorizedMethod2'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testClassInstVarAccess [

	self assert: self class classInstVar1 equals: 'classInstVar1'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testClassVarAccess [

	self assert: self class classVar1 equals: 'ClassVar1'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testExtendedMethodFromRootApp [

	self assert: self tonelExampleExtendedMethodFromRootApp equals: 'tonelExampleExtendedMethodFromRootApp'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testExtendedMethodFromSubApp [

	self assert: self tonelExampleExtendedMethodFromSubApp equals: 'tonelExampleExtendedMethodFromSubApp'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testExtendedMethodFromSubSubApp [

	self assert: self tonelExampleExtendedMethodFromSubSubApp equals: 'tonelExampleExtendedMethodFromSubSubApp'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testInstVarAccess [
	
	self assert: instVar1 equals: 'instVar1'
]

{
	#category : 'Testing'
}
TonelExampleTest >> testMethodWithPoolDictionaryUsage [

	^ Constant1
]

{
	#category : 'Testing'
}
TonelExampleTest >> testPrivateMethod [

	self assert: self privateMethod equals: 'privateMethod'
]
