"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testPatch

	| baseSnapshot targetSnapshot patch operations expected |
	baseSnapshot := CypressSnapshot definitions: self baseDefinitions.
	targetSnapshot := CypressSnapshot definitions: self targetDefinitions.
	patch := CypressPatch fromBase: baseSnapshot toTarget: targetSnapshot.
	operations := patch operations.
	expected := self baseTargetPatch asArray.
	self assert: operations size equals: expected size.
	operations do: [:each | self assert: (expected includes: each)]