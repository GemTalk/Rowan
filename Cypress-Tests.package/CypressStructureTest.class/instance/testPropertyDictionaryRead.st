tests
testPropertyDictionaryRead

	| propertyDictionary phoneNumbers |
	propertyDictionary := (self compileJSON: self sampleJson) asCypressPropertyObject.
	self assert: (propertyDictionary at: 'name') equals: 'John Smith'.
	self assert: (propertyDictionary at: 'age') equals: 25.
	self assert: (propertyDictionary at: 'registered') description: '"registered" property should have been true'.
	phoneNumbers := propertyDictionary at: 'phoneNumber'.
	self assert: phoneNumbers size equals: 2.
	self assert: ((phoneNumbers at: 1) at: 'number') equals: '212 555-1234'.
	self assert: ((phoneNumbers at: 2) at: 'number') equals: '646 555-4567'.
