running
assert: aString parsesToPackageName: packageName author: authorId branch: branchId version: versionNumber

	| queryReference |
	queryReference := CypressVersionReference name: aString.
	self
		assert: queryReference packageName equals: packageName;
		assert: queryReference author equals: authorId;
		assert: queryReference branch equals: branchId;
		assert: queryReference versionNumber equals: versionNumber.
