exporting
exportSpecification

	| spec |
	spec := self asSpecification.
	spec exportToUrl: 'file:',  self specsRoot pathString, '/'