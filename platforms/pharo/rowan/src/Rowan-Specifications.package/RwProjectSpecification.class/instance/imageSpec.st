accessing
imageSpec

	^ imageSpec ifNil: [ imageSpec := RwSimpleProjectImageSpecification new ]
