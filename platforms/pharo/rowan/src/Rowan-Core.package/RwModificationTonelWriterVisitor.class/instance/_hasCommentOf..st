class writing
_hasCommentOf: aClassDefinition 

	| comment |
	comment := aClassDefinition comment.
	^ comment notNil and: [ comment ~= '' ]