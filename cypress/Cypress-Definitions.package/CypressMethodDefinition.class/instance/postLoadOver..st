"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
