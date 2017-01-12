"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: enumeration
"
allClassesDo: aBlock
	"Iterate over all classes visible in the current session's transient symbol list, evaluating the block with each."

	GsCurrentSession currentSession symbolList do: 
			[:dict |
			dict do: 
					[:maybeClass |
					(maybeClass isBehavior and: [maybeClass isMeta not])
						ifTrue: [aBlock value: maybeClass]]]