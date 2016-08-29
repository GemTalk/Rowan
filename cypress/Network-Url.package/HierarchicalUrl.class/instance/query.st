"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: access
"
query
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query 