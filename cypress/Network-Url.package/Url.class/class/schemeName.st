"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: constants
"
schemeName
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ nil.
