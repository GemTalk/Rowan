"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: Initializing - private
"
initialize

	super initialize.
	self
		repositories: IdentityDictionary new;
		digests: IdentityDictionary new.
