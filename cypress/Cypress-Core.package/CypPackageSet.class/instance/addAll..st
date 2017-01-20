"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
addAll: somePackageRefs

	somePackageRefs
		do: [:packageRef | packages at: packageRef name put: packageRef].
	^somePackageRefs