"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
basePackageName
  "package name may have a platform/branch extension, when comparing against category/protocol names, extension is ignored"

  | nm index |
  nm := self name.
  index := nm indexOfSubCollection: '.' startingAt: 1.
  index = 0
    ifTrue: [ ^ nm ].
  ^ nm copyFrom: 1 to: index - 1
