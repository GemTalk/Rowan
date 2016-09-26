"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: querying
"
resolveAllWith: aPackageManager
  "Answer a sorted collection of all resolved references within aGofer."

  ^ aPackageManager allResolvedPackageReferences
    select: [ :each | self matches: each ]