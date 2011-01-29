object Suggester {
	//TODO: check builtins, too
	//TODO: obviously favourites should be more dynamic than this...
	// maybe start with file based, and move to an actual learning based one
	val favourites = List("vim", "scala", "ls", "cd", "cat")
	val len = (x: String, cmd: String) => if(x.length == cmd.length) 1 else 0
	val prefix = (x: String, cmd: String) => longestPrefix(x, cmd)
	val subseq = (x: String, cmd: String) => longestSubseq(x, cmd)
	val substr = (x: String, cmd: String) => longestSubstr(x, cmd)
	val dist = (x: String, cmd: String) => -distance(x, cmd)
	val favourite = (x: String, cmd: String) => if(favourites.contains(x)) 2 else 0

	val filters = List( len, prefix, subseq, substr, favourite, dist) 

	def score(cmd: String, x: String) = {
		var total = 0
		for(f <- filters)
			total = total + f(x, cmd)
		total
	}

	def findClose(cmd: String) = {
		val executables = for( dir <- Environment.pathDirs; 
			f <- dir.listFiles; 
			if f.canExecute && distance(f.getName, cmd) < 3
		) yield f
		if(executables.isEmpty)
			None
		else {
			var best = 0
			var suggest = executables.head
			for(ex <- executables) {
				var total = 0
				for(f <- filters)
					total = total + f(ex.getName, cmd)
				if(total > best) {
					best = total
					suggest = ex
				}
			}
			Some(suggest)
		}
	}

	def longestSubstr(x: String, y: String):Int = {
		var maxLen = 0
		val table = Array.ofDim[Int](x.length, y.length)

		for(i <- 0 until x.length; j <- 0 until y.length) {
			if(x(i) == y(j)) {
				if(i == 0 || j == 0)
					table(i)(j) = 1
				else
					table(i)(j) = table(i-1)(j-1) + 1
				maxLen = maxLen.max(table(i)(j))
			}
		}
		maxLen
	}

	def longestSubseq(x: String, y: String):Int = {
		if(x == "" || y == "")
			0
		else if(x.last == y.last) {
			longestSubseq(x.init, y.init) + 1
		} else {
			longestSubseq(x, y.init).max(longestSubseq(x.init, y))
		}
	}

	def longestPrefix(x: String, y: String) = {
		var pre = 0
		while(x.length > pre && y.length > pre && x(pre) == y(pre))
			pre = pre + 1
		pre
	}

//copied from: http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#Scala (and slightly fixed)
	def distance(str1: String, str2: String): Int = {
		val lenStr1 = str1.length
		val lenStr2 = str2.length
		val d: Array[Array[Int]] = Array.ofDim[Int](lenStr1 + 1, lenStr2 + 1)

		for (i <- 0 to lenStr1) d(i)(0) = i
		for (j <- 0 to lenStr2) d(0)(j) = j

		for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
			val cost = if (str1(i - 1) == str2(j-1)) 0 else 1

			d(i)(j) =
				(d(i-1)(j  ) + 1)     			// deletion
					.min( d(i  )(j-1) + 1)		// insertion
					.min( d(i-1)(j-1) + cost)	// substitution
		}
		return d(lenStr1)(lenStr2)
	}
}
