#' chicane: Capture Hi-C Analysis Engine
#'
NULL

if(getRversion() >= "2.15.1") {

	utils::globalVariables(
		c(
			'target.chr',
			'target.start',
			'target.end',
			'bait.chr', 
			'bait.start',
			'bait.end',
			'bait.to.bait',
			'bait.id',
			'target.id',
			'distance',
			'count',
			'POtr',
			'pPOtr',
			'NBItr',
			'pNBItr',
			'p.value',
			'q.value',
			'value',
			'fragment.id',
			'trans.count',
			'expected',
			'group',
			'feature.count',
			'target.trans.count',
			'bait.trans.count',
			'temp.data'
			)
		);
}
