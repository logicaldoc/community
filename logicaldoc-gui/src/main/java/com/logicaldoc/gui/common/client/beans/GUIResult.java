package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Collects the results of a search and store some search statistics
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIResult implements Serializable {

	private static final long serialVersionUID = 1L;

	private long time;

	private long estimatedHits;

	private boolean hasMore = false;

	private List<GUIDocument> hits = new ArrayList<>();

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	public boolean isHasMore() {
		return hasMore;
	}

	public void setHasMore(boolean hasMore) {
		this.hasMore = hasMore;
	}

	public List<GUIDocument> getHits() {
		return hits;
	}

	public void setHits(List<GUIDocument> hits) {
		this.hits = hits;
	}

	public long getEstimatedHits() {
		return estimatedHits;
	}

	public void setEstimatedHits(long estimatedHits) {
		this.estimatedHits = estimatedHits;
	}
}