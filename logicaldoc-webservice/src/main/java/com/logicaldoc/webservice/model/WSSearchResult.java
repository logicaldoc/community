package com.logicaldoc.webservice.model;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.util.SnippetStripper;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Represents a web service search result
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.2
 */
@XmlRootElement(name = "searchresult")
@XmlType(name = "WSSearchResult")
public class WSSearchResult {
	private long time = 0;

	private long estimatedHitsNumber = 0;

	private List<WSDocument> hits = new ArrayList<>();

	private int moreHits = 0;

	private int totalHits = 0;

	public int getTotalHits() {
		return totalHits;
	}

	public void setTotalHits(int totalHits) {
		this.totalHits = totalHits;
	}

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	public long getEstimatedHitsNumber() {
		return estimatedHitsNumber;
	}

	public void setEstimatedHitsNumber(long estimatedHitsNumber) {
		this.estimatedHitsNumber = estimatedHitsNumber;
	}

	public int getMoreHits() {
		return moreHits;
	}

	public void setMoreHits(int moreHits) {
		this.moreHits = moreHits;
	}

	public List<WSDocument> getHits() {
		return hits;
	}

	public void setHits(List<WSDocument> hits) {
		this.hits = hits;
		for (WSDocument hit : this.hits) {
			if (hit.getSummary() != null)
				hit.setSummary(SnippetStripper.strip(hit.getSummary()));
		}
	}
}