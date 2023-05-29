package com.logicaldoc.core.searchengine;

import com.logicaldoc.core.document.Document;

/**
 * Search result
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 5.2
 */
public class Hit extends Document implements Comparable<Hit> {

	private static final long serialVersionUID = 1L;

	private int score;

	private String content;

	private String summary;

	public int getScore() {
		return score;
	}

	public void setScore(int score) {
		this.score = score;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String content) {
		this.content = content;
	}

	public String getSummary() {
		return summary;
	}

	public void setSummary(String summary) {
		this.summary = summary;
	}

	@Override
	public int compareTo(Hit other) {
		try {
			if (other == null)
				return -1;

			if (this.equals(other))
				return 0;

			if (other.score == this.score) {
				if (this.getFileName() != null)
					return this.getFileName().compareToIgnoreCase(other.getFileName());
				else
					return 0;
			} else
				return -1 * (Integer.valueOf(this.score).compareTo(Integer.valueOf(other.score)));
		} catch (Exception t) {
			return 0;
		}
	}

}