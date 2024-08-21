package com.logicaldoc.core.searchengine;

import java.util.Objects;

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
		if (this.equals(other))
			return 0;

		if (other.score == this.score) {
			if (Objects.toString(this.getFileName(), "").equalsIgnoreCase(Objects.toString(other.getFileName(), "")))
				return Long.compare(getId(), other.getId());
			else
				return Objects.toString(this.getFileName(), "")
						.compareToIgnoreCase(Objects.toString(other.getFileName(), ""));
		} else
			return Integer.compare(other.score, this.score);
	}

	@Override
	public int hashCode() {
		int result;
		result = getClass().getName().hashCode();
		result = 29 * result + Long.valueOf(getId()).hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;

		if (obj == null)
			return false;

		if (obj instanceof Hit other)
			return other.getId() == this.getId();
		else
			return false;
	}
}