package com.logicaldoc.core.document;

/**
 * This class is a TagCloud
 * 
 * @author Alessandro Gasparini - Logical Objects
 * @since 4.0
 */
public class TagCloud implements Comparable<TagCloud> {

	private String tag;

	private long count;

	private int scale;

	public TagCloud(String tag) {
		this.tag = tag;
	}

	public TagCloud(String tag, long count) {
		this.tag = tag;
		this.count = count;
	}

	public TagCloud() {
	}

	public String getTag() {
		return tag;
	}

	public void setTag(String tag) {
		this.tag = tag;
	}

	public long getCount() {
		return count;
	}

	public void setCount(long count) {
		this.count = count;
	}

	public int getScale() {
		return scale;
	}

	public void setScale(int scale) {
		this.scale = scale;
	}

	@Override
	public int compareTo(TagCloud o) {
		return getTag().compareTo(o.getTag());
	}
}