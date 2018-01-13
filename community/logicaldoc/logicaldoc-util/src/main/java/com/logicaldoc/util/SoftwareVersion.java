package com.logicaldoc.util;

/**
 * Models a generic sofwtare version
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.8.1
 */
public class SoftwareVersion implements Comparable<SoftwareVersion> {

	private String version;

	public final String get() {
		return this.version;
	}

	public SoftwareVersion(String version) {
		if (version == null)
			throw new IllegalArgumentException("Version can not be null");
		if (!version.matches("[0-9]+(\\.[0-9]+)*"))
			throw new IllegalArgumentException("Invalid version format");
		this.version = version;
	}

	@Override
	public int compareTo(SoftwareVersion that) {
		if (that == null)
			return 1;
		String[] thisParts = this.get().split("\\.");
		String[] thatParts = that.get().split("\\.");
		int length = Math.max(thisParts.length, thatParts.length);
		for (int i = 0; i < length; i++) {
			int thisPart = i < thisParts.length ? Integer.parseInt(thisParts[i]) : 0;
			int thatPart = i < thatParts.length ? Integer.parseInt(thatParts[i]) : 0;
			if (thisPart < thatPart)
				return -1;
			if (thisPart > thatPart)
				return 1;
		}
		return 0;
	}

	@Override
	public boolean equals(Object that) {
		if (this == that)
			return true;
		if (that == null)
			return false;
		if (this.getClass() != that.getClass())
			return false;
		return this.compareTo((SoftwareVersion) that) == 0;
	}
}