package com.logicaldoc.util.security;

/**
 * A bean to carry the pasword criteria
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.4
 */
public class PasswordCriteria {

	/**
	 * Minimum size of the password
	 */
	private int minLength = 8;

	/**
	 * Minimum number of upper case chars
	 */
	private int minUppercaseChars = 2;

	/**
	 * Minimum number of lower case chars
	 */
	private int minLowercaseChars = 2;

	/**
	 * Minimum number of digits
	 */
	private int minDigits = 1;

	/**
	 * Minimum number of special chars
	 */
	private int minSpecialChars = 1;

	/**
	 * Maximum size of a sequence
	 */
	private int maxSequenceSize = 3;

	/**
	 * Maximum number of occurrences of the same char
	 */
	private int maxOccurrences = 3;

	public PasswordCriteria(int minLength, int minUppercaseChars, int minLowercaseChars, int minDigits,
			int minSpecialChars) {
		super();
		this.minLength = minLength;
		this.minUppercaseChars = minUppercaseChars;
		this.minLowercaseChars = minLowercaseChars;
		this.minDigits = minDigits;
		this.minSpecialChars = minSpecialChars;
	}

	public PasswordCriteria() {
	}

	public int getMinLength() {
		return minLength;
	}

	public void setMinLength(int minLength) {
		this.minLength = minLength;
	}

	public int getMinUppercaseChars() {
		return minUppercaseChars;
	}

	public void setMinUppercaseChars(int minUppercaseChars) {
		this.minUppercaseChars = minUppercaseChars;
	}

	public int getMinLowercaseChars() {
		return minLowercaseChars;
	}

	public void setMinLowercaseChars(int minLowercaseChars) {
		this.minLowercaseChars = minLowercaseChars;
	}

	public int getMinDigits() {
		return minDigits;
	}

	public void setMinDigits(int minDigits) {
		this.minDigits = minDigits;
	}

	public int getMinSpecialChars() {
		return minSpecialChars;
	}

	public void setMinSpecialChars(int minSpecialChars) {
		this.minSpecialChars = minSpecialChars;
	}

	public int getMaxSequenceSize() {
		return maxSequenceSize;
	}

	public void setMaxSequenceSize(int maxSequenceSize) {
		this.maxSequenceSize = maxSequenceSize;
	}

	public int getMaxOccurrences() {
		return maxOccurrences;
	}

	public void setMaxOccurrences(int maxOccurrences) {
		this.maxOccurrences = maxOccurrences;
	}
}
