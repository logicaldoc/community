package com.logicaldoc.util;

/**
 * A convenience class to represent name-value pairs.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 *
 * @param <K> the type of key
 * @param <V> the type of value
 */
public class Pair<K extends Object, V extends Object> {

	/**
	 * Key of this <code>Pair</code>.
	 */
	private K key;

	/**
	 * Value of this this <code>Pair</code>.
	 */
	private V value;

	/**
	 * Creates a new pair
	 */
	public Pair() {
	}

	/**
	 * Creates a new pair
	 * 
	 * @param key The key for this pair
	 * @param value The value to use for this pair
	 */
	public Pair(K key, V value) {
		this.key = key;
		this.value = value;
	}

	/**
	 * Gets the key for this pair.
	 * 
	 * @return key for this pair
	 */
	public K getKey() {
		return key;
	}

	/**
	 * Gets the value for this pair.
	 * 
	 * @return value for this pair
	 */
	public V getValue() {
		return value;
	}

	/**
	 * <p>
	 * <code>String</code> representation of this <code>Pair</code>.
	 * </p>
	 *
	 * <p>
	 * The default name/value delimiter '=' is always used.
	 * </p>
	 *
	 * @return <code>String</code> representation of this <code>Pair</code>
	 */
	@Override
	public String toString() {
		return key + "=" + value;
	}

	/**
	 * <p>
	 * Generate a hash code for this <code>Pair</code>.
	 * </p>
	 *
	 * <p>
	 * The hash code is calculated using both the name and the value of the
	 * <code>Pair</code>.
	 * </p>
	 *
	 * @return hash code for this <code>Pair</code>
	 */
	@Override
	public int hashCode() {
		int hash = 7;
		hash = 31 * hash + (key != null ? key.hashCode() : 0);
		hash = 31 * hash + (value != null ? value.hashCode() : 0);
		return hash;
	}

	/**
	 * <p>
	 * Test this <code>Pair</code> for equality with another
	 * <code>Object</code>.
	 * </p>
	 *
	 * <p>
	 * If the <code>Object</code> to be tested is not a <code>Pair</code> or is
	 * <code>null</code>, then this method returns <code>false</code>.
	 * </p>
	 *
	 * <p>
	 * Two <code>Pair</code>s are considered equal if and only if both the names
	 * and values are equal.
	 * </p>
	 *
	 * @param o the <code>Object</code> to test for equality with this
	 *        <code>Pair</code>
	 * @return <code>true</code> if the given <code>Object</code> is equal to
	 *         this <code>Pair</code> else <code>false</code>
	 */
	@Override
	public boolean equals(Object o) {
		if (this == o)
			return true;
		if (o instanceof Pair) {
			@SuppressWarnings("unchecked")
			Pair<K,V> pair = (Pair<K,V>) o;
			if (key != null ? !key.equals(pair.key) : pair.key != null)
				return false;
			if (value != null ? !value.equals(pair.value) : pair.value != null)
				return false;
			return true;
		}
		return false;
	}

	public void setKey(K key) {
		this.key = key;
	}

	public void setValue(V value) {
		this.value = value;
	}
}