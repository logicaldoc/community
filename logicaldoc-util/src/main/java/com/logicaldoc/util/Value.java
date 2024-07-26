package com.logicaldoc.util;

/**
 * A simple value holder.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9.4
 *
 * @param <V> the type of value
 */
public class Value<V extends Object> {

	/**
	 * Value of this this <code>Value</code>.
	 */
	private V value;

	/**
	 * Creates a new Value
	 */
	public Value() {
	}

	/**
	 * Creates a new Value
	 * 
	 * @param value The value to use for this pair
	 */
	public Value(V value) {
		this.value = value;
	}

	/**
	 * Gets the value for this Value.
	 * 
	 * @return value for this Value
	 */
	public V getValue() {
		return value;
	}

	/**
	 * <p>
	 * <code>String</code> representation of this <code>Value</code>.
	 * </p>
	 *
	 * @return <code>String</code> representation of this <code>Value</code>
	 */
	@Override
	public String toString() {
		return "" + value;
	}

	public void setValue(V value) {
		this.value = value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		if (obj instanceof Value other) {
			if (value == null) {
				if (other.value != null)
					return false;
			} else if (!value.equals(other.value))
				return false;
		}
		return false;
	}
}