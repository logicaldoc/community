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
	private V val;

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
	public Value(V val) {
		this.val = val;
	}

	/**
	 * Gets the value for this Value.
	 * 
	 * @return value for this Value
	 */
	public V getValue() {
		return val;
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
		return "" + val;
	}

	public void setValue(V val) {
		this.val = val;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((val == null) ? 0 : val.hashCode());
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
			if (val == null) {
				if (other.val != null)
					return false;
			} else if (!val.equals(other.val))
				return false;
		}
		return false;
	}
}