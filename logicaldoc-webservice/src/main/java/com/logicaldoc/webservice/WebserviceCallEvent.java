package com.logicaldoc.webservice;

public enum WebserviceCallEvent {

	CALL;

	/**
	 * Gets the right enumeration entry from the corresponding resource bundle
	 * key
	 * 
	 * @param key the resource bundle key
	 * 
	 * @return The corresponding entry
	 */
	public static WebserviceCallEvent fromKey(String key) {
		return WebserviceCallEvent.valueOf(
				key.contains(".") ? key.replace("event.webservice.", "").toUpperCase().replace(".", "_") : key);
	}

	/**
	 * Converts the enumeration entry to the corresponding key in the resource
	 * bundle
	 * 
	 * @return The key
	 */
	public String toKey() {
		return "event.webservice." + name().toLowerCase().replace("_", ".");
	}

	@Override
	public String toString() {
		return toKey();
	}
}
