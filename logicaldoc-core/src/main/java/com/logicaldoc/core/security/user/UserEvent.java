package com.logicaldoc.core.security.user;

import java.util.Arrays;
import java.util.List;

import com.logicaldoc.core.folder.FolderEvent;

/**
 * Possible events in the user's history
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public enum UserEvent {

	LOGIN, LOGIN_FAILED, LOGOUT, TIMEOUT, PASSWORDCHANGED, TWOFACHANGED, DELETED, FILE_CONVERSION, MESSAGE_RECEIVED, CREATED, UPDATED, DISABLED, ENABLED, NEWAPIKEY, SEARCH, AI_QUERY, LEGAL_CONFIRMED;

	/**
	 * Gets the right enumeration entry from the corresponding resource bundle
	 * key
	 * 
	 * @param key the resource bundle key
	 * 
	 * @return The corresponding entry
	 */
	public static UserEvent fromKey(String key) {
		return UserEvent
				.valueOf(key.contains(".") ? key.replace("event.user.", "").toUpperCase().replace(".", "_") : key);
	}

	/**
	 * Converts the enumeration entry to the corresponding key in the resource
	 * bundle
	 * 
	 * @return The key
	 */
	public String toKey() {
		return "event.user." + name().toLowerCase().replace("_", ".");
	}

	/**
	 * Retrieves the all resource bundle keys
	 * 
	 * @return A list of keys
	 */
	public List<String> allKeys() {
		return Arrays.asList(FolderEvent.values()).stream().map(e -> e.toKey()).toList();
	}

	@Override
	public String toString() {
		return toKey();
	}
}