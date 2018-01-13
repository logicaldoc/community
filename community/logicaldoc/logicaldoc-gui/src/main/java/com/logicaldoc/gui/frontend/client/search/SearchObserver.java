package com.logicaldoc.gui.frontend.client.search;

import com.logicaldoc.gui.common.client.beans.GUISearchOptions;

/**
 * Listener on search events
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public interface SearchObserver {

	/**
	 * Invoked when a new result is returned by the server.
	 */
	public void onSearchArrived();

	/**
	 * Invoked when the options changed
	 */
	public void onOptionsChanged(GUISearchOptions newOptions);
}
