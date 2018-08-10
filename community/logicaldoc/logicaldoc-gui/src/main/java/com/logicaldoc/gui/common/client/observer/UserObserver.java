package com.logicaldoc.gui.common.client.observer;

import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * Definition of a generic observer on user's attributes.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public interface UserObserver {
	/**
	 * Invoked when some changes on the user happens
	 */
	public void onUserChanged(GUIUser user);
}
