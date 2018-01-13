package com.logicaldoc.gui.common.client;

import com.logicaldoc.gui.common.client.beans.GUIUser;

/**
 * A listener on relevant session events
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public interface SessionObserver {
	public void onUserLoggedIn(GUIUser user);
}