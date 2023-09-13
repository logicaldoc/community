package com.logicaldoc.gui.common.client.controllers;

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
	 * 
	 * @param user the user that has been changed
	 */
	public void onUserChanged(GUIUser user);

	public void onUserLogin(String username);

	public void onUserLogout(String username);
}
