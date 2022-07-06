package com.logicaldoc.gui.frontend.client.dashboard.chat;

import java.util.Date;

/**
 * Definition of a generic observer on chat's events.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0.1
 */
public interface ChatObserver {

	public void onMessage(long id, Date date, String username, String message);
}