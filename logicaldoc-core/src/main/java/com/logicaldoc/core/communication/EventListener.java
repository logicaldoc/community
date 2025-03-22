package com.logicaldoc.core.communication;

import com.logicaldoc.core.history.ExtendedHistory;

/**
 * A listener for the event emitted by the collector
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public interface EventListener {

	/**
	 * Called when a new event arrives
	 * 
	 * @param event the event to process
	 */
	public void newEvent(ExtendedHistory event);
}
