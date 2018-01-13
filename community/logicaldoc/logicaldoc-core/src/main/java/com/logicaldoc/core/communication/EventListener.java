package com.logicaldoc.core.communication;

import com.logicaldoc.core.document.AbstractHistory;

/**
 * A listener for the event emitted by the collector
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.1
 */
public interface EventListener {

	public void newEvent(AbstractHistory event);
}
