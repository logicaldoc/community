package com.logicaldoc.gui.common.client.controllers;

import java.util.List;

import com.logicaldoc.gui.common.client.beans.GUIReadingRequest;

/**
 * Listener on documents events
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public interface ReadingRequestObserver {

	/**
	 * Invoked when the reading of a document has been confirmed
	 * 
	 * @param docId identifier of the document
	 */
	public void onConfirmReading(long docId);

	/**
	 * Notifies the arrival of new reading requests
	 * 
	 * @param readings The new readings
	 */
	public void onNewReadingRequests(List<GUIReadingRequest> readings);

}