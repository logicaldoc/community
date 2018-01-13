package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUICustomId;
import com.logicaldoc.gui.common.client.beans.GUISequence;

/**
 * The client side stub for the CustomIdService Service. This service gives all
 * needed methods to handle custom ids configutations.
 */
@RemoteServiceRelativePath("customid")
public interface CustomIdService extends RemoteService {
	/**
	 * Deletes a given configuration
	 */
	public void delete(long templateId, String type) throws ServerException;

	/**
	 * Creates or updates a configuration
	 */
	public void save(GUICustomId customid) throws ServerException;

	/**
	 * Loads a given configuration from the database
	 */
	public GUICustomId get(long templateId, String type) throws ServerException;

	/**
	 * Load all CustomIds rules
	 */
	public GUICustomId[] load() throws ServerException;

	/**
	 * Reset the numbering of a given sequence
	 */
	public void resetSequence(long sequenceId, long value) throws ServerException;

	/**
	 * Loads the list of sequences
	 */
	public GUISequence[] loadSequences() throws ServerException;

	/**
	 * Deletes the given sequence
	 */
	public void deleteSequence(long sequenceId) throws ServerException;
	
	public static class Instance {
		private static CustomIdServiceAsync instance;

		public static CustomIdServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(CustomIdService.class);
			}
			return instance;
		}
	}
}