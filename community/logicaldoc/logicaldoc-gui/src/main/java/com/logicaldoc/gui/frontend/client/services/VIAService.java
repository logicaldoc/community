package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIVIASettings;

/**
 * The client side stub for the VIA Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.8
 */
@RemoteServiceRelativePath("via")
public interface VIAService extends RemoteService {

	/**
	 * Retrieves the settings
	 */
	public GUIVIASettings get() throws ServerException;

	/**
	 * Saves the settings
	 */
	public GUIVIASettings save(GUIVIASettings settings) throws ServerException;

	public static class Instance {
		private static VIAServiceAsync instance;

		public static VIAServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(VIAService.class);
			}
			return instance;
		}
	}
}