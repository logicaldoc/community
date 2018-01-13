package com.logicaldoc.gui.common.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.InvalidSessionException;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * Informations service
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0.0
 */
@RemoteServiceRelativePath("info")
public interface InfoService extends RemoteService {
	/**
	 * Retrieves the system informations
	 */
	public GUIInfo getInfo(String locale, String tenant);

	public GUIParameter[] getSessionInfo() throws InvalidSessionException;

	public static class Instance {
		private static InfoServiceAsync instance;

		public static InfoServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(InfoService.class);
			}
			return instance;
		}
	}
}