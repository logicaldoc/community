package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;

/**
 * The client side stub for the Settings Service. This service allows the
 * management of various application settings.
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
@RemoteServiceRelativePath("setting")
public interface SettingService extends RemoteService {

	/**
	 * Loads web services, webDav and other client-related settings
	 */
	public GUIParameter[] loadClientSettings() throws ServerException;

	/**
	 * Loads the complete settings set
	 */
	public GUIParameter[] loadSettings() throws ServerException;

	/**
	 * Loads a set of settings values
	 * 
	 * @param sid The current session identifier
	 * @param names The setting names to be retrieved
	 * @return The array of settings
	 * @throws ServerException
	 */
	public GUIParameter[] loadSettingsByNames(String[] names) throws ServerException;

	/**
	 * Saves settings
	 */
	public void saveRegistration(String name, String email, String organization, String website) throws ServerException;

	/**
	 * Saves settings
	 */
	public void saveSettings(GUIParameter[] settings) throws ServerException;

	/**
	 * Loads email settings (SMTP connection)
	 */
	public GUIEmailSettings loadEmailSettings() throws ServerException;

	/**
	 * Tests the SMTP connection
	 * 
	 * @param email email address to test(it will receive a test message)
	 * @return True only if the email was sent
	 */
	public boolean testEmail(String email) throws ServerException;

	/**
	 * Tests a storage (read/write access)
	 * 
	 * @param email email address to test(it will receive a test message)
	 * @return True only if the storage has read/write permission
	 */
	public boolean testStorage(int id) throws ServerException;

	/**
	 * Saves email settings (SMTP connection)
	 */
	public void saveEmailSettings(GUIEmailSettings settings) throws ServerException;

	/**
	 * Loads the OCR settings
	 */
	public GUIParameter[] loadOcrSettings() throws ServerException;

	/**
	 * Load the GUI settings
	 */
	public GUIParameter[] loadGUISettings() throws ServerException;

	/**
	 * Saves the dashlets configuration for the current user
	 */
	public void saveDashlets(GUIDashlet[] dashlets) throws ServerException;

	/**
	 * Loads the parameters of a specified converter
	 */
	public GUIParameter[] loadConverterParameters(String converter) throws ServerException;

	public static class Instance {
		private static SettingServiceAsync instance;

		public static SettingServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SettingService.class);
			}
			return instance;
		}
	}
}